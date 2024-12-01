import re

def binary_search(f, lo=0, hi=None):
    """
    Returns a value x such that f(x) is true.
    Based on the values of f at lo and hi.
    Assert that f(lo) != f(hi).
    """
    lo_bool = f(lo)
    if hi is None:
        offset = 1
        while f(lo+offset) == lo_bool:
            offset *= 2
        hi = lo + offset
    else:
        assert f(hi) != lo_bool
    best_so_far = lo if lo_bool else hi
    while lo <= hi:
        mid = (hi + lo) // 2
        result = f(mid)
        if result:
            best_so_far = mid
        if result == lo_bool:
            lo = mid + 1
        else:
            hi = mid - 1
    return best_so_far


inp = """
Immune System:
3916 units each with 3260 hit points with an attack that does 8 radiation damage at initiative 16
4737 units each with 2664 hit points (immune to radiation, cold, bludgeoning) with an attack that does 5 slashing damage at initiative 13
272 units each with 10137 hit points with an attack that does 331 slashing damage at initiative 10
92 units each with 2085 hit points (immune to fire) with an attack that does 223 bludgeoning damage at initiative 1
126 units each with 11001 hit points (immune to bludgeoning; weak to cold, fire) with an attack that does 717 bludgeoning damage at initiative 8
378 units each with 4669 hit points (immune to cold, slashing) with an attack that does 117 fire damage at initiative 17
4408 units each with 11172 hit points (immune to slashing; weak to bludgeoning) with an attack that does 21 bludgeoning damage at initiative 5
905 units each with 11617 hit points (weak to fire) with an attack that does 100 fire damage at initiative 20
3574 units each with 12385 hit points (weak to bludgeoning; immune to radiation) with an attack that does 27 radiation damage at initiative 19
8186 units each with 3139 hit points (immune to bludgeoning, fire) with an attack that does 3 bludgeoning damage at initiative 9

Infection:
273 units each with 26361 hit points (weak to slashing; immune to radiation) with an attack that does 172 radiation damage at initiative 18
536 units each with 44206 hit points (weak to fire, cold) with an attack that does 130 bludgeoning damage at initiative 12
1005 units each with 12555 hit points (immune to fire, radiation, bludgeoning) with an attack that does 24 radiation damage at initiative 6
2381 units each with 29521 hit points (immune to bludgeoning, radiation) with an attack that does 23 slashing damage at initiative 4
5162 units each with 54111 hit points (weak to radiation) with an attack that does 19 fire damage at initiative 2
469 units each with 45035 hit points (weak to fire, slashing) with an attack that does 163 radiation damage at initiative 15
281 units each with 23265 hit points (weak to slashing; immune to bludgeoning) with an attack that does 135 radiation damage at initiative 11
4350 units each with 46138 hit points (weak to fire) with an attack that does 18 bludgeoning damage at initiative 14
3139 units each with 48062 hit points (immune to bludgeoning, slashing, fire; weak to cold) with an attack that does 28 bludgeoning damage at initiative 3
9326 units each with 41181 hit points (weak to fire, bludgeoning) with an attack that does 8 cold damage at initiative 7
""".strip()

def doit(boost=0, part1=False):
    lines = inp.splitlines()
    immune, infection = inp.split("\n\n")

    teams = []

    REGEX = re.compile(r"(\d+) units each with (\d+) hit points (\([^)]*\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)")

    # namedtuple? who needs namedtuple with hacks like these?
    UNITS, HP, DAMAGE, DTYPE, FAST, IMMUNE, WEAK = range(7)

    blah = boost
    for inps in [immune, infection]:
        lines = inps.splitlines()[1:]
        team = []
        for line in lines:
            s = REGEX.match(line)
            units, hp, extra, damage, dtype, fast = s.groups()
            immune = []
            weak = []
            if extra:
                extra = extra.rstrip(" )").lstrip("(")
                for s in extra.split("; "):
                    if s.startswith("weak to "):
                        weak = s[len("weak to "):].split(", ")
                    elif s.startswith("immune to "):
                        immune = s[len("immune to "):].split(", ")
                    else:
                        assert False
            u = [int(units), int(hp), int(damage) + blah, dtype, int(fast), set(immune), set(weak)]
            team.append(u)
        teams.append(team)
        blah = 0
    
    def power(t):
        return t[UNITS] * t[DAMAGE]

    def damage(attacking, defending):
        mod = 1
        if attacking[DTYPE] in defending[IMMUNE]:
            mod = 0
        elif attacking[DTYPE] in defending[WEAK]:
            mod = 2
        return power(attacking) * mod
    
    def sort_key(attacking, defending):
        return (damage(attacking, defending), power(defending), defending[FAST])
    
    while all(not all(u[UNITS] <= 0 for u in team) for team in teams):
        teams[0].sort(key=power, reverse=True)
        teams[1].sort(key=power, reverse=True)

        targets = []

        # target selection
        for team_i in range(2):
            other_team_i = 1 - team_i
            team = teams[team_i]
            other_team = teams[other_team_i]

            remaining_targets = set(i for i in range(len(other_team)) if other_team[i][UNITS] > 0)
            my_targets = [None] * len(team)

            for i, t in enumerate(team):
                if not remaining_targets:
                    break
                best_target = max(remaining_targets, key= lambda i: sort_key(t, other_team[i]))
                enemy = other_team[best_target]
                if damage(t, enemy) == 0:
                    continue
                my_targets[i] = best_target
                remaining_targets.remove(best_target)
            targets.append(my_targets)
        
        # attacking
        attack_sequence = [(0, i) for i in range(len(teams[0]))] + [(1, i) for i in range(len(teams[1]))]
        attack_sequence.sort(key=lambda x: teams[x[0]][x[1]][FAST], reverse=True)
        did_damage = False
        for team_i, index in attack_sequence:
            to_attack = targets[team_i][index]
            if to_attack is None:
                continue
            me = teams[team_i][index]
            other = teams[1-team_i][to_attack]

            d = damage(me, other)
            d //= other[HP]

            if teams[1-team_i][to_attack][UNITS] > 0 and d > 0:
                did_damage = True

            teams[1-team_i][to_attack][UNITS] -= d
            teams[1-team_i][to_attack][UNITS] = max(teams[1-team_i][to_attack][UNITS], 0)
        if not did_damage:
            return None
    
    if part1:
        return sum(u[UNITS] for u in teams[0]) or sum(u[UNITS] for u in teams[1])
    asd = sum(u[UNITS] for u in teams[0])
    if asd == 0:
        return None
    else:
        return asd
print(doit(part1=True))
# I did a manual binary search, submitted the right answer, then added in did_damage.
# Turns out that doit can infinite loop without the did_damage check!
# WARNING: "doit" is not guaranteed to be monotonic! You should manually check values yourself.
# print(doit(33))
maybe = binary_search(doit)
print(doit(maybe))

