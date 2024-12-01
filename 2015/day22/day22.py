from dataclasses import dataclass

# from aocd.models import Puzzle


class Fight:
    costs = {
        "Magic Missile": 53,
        "Drain": 73,
        "Shield": 113,
        "Poison": 173,
        "Recharge": 229,
    }
    costs_inv = {v: k for k, v in costs.items()}

    def __init__(self, actions, boss, player, hard, action_to_add, effects):
        self.b_hp, self.b_dmg = boss
        self.p_hp, self.p_mana = player
        self.p_armor = 0
        self.actions = actions
        self.hard = hard
        self.action_to_add = action_to_add
        self.effects = effects

    def __str__(self):
        return f"""
        Boss HP: {self.b_hp}
        Boss Damage: {self.b_dmg}
        Player HP: {self.p_hp}
        Player Mana: {self.p_mana}
        Player Armor: {self.p_armor}
        Actions: {', '.join(self.actions)}
        Hard Mode: {self.hard}
        Next Action: {self.action_to_add}
        Effects: {', '.join(self.effects.keys())}
        """

    def do_fight(self):
        def apply_effects():
            todelete = []
            for effect in self.effects:
                match effect:
                    case "Shield":
                        self.p_armor += 7
                    case "Poison":
                        self.b_hp -= 3
                    case "Recharge":
                        self.p_mana += 101
                self.effects[effect] -= 1
                if self.effects[effect] == 0:
                    todelete.append(effect)
            for effect in todelete:
                del self.effects[effect]

        self.p_armor = 0

        if self.hard:
            self.p_hp -= 1
            if self.p_hp <= 0:
                return "boss"

        apply_effects()
        self.actions.append(self.action_to_add)
        match self.action_to_add:
            case "Magic Missile":
                if self.p_mana < 53:
                    return "Invalid"
                self.b_hp -= 4
                self.p_mana -= 53
            case "Drain":
                if self.p_mana < 73:
                    return "Invalid"
                self.b_hp -= 2
                self.p_hp += 2
                self.p_mana -= 73
            case "Shield":
                if "Shield" in self.effects or self.p_mana < 113:
                    return "Invalid"
                self.effects["Shield"] = 6
                self.p_mana -= 113
            case "Poison":
                if "Poison" in self.effects or self.p_mana < 173:
                    return "Invalid"
                self.effects["Poison"] = 6
                self.p_mana -= 173
            case "Recharge":
                if "Recharge" in self.effects or self.p_mana < 229:
                    return "Invalid"
                self.effects["Recharge"] = 5
                self.p_mana -= 229

        self.p_armor = 0
        apply_effects()

        if self.b_hp <= 0:
            return "player"

        self.p_hp -= max(1, self.b_dmg - self.p_armor)

        if self.p_hp <= 0:
            return "boss"

    @property
    def spent_mana(self):
        total = sum([Fight.costs[action] for action in self.actions])
        return total

    def get_new_fights(self, max_add):
        return [
            Fight(
                self.actions.copy(),
                (self.b_hp, self.b_dmg),
                (self.p_hp, self.p_mana),
                self.hard,
                Fight.costs_inv[a],
                self.effects.copy(),
            )
            for a in Fight.costs_inv
            if a < max_add
        ]


def generate_actions(boss, hard=False):
    player = (50, 500)
    initial = Fight([], boss, player, hard, None, {})
    fights = initial.get_new_fights(999)
    min_cost = None
    while fights:
        fight = fights.pop()
        winner = fight.do_fight()
        if winner is None:
            fights += fight.get_new_fights(
                999 if min_cost is None else min_cost - fight.spent_mana
            )
        elif winner == "player" and (min_cost is None or min_cost > fight.spent_mana):
            min_cost = fight.spent_mana
            print(f"winner: {winner}, min_cost: {min_cost}, actions: {fight.actions}")

    return min_cost


def solve(data):
    lines = data.splitlines()
    boss = {}
    for line in lines:
        stat, val = line.split(": ")
        boss[stat] = int(val)

    boss_stats = (boss["Hit Points"], boss["Damage"])

    parta = generate_actions(boss_stats)
    partb = generate_actions(boss_stats, hard=True)

    return str(parta), str(partb)


if __name__ == "__main__":
    print("part a: {}\npart b: {}".format(*solve("Hit Points: 51\nDamage: 9")))
#    p = Puzzle(year=2015, day=22)
#    print("part a: {}\npart b: {}".format(*solve(p.input_data)))
