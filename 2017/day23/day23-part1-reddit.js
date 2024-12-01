function solve1(n) {
  const regex = /(\w+) ([\d|\w]+) (-?[\d|\w]+)/
  n = n.split('\n').map(l => regex.exec(l).slice(1, 4))

  let i = 0
  let regs = {
    a: 0,
    b: 0,
    c: 0,
    d: 0,
    e: 0,
    f: 0,
    g: 0,
    h: 0
  }

  const toVal = a => {
    const int = parseInt(a)
    if (isNaN(int)) return regs[a]
    return int
  }

  let loops = 0

  while (i < n.length) {
    switch (n[i][0]) {
      case 'set':
        regs[n[i][1]] = toVal(n[i][2])
        i++
        break
      case 'sub':
        regs[n[i][1]] -= toVal(n[i][2])
        i++
        break
      case 'mul':
        regs[n[i][1]] *= toVal(n[i][2])
        loops++
        i++
        break
      case 'jnz':
        if (toVal(n[i][1]) !== 0) {
          i += toVal(n[i][2])
        } else {
          i++
        }
        break
    }
  }
  console.log(regs);

  return loops
}

const code = "set b 99\n" +
  "set c b \n" +
  "jnz a 2 \n" +
  "jnz 1 5 \n" +
  "mul b 100 \n" +
  "sub b -100000\n" +
  "set c b \n" +
  "sub c -17000\n" +
  "set f 1 \n" +
  "set d 2 \n" +
  "set e 2 \n" +
  "set g d \n" +
  "mul g e \n" +
  "sub g b \n" +
  "jnz g 2 \n" +
  "set f 0 \n" +
  "sub e -1\n" +
  "set g e \n" +
  "sub g b \n" +
  "jnz g -8\n" +
  "sub d -1\n" +
  "set g d \n" +
  "sub g b \n" +
  "jnz g -13 \n" +
  "jnz f 2 \n" +
  "sub h -1\n" +
  "set g b \n" +
  "sub g c \n" +
  "jnz g 2 \n" +
  "jnz 1 3 \n" +
  "sub b -17 \n" +
  "jnz 1 -23";

console.log(solve1(code));

