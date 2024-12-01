import hashlib
code = "gdjjyniy"
stack = [(code, (0,0))]
possible = {'b','c','d','e','f'}
maximum = 0

solution = ''

while stack:
    codea,p = stack.pop(0)
    print(codea)
    x,y = p
    m = hashlib.md5()
    m.update((codea).encode('utf-8'))
    a_code = m.hexdigest()

    if(x == 3 and y == 3):
        solution = codea
        continue

    else:   
        np = (x, y - 1)
        if(np[1] >= 0):
            if(a_code[0] in possible):
                stack.append((codea + 'U', np))

        np = (x, y + 1)
        if(np[1] <= 3):
            if(a_code[1] in possible):
                stack.append((codea + 'D', np))

        np = (x - 1, y)
        if(np[0] >= 0):
            if(a_code[2] in possible):
                stack.append((codea + 'L', np))

        np = (x + 1, y)
        if(np[0] <= 3):
            if(a_code[3] in possible):
                stack.append((codea + 'R', np))
print(len(solution) - len(code) )
