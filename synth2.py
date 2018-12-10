
from z3 import *

m = Solver()

N = 5
M = 4
linenos = [1,2,3,4,5] #Length must be N

init_regs = [Int('R' + str(i)) for i in range(M)]

#reg_inds = [[Int('ra' + str(j)), Int('rb' + str(j))] for j in range(N)]
reg_inds = [[3,3], [1,3], [2,3], [0,0], [3,0]]
bits = ['0' for x in range(N)]
all_regs = []
path = []

postval = init_regs[1] + init_regs[2]
path_constr = []
while(True):
    semantics = []
    assumptions = []
    line = 0
    line_old = -1
    regs = [[Int('r' + str(i) + str(j) + '_' + ''.join(bits)) 
            for i in range(M)] for j in range(N+1)]
    semantics.append(And([init_regs[i] == regs[0][i] for i in range(M)]))

    while line < N:
        for j in range(0, M):
            if(j != reg_inds[line][1]):
                semantics.append(regs[line+1][j] == regs[line][j])
            else:
                semantics.append(regs[line+1][j] == regs[line][j] -
                        regs[line][reg_inds[line][0]])
        line_old = line
        if bits[line] == '0':
            assumptions.append(regs[line+1][reg_inds[line][1]] > 0)
            line = line+1
        else:
            assumptions.append(regs[line+1][reg_inds[line][1]] <= 0)
            line = linenos[line]
     
    path_cond = And(assumptions)
    path_constr.append(Implies(And(And(semantics), path_cond), regs[line_old+1][0] ==
        postval))
    if(line != N):
        path.append(path_cond)

    all_regs += [r for R in regs for r in R]
    
    idx = len(bits)-1
    while idx >= 0 and bits[idx] == '1':
        idx -= 1
    if idx >= 0:
        bits[idx] = '1'
        idx += 1
        while idx < len(bits):
            bits[idx] = '0'
            idx += 1
    else:
        break
    print(bits)

#Specification
#Constr.append(ans == (init_regs[0] - init_regs[1]))
#Constr.append(path == True)
m.add(ForAll(init_regs + all_regs, And(path_constr)))
m.check()
