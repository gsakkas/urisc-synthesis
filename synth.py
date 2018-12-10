
from z3 import *

m = Solver()

N = 5
M = 4
linenos = [1,2,3,4,5] #Length must be N

init_regs = [Int('R' + str(i)) for i in range(M)]

reg_inds = [[Int('ra' + str(j)), Int('rb' + str(j))] for j in range(N)]
bits = ['0' for x in range(N)]

all_regs = []
path = []

post_val = init_regs[1] + init_regs[2]
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
            c = regs[line+1][j] == (regs[line][j] - regs[line][0])
            for k in range(1, M):
                c = If(reg_inds[line][0] == k, regs[line+1][j] == (regs[line][j] -
                        regs[line][k]), c)
            semantics.append(If(reg_inds[line][1] != j, (regs[line+1][j] ==
                regs[line][j]), c))
        line_old = line
        if bits[line] == '0':
            c = []
            for k in range(0, M):
                c.append(Implies(reg_inds[line][1] == k, regs[line+1][k] > 0))
            assumptions.append(And(c))
            line = line+1
        else:
            c = []
            for k in range(0, M):
                c.append(Implies(reg_inds[line][1] == k, regs[line+1][k] <= 0))
            assumptions.append(And(c))
            line = linenos[line]
     
    path_cond = And(assumptions)
    path_constr.append(Implies(And(And(semantics), path_cond),
        regs[line_old+1][0] == post_val))
    if(line == N):
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

path = Or(path)
#Specification

ris = [r for R in reg_inds for r in R]
m.add(ForAll(init_regs + all_regs + ris, And(path_constr)))

m.check()
