using Revise

using SSA
using SymEngine

# prevoius way, using SymEngine
ssatape=SSATape()
setInputArgs(ssatape,[:x,:y,:z])
r=ssatape(Basic("x+y*2+z*x"))
ssatape(r,:r)

fun_interpret=SSAInterpreter(ssatape,[:r])
test_input=rand(3)
fun_interpret(test_input...)

func_compiled=SSACompiledFunc(ssatape,[:r])
func_compiled(test_input...)
mkdir("./gene")
saveSSAFunc(func_compiled,"./gene/test1")
func_compiled=loadSSAFunc("./gene/test1")

# now, we use the MathExpr

using MathExpr

engine=ExprEngine()
x,y,z=engine.([:x,:y,:z])
expr=x+y*2+z*x
r=ssatape(expr)
ssatape(r,:r)
ssatape(expr,term_map)
ssatape(:+,[ssatape(1.0),ssatape(2.0)])
ssatape.compute
term=expr.termpool.array[5]
ssatape.(expr.termpool.array)

term_map=ssatape(engine)

typeof(expr)

@time ssatape(expr)
@time ssatape(expr,term_map)
r=ssatape(expr)
ssatape(r,:r)
func_2=SSACompiledFunc(ssatape,[:r])
func_2(test_input...)

# we now test the diff, we try not use Basic in the new implementation
diffMap=initDefaultDiffDict(ssatape.(getInputArgs(ssatape)),ssatape)

ssaform=getfromA(ssatape.compute,ComputeNode(0))
arg=InputNode(0)
import SSA.evalDiff
import SSA.initDefaultDiffDict
ssatape.number
ssatape.compute
evalDiff(ssaform,arg,diffMap,ssatape)
evalDiff(InputNode(0),InputNode(0),diffMap,ssatape)
ssaform.op
terms=ssaform.args
evalDiffMul(terms,arg,diffMap,ssatape)
ssatape.compute

evalDiff(ssatape.(getInputArgs(ssatape)),diffMap,ssatape)

addToOutput([:r],[:x,:y,:z],diffMap,ssatape)
output_diff,output_syms=diffTape([:r],[:x,:y,:z],diffMap,ssatape)

targets=[:r]
target=:r
args=[:x,:y,:z]
