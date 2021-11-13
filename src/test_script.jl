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
