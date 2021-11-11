# to bench mark the efficiency of
# we can directly generate the Expr on the fly
# this is useful to test the AD code
using Base.Meta
expr=Meta.parse("(x1::Float64,x2::Float64)->(y1=x1+x2;y2=x1*x2;[y1,y2])")

dump(expr)
# func=eval(expr)
# @time func(1.0,2.0)
Expr(:call,:+,1,2)
Expr(:tuple,1,2,3)
Expr(Symbol("::"),1,2)
