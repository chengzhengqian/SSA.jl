# a simplified version of SSA, non condition right now
# we should move this part to an independent module
# we move this from 
using Base.Meta
using SymEngine
using MathExpr

export Node, InputNode, NumberNode, ComputeNode, BDict, getfromA, getfromB,hasA, hasB, add,SSAForm, SSATape, evalTape, getInputArgs, setInputArgs,addTape

abstract type Node end

struct InputNode <: Node
    idx::Int
end

const ReservedInputNodeSymbol="I"
"""
input node
"""
function Base.show(io::IO,input::InputNode)
    print(io,"$(ReservedInputNodeSymbol)$(input.idx)")
end

# struct OutputNode <: Node
#     idx::Int
# end

# const ReservedOutputNodeSymbol="O"
# """
# output node
# """
# function Base.show(io::IO,output::OutputNode)
#     print(io,"$(ReservedOutputNodeSymbol)$(output.idx)")
# end

struct ComputeNode <: Node
    idx::Int
end

const ReservedComputeNodeSymbol="C"
"""
output node
"""
function Base.show(io::IO,compute::ComputeNode)
    print(io,"$(ReservedComputeNodeSymbol)$(compute.idx)")
end


struct NumberNode <: Node
    idx::Int
end

const ReservedNumberNodeSymbol="N"
"""
output node
"""
function Base.show(io::IO,number::NumberNode)
    print(io,"$(ReservedNumberNodeSymbol)$(number.idx)")
end

struct BDict{A,B}
    a_to_b::Dict{A,B}
    b_to_a::Dict{B,A}
end

function BDict{A,B}() where {A,B}
    BDict{A,B}(Dict{A,B}(),Dict{B,A}())
end

function Base.length(bdict::BDict{A,B}) where {A,B}
    length(bdict.a_to_b)
end


function Base.show(io::IO,bdict::BDict{A,B}) where {A,B}
    print(io,"$(A)<=>$(B):$(bdict.a_to_b)")
end


function getfromA(bdict::BDict{A,B},a::A) where {A,B}
    bdict.a_to_b[a]
end

function getfromB(bdict::BDict{A,B},b::B) where {A,B}
    bdict.b_to_a[b]
end

function hasA(bdict::BDict{A,B},a::A) where {A,B}
    haskey(bdict.a_to_b,a)
end

function hasB(bdict::BDict{A,B},b::B) where {A,B}
    haskey(bdict.b_to_a,b)
end

"""
the BDict should strictly be an one-to-one map
"""
function add(bdict::BDict{A,B},m::Pair{A,B}) where {A,B}
    a,b=m
    if(hasA(bdict,a) || hasB(bdict,b))
        error("the BDict should be an one-to-one map ")
    end
    bdict.a_to_b[a]=b
    bdict.b_to_a[b]=a
end

"""
just the operation plus the arguments
"""
struct SSAForm
    op::Symbol
    args::Array{Node,1}
end

function Base.show(io::IO,ssaform::SSAForm)
    print(io,"($(join(ssaform.args,ssaform.op)))")
end

function Base.:(==)(ssa1::SSAForm,ssa2::SSAForm)
    (ssa1.op==ssa2.op)&&(ssa1.args==ssa2.args)
end

function Base.isequal(ssa1::SSAForm,ssa2::SSAForm)
    (ssa1.op==ssa2.op)&&(ssa1.args==ssa2.args)
end

"""
this is necessary to identify the same ssa in the Dict.
"""
function Base.hash(ssa::SSAForm)
    hash([ssa.op,ssa.args...])
end


"""
We use BDict to store the information
"""
struct SSATape    
    input::BDict{InputNode,Symbol} #  track the order of the input args
    compute::BDict{ComputeNode,SSAForm}
    # we don't need to define the OutputNode, as we just need to record the correpsonding ComputeNode for a given output symbol
    output::Dict{Symbol,Node} # each output node strictly correpodes a compute node, but a compute node may correspond zero or multiple output node (or maybe just input node, though that does not make too much sense here)
    # output_name::BDict{OutputNode,Symbol} #  track the order of the output args
    number::BDict{NumberNode,Float64}     #  we assume all number are Float64
end

function SSATape()
    SSATape(BDict{InputNode,Symbol}(),
            BDict{ComputeNode,SSAForm}(),
            Dict{Symbol,Node}(),
            BDict{NumberNode,Float64}())
end

"""
focus to printing the key information. Then input args (with given order) and output results (no default order)
"""
function Base.show(io::IO, ssatape::SSATape)
    print(io,"SSATape($(getInputArgs(ssatape)) => $(keys(ssatape.output)), $(length(ssatape.compute)) Compute Steps, $(length(ssatape.number)) Number Constants)")
end


function getInputArgs(ssatape::SSATape)
    [getfromA(ssatape.input,InputNode(i-1)) for i in 1:length(ssatape.input)]
end

"""
to specify the order the input, it is usual to initalize the input
# we update the API that one should only use addTape to add new symbols
"""
function setInputArgs(ssatape::SSATape,input_args::Array{Symbol,1})
    [ addTape(arg_,ssatape) for arg_ in input_args]
end

  

"""
return the InputNode for the symbol.
If the symbol does not exists, create a new one
(This is dangerous, as one may forget the specify the order of input.)
So now, it an symbol does nto exists, throw an error
And add a function to add symtal
We follow the convection that Node idx start from 0;
This means that when we generate the assembly code, we can directly use this index. (though this is not consistent with the julia's default convention)
As we going to use SymEngine to perform some simplification. So if an symbol start with I, OR C, indicates that it is an input nodes or compute nodes.
We assume that if it start with I,C,O, the index makes sense, and we does not check right now.
"""
function evalTape(sym::Symbol,tape::SSATape)
    sym_str="$(sym)"
    if(startswith(sym_str,ReservedInputNodeSymbol))
        return InputNode(parse(Int,sym_str[2:end]))
    # elseif(startswith(sym_str,ReservedOutputNodeSymbol))
    #     return OutputNode(parse(Int,sym_str[2:end]))
    elseif(startswith(sym_str,ReservedComputeNodeSymbol))
        return ComputeNode(parse(Int,sym_str[2:end]))
    elseif(startswith(sym_str,ReservedNumberNodeSymbol))
        return NumberNode(parse(Int,sym_str[2:end]))
    else
        input=tape.input
        if(!hasB(input,sym))
            # newNode=InputNode(length(input))
            # add(input,newNode=>sym)
            error("use addTape to add $(sym)!")
        end    
        return getfromB(input,sym)
    end
end

function addTape(sym::Symbol,tape::SSATape)
    input=tape.input
    if(hasB(input,sym))
        # newNode=InputNode(length(input))
        # add(input,newNode=>sym)
        error("$(sym) has been added!")
    end    
    newNode=InputNode(length(input))
    add(input,newNode=>sym)
end

function evalTape(num::Number,tape::SSATape)
    number=tape.number    
    num=convert(Float64,num)
    if(!hasB(number,num))
        newNode=NumberNode(length(number))
        add(number,newNode=>num)
    end
    return getfromB(number,num)
end

"""
we make a convenient function to this one see below
"""
function evalTape(ssaform::SSAForm,tape::SSATape)
    compute=tape.compute
    if(!hasB(compute,ssaform))
        newNode=ComputeNode(length(compute))
        add(compute,newNode=>ssaform)
    end
    return getfromB(compute,ssaform)
end

"""
convert to SSAForm
"""
function evalTape(op::Symbol,args::Array,ssatape::SSATape)
    ssatape(SSAForm(op,args))
end


"""
for the MathExpr library, to evaluate the single term.
For better performance, we could store all the result for the termpool
"""
function evalTape(term::Term, ssatape::SSATape)
    n_args=length(term.value)
    if(n_args>=2)
        args=ssatape.(term.sympool.(term.value))
        return ssatape(SSAForm(:*,args))
    elseif(n_args==1)
        return ssatape(term.sympool(term.value[1]))
    else
        return ssatape(1.0)
    end    
end

function evalTape(term::Term, term_map::Dict,ssatape::SSATape)
    if(!haskey(term_map,term))
        term_map[term]=ssatape(term)
    end
    term_map[term]
end

# now, we update the API, so we could use an empty term_map. We remove this function now.
# """
# we store all values of terms in a dictionary
# The return type is different from other calls, as we return a dictionary instead of a node
# """
# function evalTape(engine::ExprEngine,ssatape::SSATape)
#     Dict([term=>ssatape(term)  for term in engine.termpool.array]...)
# end

"""
c*n,
check some special values for c
c=1.0
"""
function evalTape(c::Number,n::Node,ssatape::SSATape)
    if(c==1.0)
        return n
    elseif(c==0.0)
        return ssatape(c)
    else
        return ssatape(SSAForm(:*,[ssatape(c),n]))
    end    
end


"""
for performance, we should first evaluate all terms in engine, see previous function, then evaluate individual term;
This is not good, as we force to evaulate the term to term_map.
Here, we update fucntion so that when the term is not in term_map, we evalaute them, and update to term map
term_map=Dict{Term,Node}()
"""
function evalTape(expr::SymExpr, term_map::Dict, ssatape::SSATape)
    args=[ssatape(v,ssatape(expr.termpool(k),term_map)) for (k,v) in expr.coefficient]
    n_args=length(args)
    if(n_args==0)
        ssatape(0.0)
    elseif(n_args==1)
        args[1]
    else
        ssatape(SSAForm(:+,args))
    end    
end

"""
this one directly evaluate the expr, without the term_map. When we fully evaluate the terms in termpool, this two method should yield the same result
We should check whether the number of args is 0,1 >2
"""
function evalTape(expr::SymExpr, ssatape::SSATape)
    args=[ssatape(v,ssatape(expr.termpool(k))) for (k,v) in expr.coefficient]
    n_args=length(args)
    if(n_args==0)
        ssatape(0.0)
    elseif(n_args==1)
        args[1]
    else
        ssatape(SSAForm(:+,args))
    end    
end


function evalTape(expr::Expr,tape::SSATape)
    if(expr.head==:call)
        # we need to treat power with special care, right now, we just treat the remaining case
        # now, we add support to - and ^
        op=expr.args[1]
        args=expr.args[2:end]
        # treat the power operator
        if(op==(:^))
            if(length(args)==2)
                op=:*
                # this is not the most efficient way, but for most case, the exponent should be very small, like 2.
                args=[args[1] for _ in 1:convert(Int,args[2])]
            else
                error("power operator expected two arguments but get $(length(args))")
            end            
        end
        if(op==(:-))
            if(length(args)==1)
                op=:*
                args=[-1,args[1]]
            end            
        end        
        # treat the subtraction
        args=[evalTape(arg_,tape) for arg_ in args]
        return evalTape(SSAForm(op,args),tape)
    else
        error("unexpected Expr $(expr)")
    end    
end

# finally, we need to assign a compute Node to output and with a name
"""
node is assumed to be ComputeNode, but could also be InputNode
"""
function evalTape(node::Node, sym::Symbol,tape::SSATape)
    output=tape.output
    tape.output[sym]=node
end

# we also need to evaluate the Basic form
function evalTape(expr::Basic,tape::SSATape)
    evalTape(Meta.parse("$(expr)"),tape)
end



"""
redirect to evalTape
"""
function (tape::SSATape)(para...)
    evalTape(para...,tape)
end


"""
convert a node to Basic for further computation
Notice the numbernode is returned as the given number.
# We check wether the number is a interger or not, as returning a integer like .1.0 will remove some trivial term.
isinteger(1.0)
"""
function Base.convert(::Type{Basic},node::Node,tape::SSATape)
    if(node isa NumberNode)
        value=getfromA(tape.number,node)
        if(isinteger(value))
            return Basic(convert(Int,value))
        end        
        Basic(value)
    else
        Basic("$(node)")
    end
end

