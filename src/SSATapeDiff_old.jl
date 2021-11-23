# add forward for SSATape
export Derivative, evalDiff, initDefaultDiffDict, addToOutput, defaultDerivatives
# we first define a structure to represent derivatives
# right now, we only consider first order
struct Derivative
    target::Node
    arg::Node
end

function Base.show(io::IO,diff::Derivative)
    print(io,"∂$(diff.target)/∂$(diff.arg)")
end

function evalDiff(ssaform::SSAForm,arg::Node,dict::Dict{Derivative,Node},ssatape::SSATape)
    expr=Basic("$(ssaform)")
    result=Basic(0)
    for v in free_symbols(expr)
        v_=evalTape(Symbol("$(v)"),ssatape)
        result+=diff(expr,v)*convert(Basic,evalDiff(v_,arg,dict,ssatape),ssatape)
    end
    evalTape(result,ssatape)
end



function evalDiff(node::Node,arg::Node,dict::Dict{Derivative,Node},ssatape::SSATape)
    if(node isa NumberNode)
        return evalTape(Basic(0),ssatape)
    end    
    ∂node∂arg=Derivative(node,arg)
    if(node isa ComputeNode)
        if(!haskey(dict,∂node∂arg))        
            ssaform=getfromA(ssatape.compute,node)
        ∂node∂arg_node=evalDiff(ssaform,arg,dict,ssatape)
        dict[∂node∂arg]=∂node∂arg_node
        end
    end
    dict[∂node∂arg]
end

function evalDiff(node::Node,args::Array{T,1},dict::Dict{Derivative,Node},ssatape::SSATape) where T<: Node
    [evalDiff(node,arg_,dict,ssatape) for arg_ in args]
end

"""
add print information
"""
function evalDiff(args::Array{T,1},dict::Dict{Derivative,Node} ,ssatape::SSATape) where T<:Node
    current_compute_size=length(ssatape.compute)
    idx=0
    # we use 1000 steps to show and perform gc
    N_print_and_gc_idx=round(Int,current_compute_size/1000)
    for i in 1:current_compute_size
        # print and perform gc
        if(idx>N_print_and_gc_idx)
            idx=0
            print("$(i)/$(current_compute_size)\n")
            customized_gc()
        end
        idx+=1
        evalDiff(ComputeNode(i-1),args,dict,ssatape)
    end    
end

function initDefaultDiffDict(args::Array{T,1},ssatape::SSATape) where T<:Node
    dict=Dict{Derivative,Node}()
    for i in args
        for j in args
            diff=Derivative(i,j)
            value=1.0
            if(i!=j)
                value=0.0
            end        
            dict[diff]=evalTape(Basic(value),ssatape)
        end
    end
    dict
end


# now, we need to add the derivative to the output
# to facilatate the further use, we use a string ∂y∂x
"""
test case,
assuming y is in output and x is in input 
sym=Symbol("∂r∂x")
"""
function evalDiff(sym::Symbol,dict::Dict{Derivative,Node},ssatape::SSATape)
    tokens=split("$(sym)","∂")
    y_str=tokens[2]
    y_node=ssatape.output[Symbol(y_str)]
    x_str=tokens[3]
    x_node=getfromB(ssatape.input,Symbol(x_str))
    dict[Derivative(y_node,x_node)]
end

"""
add diff sym to output of ssatape
"""
function addToOutput(sym::Symbol,dict::Dict{Derivative,Node},ssatape::SSATape)
    sym_node=evalDiff(sym,dict,ssatape)
    evalTape(sym_node,sym,ssatape)
end

"""
we add a more intuitive way to add outputs for derivatives
make sure to call evalDiff for all args before add to output
"""
function addToOutput(targets::Array{Symbol,1},args::Array{Symbol,1},dict::Dict{Derivative,Node},ssatape::SSATape)
    syms=defaultDerivatives(targets,args)
    [addToOutput(sym_,dict,ssatape) for sym_ in syms],syms
end

"""
The default order to store the first order derivatives
"""
function defaultDerivatives(targets::Array{Symbol,1},args::Array{Symbol,1})
    [Symbol("∂$(target_)∂$(arg_)")  for target_ in targets for arg_ in args]
end
