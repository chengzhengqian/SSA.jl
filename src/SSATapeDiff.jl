# add forward for SSATape
export Derivative, evalDiff, initDefaultDiffDict, addToOutput, defaultDerivatives, diffTape
# we first define a structure to represent derivatives
# right now, we only consider first order
# we now use the customize derivative formulas instead of using SymEngine
#TODO add more conveinent API
struct Derivative
    target::Node
    arg::Node
end

function Base.show(io::IO,diff::Derivative)
    print(io,"∂$(diff.target)/∂$(diff.arg)")
end

"""
we assume ssaform only have * and -, which is true from the new way to construct the ssatape
"""
function evalDiff(ssaform::SSAForm,arg::Node,diffMap::Dict{Derivative,Node},ssatape::SSATape)
    if(ssaform.op==(:*))
        return evalDiffMul(ssaform.args,arg,diffMap,ssatape)
    elseif (ssaform.op==(:+))
        return evalDiffAdd(ssaform.args,arg,diffMap,ssatape)
    else
        error("unsupported op for $(ssaform)")
    end    
end

# old version
# function evalDiff(ssaform::SSAForm,arg::Node,dict::Dict{Derivative,Node},ssatape::SSATape)
#     expr=Basic("$(ssaform)")
#     result=Basic(0)
#     for v in free_symbols(expr)
#         v_=evalTape(Symbol("$(v)"),ssatape)
#         result+=diff(expr,v)*convert(Basic,evalDiff(v_,arg,dict,ssatape),ssatape)
#     end
#     evalTape(result,ssatape)
# end

"""
ssafrom=(:+,terms)

"""
function evalDiffAdd(terms::Array{Node,1}, arg::Node, diffMap::Dict{Derivative,Node},ssatape::SSATape)
    ∂terms∂arg=Array{Node,1}()
    for term in terms
        ∂term∂arg=evalDiff(term,arg,diffMap,ssatape)
        if(∂term∂arg!=ssatape(0.0))
            push!(∂terms∂arg,∂term∂arg)
        end        
    end
    return sumTerms(∂terms∂arg,ssatape)
end

"""
sum nodes. Assume all of them is non-zero
"""
function sumTerms(terms::Array{Node,1},ssatape::SSATape)
    n_terms=length(terms)
    if(n_terms==0)
        return ssatape(0.0)
    elseif(n_terms==1)
        return terms[1]
    else
        return ssatape(SSAForm(:+, terms))
    end    
end
"""
assuming all of term is non-zero and not identity
"""
function mulTerms(terms::Array{Node,1},ssatape::SSATape)
    n_terms=length(terms)
    if(n_terms==0)
        return ssatape(1.0)
    elseif(n_terms==1)
        return terms[1]
    else
        return ssatape(SSAForm(:*, terms))
    end    
end


"""
ssaform=(:*, terms)
idx=1
check wether the the derivative is zero or one to simplify the calculation
"""
function evalDiffMul(terms::Array{Node,1}, arg::Node, diffMap::Dict{Derivative,Node},ssatape::SSATape)
    ∂terms∂arg=Array{Node,1}()
    for idx in 1:length(terms)
        term=terms[idx]
        ∂term∂arg=evalDiff(term,arg,diffMap,ssatape)
        if(∂term∂arg!=ssatape(0.0))
            terms_new=copy(terms)
            if(∂term∂arg!=ssatape(1.0))
                terms_new[idx]=∂term∂arg
            else
                deleteat!(terms_new,idx)
            end
            push!(∂terms∂arg,mulTerms(terms_new,ssatape))
        end        
    end
    return sumTerms(∂terms∂arg,ssatape)
end



"""
evaluate the node to node derivative
"""
function evalDiff(node::Node,arg::Node,dict::Dict{Derivative,Node},ssatape::SSATape)
    if(node isa NumberNode)
        return evalTape(0,ssatape)
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
    evalDiff(length(ssatape.compute)-1,args,dict,ssatape)
end

"""
idx start from 0
idx=2000
"""
function evalDiff(idx::Int,args::Array{T,1},dict::Dict{Derivative,Node} ,ssatape::SSATape) where T<:Node
    N_print_idx=trunc(Int,idx/100)    
    for i in 0:idx        
        evalDiff(ComputeNode(i),args,dict,ssatape)
        if(i%N_print_idx==0)
            print("evalDiff $(i)/$(idx)\n")
        end        
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
"""
assuming we need the diff for all inputs
"""
function initDefaultDiffDict(ssatape::SSATape)
    initDefaultDiffDict(ssatape.(getInputArgs(ssatape)),ssatape)
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


"""
we provide the convinient API by ensure that we automatically compute all the necessary forward steps
diffMap=initDefaultDiffDict(ssatape.(getInputArgs(ssatape)),ssatape)
diffMap=initDefaultDiffDict(ssatape)
typeof(diffMap).
So one only need diffTape and a diffMap (for efficiency to reuse the derivatives)
"""
function diffTape(targets::Array{Symbol,1},args::Array{Symbol,1},diffMap::Dict{Derivative,Node} ,ssatape::SSATape)
    max_compute_node_idx=0
    for target in targets
        node=ssatape.output[target]
        if(node isa ComputeNode)
            if(node.idx>max_compute_node_idx)
                max_compute_node_idx=node.idx
            end            
        end        
    end
    args_node=ssatape.(args)
    evalDiff(max_compute_node_idx,args_node,diffMap,ssatape)
    addToOutput(targets,args,diffMap,ssatape)
end
