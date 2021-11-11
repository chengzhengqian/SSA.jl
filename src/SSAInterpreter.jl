export SSAInterpreter, interpret
"""
we assume that the ssatape should be fixed after the creation of interpreter
Also, we assume that data type is always Float64.
"""
struct SSAInterpreter
    ssatape::SSATape
    computeArray::Array{Float64,1}
    # the input order is assumed to following the input in ssatape
    # store the order for the output
    outputArray::Array{Symbol,1}    
end

function SSAInterpreter(ssatape::SSATape,outputArray::Array{Symbol,1})
    computeArray=zeros(length(ssatape.compute))
    SSAInterpreter(ssatape,computeArray,outputArray)
end

function Base.show(io::IO,interpreter::SSAInterpreter)
    input_args=getInputArgs(interpreter)
    output_args=interpreter.outputArray
    input=join(input_args,",")
    output=join(output_args,",")
    print(io,"SSAInterpreter:($(length(input_args)) elements to $(length(output_args)) elements)[($(input))->$(output)]")
end


function interpret(node::Node,interpreter::SSAInterpreter,  inputArray::Array{Float64,1})
    if(node isa InputNode)
        return inputArray[node.idx+1]
    elseif(node isa ComputeNode)
        return interpreter.computeArray[node.idx+1]
    elseif(node isa NumberNode)
        return getfromA(interpreter.ssatape.number,node)
    else
        error("unexpected node type $(typeof(node))")
    end
end

function interpret(interpreter::SSAInterpreter, inputArray::Array{Float64,1})
    # i=1
    computeArray=interpreter.computeArray
    for i in 1:length(computeArray)
        ssaform=getfromA(interpreter.ssatape.compute,ComputeNode(i-1))
        op=ssaform.op
        args=[interpret(arg_,interpreter,inputArray) for arg_ in  ssaform.args]
        computeArray[i]=eval((:($op(($args)...))))
    end
    [interpret(interpreter.ssatape.output[sym_],interpreter,inputArray)  for sym_ in interpreter.outputArray]
end

function (interpreter::SSAInterpreter)(inputArray...)
    interpret(interpreter,collect(inputArray))
end

function getInputArgs(interpreter::SSAInterpreter)
    ssatape=interpreter.ssatape
    getInputArgs(ssatape)
end

