# compile a SSATape to machine code
# we pass 4 array to the function
#  input array, compute array, numeric array
#  the output is just read from compute array

using JITFunc
using JLD
export compile, opToInstruction, movInstruction,initComputeArray, initNumberArray, getOutputArray,SSACompiledFunc,saveSSAFunc,loadSSAFunc,convertToOutputIndex
# length(ssatape.compute)
const nodeToIdx=Dict(InputNode=>1,ComputeNode=>2,NumberNode=>3)
function compile(ssatape::SSATape)
    # we first evaluate the compute node
    buffer=IOBuffer()
    print(buffer,"BITS 64\n")
    n_compute=length(ssatape.compute)
    for i in 1:n_compute
        target=ComputeNode(i-1)
        ssaform=getfromA(ssatape.compute,target)
        compute_to_xmm0=compile(ssaform)
        copy_to_target="$(movInstruction) $(compile(target)), xmm0"
        print(buffer,compute_to_xmm0)
        print(buffer,"\n")
        print(buffer,copy_to_target)
        print(buffer,"\n")
    end
    print(buffer,"ret\n")
    String(take!(buffer))
end

"""
node that we assume the SSAForm are regular, i.e, only +, - and *, and the number of args is at least 2.
"""
const opToInstruction=Dict((:+)=>"addsd",(:-)=>"subsd",(:*)=>"mulsd")
const movInstruction="movsd"           # "movq" is also fine.

function compile(ssaform::SSAForm)
    opInstruction=opToInstruction[ssaform.op]
    args=ssaform.args
    n_args=length(args)
    if(n_args>=2)
        instr1="$(movInstruction) xmm0,$(compile(ssaform.args[1]))"
        instr_remains=[ "$(opInstruction) xmm0,$(compile(ssaform.args[i]))"  for i in 2:length(args)]
        return join([instr1,instr_remains...],"\n")
    else
        error("$(ssaform) has too few arguments(at least 2)!")
    end    
end

function compile(node::Node)
    return "[$(reg(nodeToIdx[typeof(node)]))+$(node.idx)*8]"
end

function initComputeArray(ssatape::SSATape)
    n_compute=length(ssatape.compute)
    zeros(n_compute)
end

function initNumberArray(ssatape::SSATape)
    n_number=length(ssatape.number)
    [ getfromA(ssatape.number,NumberNode(i-1)) for i in 1:n_number]
end

function getOutputArray(ssatape::SSATape,outputSyms::Array{Symbol,1},computeArray)
    computeArray[[ssatape.output[sym_].idx+1 for sym_ in outputSyms]]
end

# now, we dress up the function,

struct SSACompiledFunc
    func::Func                  # the native machine code
    computeArray::Array{Float64,1}
    numberArray::Array{Float64,1}
    inputSize::Int
    outputSize::Int
    # notice the output could from inputArray or compute Array
    # so we need a tuple to store it
    outputIndex::Array{Tuple{Int,Int},1}
end


function Base.show(io::IO,func::SSACompiledFunc)
    print(io,"SSACompiledFunc: $(func.inputSize) numbers -> $(func.outputSize) numbers [$(func.func.size[1]) bytes]")
end

function convertToOutputIndex(node::Node)
    idx=node.idx+1
    if(node isa ComputeNode)
        type_idx=1
    elseif(node isa InputNode)
        type_idx=2
    else(node isa NumberNode)
        type_idx=3
    end
    (type_idx,idx)
end


function SSACompiledFunc(ssatape::SSATape,outputSyms::Array{Symbol,1})
    asm=compile(ssatape)
    inputSize=length(ssatape.input)
    outputSize=length(outputSyms)
    outputIndex=[convertToOutputIndex(ssatape.output[sym_]) for sym_ in outputSyms]
    computeArray=initComputeArray(ssatape)
    numberArray=initNumberArray(ssatape)
    func=Func(asm, "ssaCompillerGenerated",mode="nasm")
    SSACompiledFunc(func,computeArray,numberArray,inputSize,outputSize,outputIndex)
end

function (compiledFunc::SSACompiledFunc)(args...)
    inputArray=collect(args)
    if(length(inputArray)!=compiledFunc.inputSize)
        error("$(compiledFunc) takes $(compiledFunc.inputSize) numbers, but get $(length(inputArray)) args!")
    end
    compiledFunc.func(inputArray,compiledFunc.computeArray,compiledFunc.numberArray)
    getOutputArray(compiledFunc,inputArray)
end

function getOutputArray(func::SSACompiledFunc,inputArray)
    buffers=[func.computeArray,inputArray,func.numberArray]
    [  buffers[arr_idx][idx] for (arr_idx,idx) in func.outputIndex]
end


# add function to save SSACompiledFunc
function saveSSAFunc(func::SSACompiledFunc,filename)
    saveFunc(func.func,"$(filename)_func.bin")
    save("$(filename)_info.jld","numberArray",func.numberArray,"computeArraySize",length(func.computeArray),"inputSize",func.inputSize,"outputSize",func.outputSize,"outputIndex",func.outputIndex)
end

function loadSSAFunc(filename)
    jitfunc=loadFunc("$(filename)_func.bin")
    data=load("$(filename)_info.jld")
    SSACompiledFunc(jitfunc,zeros(data["computeArraySize"]),data["numberArray"],data["inputSize"],data["outputSize"],data["outputIndex"])
end
