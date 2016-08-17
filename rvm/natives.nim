
import core
import parse


proc printImpl(vm: VM): VMValue =
  result = vm.param(0)
  echo "print: ", result

proc funcImpl(vm: VM): VMValue =
  store result, vm.makeFunc(vmparam[BlockHead](vm, -1), vmparam[BlockHead](vm, 0))

{.push overflowChecks:off.}
proc addImpl(vm: VM): VMValue =
  store result, vmparam[int](vm, -1) + vmparam[int](vm, 0)

proc subImpl(vm: VM): VMValue =
  store result, vmparam[int](vm, -1) - vmparam[int](vm, 0)

proc mulImpl(vm: VM): VMValue =
  store result, vmparam[int](vm, -1) * vmparam[int](vm, 0)

proc eqImpl(vm: VM): VMValue =
  store result, vmparam[int](vm, -1) == vmparam[int](vm, 0)

proc gtImpl(vm: VM): VMValue =
  store result, vmparam[int](vm, -1) > vmparam[int](vm, 0)
{.pop.}


# proc decImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
#   store rx, vmcast[int](code.getWord()) - 1
#   code.getWord() = rx  
#   result = code.next

# proc skipImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
#   eval(vm, code, rx)

proc whileImpl(vm: VM): VMValue =
  let cond = vmparam[BlockHead](vm, -1)
  let body = vmparam[BlockHead](vm, 0)
  while true:
    if vmcast[bool](evalAll(vm, cond)):
      result = evalAll(vm, body)
    else:
      break

proc eitherImpl(vm: VM): VMValue =
  let blockCond = vmparam[BlockHead](vm, -2)
  let blockThen = vmparam[BlockHead](vm, -1)
  let blockElse = vmparam[BlockHead](vm, 0)
  if vmcast[bool](evalAll(vm, blockCond)):
    result = evalAll(vm, blockThen)
  else:
    result = evalAll(vm, blockElse)

proc makeNatives*(vm: VM): ObjectHead =
  var natives = vm.makeObject()

  proc add(name: string, params: int, impl: Native) =
    vm.add natives, !name, vm.makeFunc(params, impl)

  let One = 1
  let Two = 2
  let Three = 3

  vm.add natives, !"+", addImpl
  vm.add natives, !"*", mulImpl
  vm.add natives, !"-", subImpl
  vm.add natives, !"=", eqImpl
  vm.add natives, !">", gtImpl

  add "add", Two, addImpl
  add "mul", Two, mulImpl
  add "sub", Two, subImpl
  add "eq", Two, eqImpl
  add "gt", Two, gtImpl

  add "print", One, printImpl 
  add "func", Two, funcImpl
  add "while", Two, whileImpl
  add "either", Three, eitherImpl
  # vm.add natives, !"dec", decImpl

  # vm.add natives, !"пусть", skipImpl
  # vm.add natives, !"будетъ", skipImpl
  # vm.add natives, !"езъмь", skipImpl
  # vm.add natives, !"изначально", skipImpl
  # vm.add natives, !"к", skipImpl
  # vm.add natives, !"чем", skipImpl
  # vm.add natives, !"же", skipImpl
  # vm.add natives, !"то", skipImpl
  # vm.add natives, !"да", skipImpl

  # vm.add natives, !"присовокупить", addImpl
  # vm.add natives, !"отнять", subImpl
  # vm.add natives, !"предыдущее", decImpl
  # vm.add natives, !"ответствуй!", printImpl 
  # vm.add natives, !"пока", whileImpl
  # vm.add natives, !"поболее", gtImpl
  
  result = natives 

when isMainModule:
  var vm = createVM()
  discard vm.makeNatives()
  vm.dumpHeap 0, 20