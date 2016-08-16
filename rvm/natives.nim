
import core
import parse


proc printImpl(vm: VM) =
  vm.top() = vm.param(-1)
  echo "print: ", vm.top()

proc funcImpl(vm: VM) =
  vm.top.store vm.makeFunc(vmparam[BlockHead](vm, -2), vmparam[BlockHead](vm, -1))

template binOp(f: expr, T: typedesc[Value], op: expr) =
  {.push overflowChecks:off.}
  proc f(vm: VM) =
    vm.top.store op(vmparam[T](vm, -2), vmparam[T](vm, -1))
  {.pop.}

binOp(addImpl, int, `+`)
binOp(subImpl, int, `-`)
binOp(mulImpl, int, `*`)
binOp(eqImpl, int, `==`)
binOp(gtImpl, int, `>`)

# proc decImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
#   store rx, vmcast[int](code.getWord()) - 1
#   code.getWord() = rx  
#   result = code.next

# proc skipImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
#   eval(vm, code, rx)

proc whileImpl(vm: VM) =
  let cond = vmparam[BlockHead](vm, -2)
  let body = vmparam[BlockHead](vm, -1)
  while true:
    evalAll(vm, cond)
    if vmcast[bool](vm.top()):
      evalAll(vm, body)
    else:
      break

proc eitherImpl(vm: VM) =
  let blockCond = vmparam[BlockHead](vm, -3)
  let blockThen = vmparam[BlockHead](vm, -2)
  let blockElse = vmparam[BlockHead](vm, -1)
  evalAll(vm, blockCond)
  if vmcast[bool](vm.top()):
    evalAll(vm, blockThen)
  else:
    evalAll(vm, blockElse)

proc makeNatives*(vm: VM): ObjectHead =
  var natives = vm.makeObject()

  proc add(name: string, params: BlockHead, impl: Native) =
    vm.add natives, !name, vm.makeFunc(params, impl)

  let One = vm.parse "a"
  let Two = vm.parse "x y"
  let TwoBlocks = vm.parse "a b"
  let Three = vm.parse "cond then else"

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
  add "func", TwoBlocks, funcImpl
  add "while", TwoBlocks, whileImpl
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