
import core
import parse


proc printImpl(vm: VM, rx: var VMValue) =
  rx = vm.param(0)[]
  echo "print: ", rx

proc funcImpl(vm: VM, rx: var VMValue) =
  rx.store vm.makeFunc(vmparam[BlockHead](vm, 0), vmparam[BlockHead](vm, 1))

template binOp(f: expr, T: typedesc[Value], op: expr) =
  {.push overflowChecks:off.}
  proc f(vm: VM, rx: var VMValue) =
    rx.store op(vmparam[T](vm, 0), vmparam[T](vm, 1))
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

proc whileImpl(vm: VM, rx: var VMValue) =
  let cond = vmparam[BlockHead](vm, 0)
  let body = vmparam[BlockHead](vm, 1)
  while true:
    var condition: VMValue
    evalAll(vm, cond, condition)
    if vmcast[bool](condition):
      evalAll(vm, body, rx)
    else:
      break

proc eitherImpl(vm: VM, rx: var VMValue) =
  let blockCond = vmparam[BlockHead](vm, 0)
  let blockThen = vmparam[BlockHead](vm, 1)
  let blockElse = vmparam[BlockHead](vm, 2)
  var cond: VMValue
  evalAll(vm, blockCond, cond)
  if vmcast[bool](cond):
    evalAll(vm, blockThen, rx)
  else:
    evalAll(vm, blockElse, rx)

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