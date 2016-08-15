
import core
import parse


proc printImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
  result = eval(vm, code, rx)
  echo rx

proc funcImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
  var params, body: VMValue
  result = eval(vm, code, params)
  result = eval(vm, result, body)
  rx.store vm.makeFunc(vmcast[BlockHead](params), vmcast[BlockHead](body))

template binOp(f: expr, T: typedesc[Value], op: expr) =
  {.push overflowChecks:off.}
  proc f(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
    var a, b: VMValue
    result = eval(vm, code, a)
    result = eval(vm, result, b)
    rx.store op(vmcast[T](a), vmcast[T](b))
  {.pop.}

binOp(addImpl, int, `+`)  
binOp(subImpl, int, `-`)  
binOp(mulImpl, int, `*`)  
binOp(eqImpl, int, `==`)  
binOp(gtImpl, int, `>`)  

proc decImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
  store rx, vmcast[int](code.getWord()) - 1
  code.getWord() = rx  
  result = code.next

proc skipImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
  eval(vm, code, rx)

proc whileImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
  result = eval(vm, code, rx)
  let cond = vmcast[BlockHead](rx)
  result = eval(vm, result, rx)
  let body = vmcast[BlockHead](rx)
  while true:
    var condition: VMValue
    evalAll(vm, cond, condition)
    if vmcast[bool](condition):
      evalAll(vm, body, rx)
    else:
      break

proc eitherImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
  result = eval(vm, code, rx)
  let cond = vmcast[BlockHead](rx)
  result = eval(vm, result, rx)
  let bodyThen = vmcast[BlockHead](rx)
  result = eval(vm, result, rx)
  let bodyElse = vmcast[BlockHead](rx)
  var condition: VMValue
  evalAll(vm, cond, condition)
  if vmcast[bool](condition):
    evalAll(vm, bodyThen, rx)
  else:
    evalAll(vm, bodyElse, rx)

proc makeNatives*(vm: VM): ObjectHead =
  var natives = vm.makeObject()
  vm.add natives, !"+", addImpl
  vm.add natives, !"*", mulImpl
  vm.add natives, !"-", subImpl
  vm.add natives, !"=", eqImpl
  vm.add natives, !">", gtImpl

  vm.add natives, !"add", addImpl 
  vm.add natives, !"equal", eqImpl

  vm.add natives, !"print", printImpl 
  vm.add natives, !"func", funcImpl
  vm.add natives, !"while", whileImpl
  vm.add natives, !"either", eitherImpl
  vm.add natives, !"dec", decImpl

  vm.add natives, !"пусть", skipImpl
  vm.add natives, !"будетъ", skipImpl
  vm.add natives, !"езъмь", skipImpl
  vm.add natives, !"изначально", skipImpl
  vm.add natives, !"к", skipImpl
  vm.add natives, !"чем", skipImpl
  vm.add natives, !"же", skipImpl
  vm.add natives, !"то", skipImpl
  vm.add natives, !"да", skipImpl

  vm.add natives, !"присовокупить", addImpl
  vm.add natives, !"отнять", subImpl
  vm.add natives, !"предыдущее", decImpl
  vm.add natives, !"ответствуй", printImpl 
  vm.add natives, !"пока", whileImpl
  vm.add natives, !"поболее", gtImpl
  
  result = natives 

when isMainModule:
  var vm = createVM()
  discard vm.makeNatives()
  vm.dumpHeap 0, 20