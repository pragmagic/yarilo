
import core
import parse

proc printImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
  result = eval(vm, code, rx)
  echo "printing: ", rx

proc funcImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
  var params, body: VMValue
  result = eval(vm, code, params)
  result = eval(vm, result, body)
  rx.store vm.makeFunc(vmcast[BlockHead](params), vmcast[BlockHead](body))

proc addImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
  var a, b: VMValue
  result = eval(vm, code, a)
  result = eval(vm, result, b)
  rx.store vmcast[int](a) + vmcast[int](b)

proc equalImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
  var a, b: VMValue
  result = eval(vm, code, a)
  result = eval(vm, result, b)
  rx.store vmcast[int](a) == vmcast[int](b)

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

proc makeNatives*(vm: VM): ObjectHead =
  var natives = vm.makeObject()
  vm.add natives, !"print", printImpl 
  vm.add natives, !"func", funcImpl
  vm.add natives, !"while", whileImpl
  vm.add natives, !"add", addImpl
  vm.add natives, !"equal", equalImpl 
  vm.add natives, !"+", addImpl
  vm.add natives, !"=", equalImpl
  result = natives 

when isMainModule:
  var vm = createVM()
  discard vm.makeNatives()
  vm.dumpHeap 0, 20