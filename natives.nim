
import core
import parse

proc printImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
  result = eval(vm, code, rx)  
  echo rx

proc funcImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
  var params, body: VMValue
  result = eval(vm, code, params)
  result = eval(vm, result, body)
  rx.store vm.makeFunc(vmcast[BlockHead](params), vmcast[BlockHead](params))

proc addImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
  var a, b: VMValue
  result = eval(vm, code, a)
  result = eval(vm, result, b)
  rx.store vmcast[int](a) + vmcast[int](b)

proc makeNatives*(vm: VM): ObjectHead =
  var natives = vm.makeObject()
  vm.add natives, !"print", printImpl 
  vm.add natives, !"func", funcImpl
  vm.add natives, !"+", addImpl
  result = natives 

when isMainModule:
  var vm = createVM()
  discard vm.makeNatives()
  vm.dumpHeap 0, 20