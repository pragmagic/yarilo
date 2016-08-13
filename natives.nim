
import core
import parse

proc printImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
  result = eval(vm, code, rx)  
  echo "printing: ", rx

proc funcImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot =
  var params, body: VMValue
  result = eval(vm, code, params)
  result = eval(vm, result, body)
  rx.store vm.makeFunc(vmcast[BlockHead](params), vmcast[BlockHead](params))

proc makeNatives*(vm: VM): ObjectHead =
  var natives = vm.makeObject()
  vm.add natives, !"print", printImpl 
  vm.add natives, !"func", funcImpl
  result = natives 

when isMainModule:
  var vm = createVM()
  discard vm.makeNatives()
  vm.dumpHeap 0, 20