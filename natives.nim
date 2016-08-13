
import core
import parse

import strutils

proc printImpl(vm: VM, code: HeapSlot, rx: var VMValue): HeapSlot  =
  echo "code: ", toHex(cast[int](code))
  result = eval(vm, code, rx)  
  echo "printing: ", rx

# proc funcImpl(vm: VM): Value {.cdecl.} =
#   let params = vm.stackFrame 1
#   let body = vm.stackFrame 2
#   result = vm.makeFunc(params.asBlock, body.asBlock)

proc makeNatives*(vm: VM): HeapSlot =
  var natives = vm.makeObject()
  vm.add natives, !"print", printImpl 
  #vm.add natives, !"func", vm.makeFunc(vm.parse "params body", funcImpl)
  result = natives 

when isMainModule:
  var vm = createVM()
  discard vm.makeNatives()
  vm.dumpHeap 0, 20