
import core
import parse
import natives
import os
import strutils

var vm = createVM()
vm.bootstrap(vm.makeNatives)

let ob = makeObject(vm)
echo "ob: ", toHex(cast[int](ob.head))
var user = vm.alloc (ob)

echo "user: ", toHex(cast[int](user))

proc interpret(s: string) =
  let b = vm.parse s
  echo "evaluating:", b
  vm.dumpHeap 0, 20
  vm.expandAll(b, user)
  vm.dumpHeap 0, 20
  vm.bindAll(b, user)
  vm.dumpHeap(0, 20)
  echo "EVAL: ", vm.eval(b) 
  vm.dumpHeap 0, 30

# let code = if paramCount() > 0: readFile paramStr(1)
#            else: readAll stdin

interpret """
  print 42
  my: func [x] [print x]
  my 77
"""