
import core
import parse
import natives
import os
import strutils

var vm = createVM()
vm.bootstrap(vm.makeNatives)

var user = makeObject(vm)

proc interpret(s: string) =
  let b = vm.parse s
  echo b
  vm.bindAll(b, user)
  vm.dumpHeap(0, 20)
  echo "EVAL: ", vm.eval(b) 
  vm.dumpHeap 0, 30

# let code = if paramCount() > 0: readFile paramStr(1)
#            else: readAll stdin

interpret """
  print 42
"""
#   my: func [x] [print x]

#   my 77
# """