
import core
import parse
import natives
import os
import strutils

var vm = createVM()
vm.bootstrap(vm.makeNatives)

var user = vm.alloc makeObject(vm)

proc interpret(s: string) =
  let b = vm.parse s
  echo "evaluating:", b
  vm.expandAll(b, user)
  vm.bindAll(b, user)
  echo "EVAL: ", vm.eval(b) 

# let code = if paramCount() > 0: readFile paramStr(1)
#            else: readAll stdin

interpret """
  print 42
  my: func [x] [print x]
  my 77 + 22
"""