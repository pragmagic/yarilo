
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
  print 12 + 42
  print 5 = 5
  i: 5
  print equal i 5
  i: 6
  print equal i 5
  while [i = 6] [print i + 55 i: 7]  
"""

let x = """" 
  my: func [x] [print x]
  equal 5 5
  add 77 99

"""