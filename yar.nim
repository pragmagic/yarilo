
import core
import parse
import natives
import os
import strutils

var vm = createVM()

var user = vm.alloc vm.makeObject
var sys = vm.alloc vm.makeNatives

proc interpret(s: string) =
  let b = vm.parse s
  #echo "evaluating:", b
  vm.expandAll(b, user)
  vm.bindAll(b, sys)
  vm.bindAll(b, user)
  vm.push 
  vm.eval(b)  

if paramCount() > 0: 
  interpret readFile paramStr(1)
else: 
  echo "please specify script file."

# let z = """
#   print 12 + 42
#   print 5 = 5
#   i: 5
#   print equal i 5
#   i: 6
#   my: func [x] [print x + 100000]
#   print equal i 5
#   my 88
#   while [i = 6] [print i + 55 i: 7]  
# """

# let x = """" 
#   my: func [x] [print x]
#   equal 5 5
#   add 77 99

# """

# interpret a