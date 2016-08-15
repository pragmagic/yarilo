
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
  #echo "evaluating:", b
  vm.expandAll(b, user)
  vm.bindAll(b, user)
  echo "Result: ", vm.eval(b) 

let code = if paramCount() > 0: readFile paramStr(1)
           else: readAll stdin

interpret code

# let a =  """
#   сумма: пусть изначально 0
#   число: же изначально 1000000
#   пока [число поболее чем 0] [
#     пусть сумма: будетъ к сумма присовокупить число 
#     число: же езъмь число отнять 1
#   ]
#   ответствуй сумма
# """

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