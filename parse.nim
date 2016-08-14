
import core
import parseutils
import strutils

proc parse(vm: VM, s: string, pos: var int): BlockBuilder =
  result = makeBlock(vm)
  var i = pos
  while true:
    if i >= s.len: break
    case s[i]
    of Whitespace: inc i
    of ']': 
      inc i
      break
    of '[':
      inc i
      vm.add result, vm.parse(s, i)
    of Digits:
      let start = i
      while i < s.len and (s[i] in Digits): inc(i)
      let iv = parseInt(s.substr(start, i-1))
      vm.add result, iv
    else:
      let start = i
      while i < s.len and not (s[i] in Whitespace) and s[i] != ']': inc(i)
      let w = substr(s, start, i-1)
      if w[^1] == ':':
        vm.add result, SetWord(!substr(w, 0, w.len - 2))
      elif w == "true":
        vm.add result, true
      elif w == "false":
        vm.add result, false
      elif s[start] in {'+', '-', '='}:
        vm.add result, Operation(!w)        
      else:
        vm.add result, Word(!w)
  pos = i

proc parse*(vm: VM, s: string): BlockHead =
  var pos = 0
  result = vm.parse(s, pos)

when isMainModule:
  var vm = createVM()
  echo vm.parse "1 [a b] 2 3 true false 5"
  vm.dumpHeap(0, 20)

