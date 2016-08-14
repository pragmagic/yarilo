
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
      if s[i-1] == ':':
        vm.add result, SetWord(!substr(s, start, i-2))
      elif s[start] in {'+', '-'}:
        vm.add result, Operation(!substr(s, start, i-1))        
      else:
        vm.add result, Word(!substr(s, start, i-1))
  pos = i

proc parse*(vm: VM, s: string): BlockHead =
  var pos = 0
  result = vm.parse(s, pos)

when isMainModule:
  var vm = createVM()
  echo vm.parse "1 [a b] 2 3"
  vm.dumpHeap(0, 20)

