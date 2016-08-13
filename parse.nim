
#
# High level API for doing RVM in Nim
#

import core
import parseutils
import strutils

proc parse(vm: VM, s: string, pos: var int): BlockHead =
  var builder: BlockBuilder
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
      vm.add builder, vm.parse(s, i)
    of Digits:
      let start = i
      while i < s.len and (s[i] in Digits): inc(i)
      let iv = parseInt(s.substr(start, i-1))
      vm.add builder, iv
    else:
      let start = i
      while i < s.len and not (s[i] in Whitespace) and s[i] != ']': inc(i)
      if s[i-1] == ':':
        vm.add builder, SetWord(!substr(s, start, i-2))
      else:
        vm.add builder, Word(!substr(s, start, i-1))
  pos = i
  result = builder

proc parse*(vm: VM, s: string): BlockHead =
  var pos = 0
  result = vm.parse(s, pos)

when isMainModule:
  var vm = createVM()
  discard vm.parse "x y z"
  vm.dumpHeap(0, 20)

