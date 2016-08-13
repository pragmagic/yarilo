

include sym

const 
  HeapSize = 1 shl 16
  StackSize = 1 shl 16
  StackFrameSize = 256

type
  EvalProc = proc(vm: VM, code: Code, rx: var VMValue): Code {.nimcall.}
  ToStringProc = proc(data: pointer): string {.nimcall.}

  TypeKind = enum
    tkNone
    tkInt
    tkWord
    tkSetWord
    tkGetWord
    tkNative
    tkBlock
    tkObject
    tkFunc

  PType = ptr VMType
  VMType = object
    eval: EvalProc
    kind: TypeKind
    toString: ToStringProc
  
  VMValue = object
    typ: PType
    data: pointer

  HeapSlot* = ptr HeapLayout
  HeapLayout = object
    val: VMValue
    nxt: HeapSlot
    ext: pointer

  Heap = ptr array[HeapSize, HeapLayout]
  Stack = ptr array[StackSize, VMValue]
  StackFrame = ptr array[StackFrameSize, VMValue]

  Code = HeapSlot

  VM* = ptr RVM
  RVM = object
    ax: VMValue         # 
    ip: Code          # Resembles HeapLayout
    ext: pointer      #

    fp: int           # current frame size
    frame: StackFrame

    sp: int
    stack: Stack 

    tail: int
    heap: Heap
    
    sysMod: HeapSlot

var types: array[TypeKind, VMType]

#
# Accessors
#

template kind(s: HeapSlot): expr = s.val.typ.kind

#
# Heap Operations
#

template alloc*(vm: VM): expr = 
  let slot = addr vm.heap[vm.tail]
  inc vm.tail
  slot

template alloc(vm: VM, s: Symbol): expr =
  let slot = addr vm.heap[vm.tail]
  slot.ext = cast[pointer](s)  
  inc vm.tail
  slot

#
# Stack Op
#

template saveFrame(vm: VM) =
  vm.frame[vm.fp].data = cast[pointer](vm.fp)
  inc vm.fp
  copyMem(addr vm.stack[vm.sp], vm.frame, vm.fp * sizeof(VMValue))
  inc vm.sp, vm.fp
  vm.fp = 0


template restoreFrame(vm: VM) =
  vm.fp = cast[int](vm.stack[vm.sp].data)
  dec vm.sp, vm.fp + 1
  copyMem(vm.frame, addr vm.stack[vm.sp], vm.fp * sizeof(VMValue))

#
# List Ops
#

template first(s: HeapSlot): expr = cast[HeapSlot](s.val.data) 

iterator items(s: HeapSlot): HeapSlot =
  var i = s
  while i != nil:
    yield i
    i = i.nxt 

#
# Eval
#

template eval(vm: VM, code: Code, ax: var VMValue): expr = code.val.typ.eval(vm, code, ax)

proc evalAll(vm: VM, code: Code, rx: var VMValue) {.inline.} =
  var ip = code
  while ip != nil:
    ip = eval(vm, ip, rx)

proc evalConst(vm: VM, code: Code, rx: var VMValue): Code {.nimcall.} =
  rx = code.val
  result = code.nxt

proc evalWord(vm: VM, code: Code, rx: var VMValue): Code {.nimcall.} =
  vm.ax = cast[HeapSlot](code.ext).val
  vm.ip = code.nxt
  eval(vm, cast[HeapSlot](vm), rx) # tail call

proc evalGetWord(vm: VM, code: Code, rx: var VMValue): Code {.nimcall.} =
  rx = cast[HeapSlot](code.ext).val
  result = code.nxt

proc evalSetWord(vm: VM, code: Code, rx: var VMValue): Code {.nimcall.} =
  eval(vm, code.nxt, cast[HeapSlot](code.ext).val) # tail call

proc evalNative(vm: VM, code: Code, rx: var VMValue): Code {.nimcall.} =
  (cast[EvalProc](code.val.data))(vm, code.nxt, rx) # tail call

proc evalFunc(vm: VM, code: Code, rx: var VMValue): Code {.nimcall.} =
  template params(): expr = code.first
  template body(): expr = params.nxt
  vm.saveFrame
  result = code.nxt
  for i in params.first:
    result = eval (vm, result, vm.frame[vm.fp])
    inc vm.fp
  evalAll(vm, body.first, rx)
  vm.restoreFrame

#
# String conversions
#

proc toStringDefault(val: pointer): string =
  result = "[to-string not implemented]"

proc toStringInt(val: pointer): string = $(cast[int](val))

proc `$`*(val: VMValue): string {.inline.} = 
  val.typ.toString(val.data)

#
# Create VM
#

template defType(knd: TypeKind, ev: EvalProc, str: ToStringProc) =
  types[knd].eval = ev
  types[knd].kind = knd
  types[knd].toString = str

defType tkNone, evalConst, toStringDefault
defType tkInt, evalConst, toStringInt
defType tkWord, evalWord, toStringDefault
defType tkSetWord, evalSetWord, toStringDefault
defType tkGetWord, evalGetWord, toStringDefault
defType tkNative, evalNative, toStringDefault
defType tkFunc, evalFunc, toStringDefault

proc createVM*(): VM =
  result = create(RVM)
  result.heap = cast[Heap](alloc(HeapSize * sizeof HeapLayout))
  result.frame = cast[StackFrame](alloc(StackFrameSize * sizeof VMValue))
  result.stack = cast[Stack](alloc(StackSize * sizeof VMValue))

#
# Debug
#

import strutils

proc dumpHeap*(vm: VM, a, b: int) = 
  echo "Memory dump:"
  for i in a..b:
    let pi = cast[ptr array[4, int]](addr vm.heap[i])
    let s = "" # if pi[3] > 0 and pi[3] < 1000:  " (" & $Symbol(cast[pointer](pi[3])) & ")" else: ""
    echo toHex(cast[int](pi), 8), ": ", 
      toHex(pi[0], 8), " ", toHex(pi[1], 8), " - ", toHex(pi[2], 8), " ", toHex(pi[3], 8), s

proc dumpStack*(vm: VM) = 
  echo "Stack: bp: ", vm.fp 
  for i in 0..vm.fp:
    let pi = cast[ptr array[2, int]](addr vm.stack[i])
    echo i, ": ", toHex(pi[0], 8), " ", toHex(pi[1], 8) 

