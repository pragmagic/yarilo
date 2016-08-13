

include sym

const 
  HeapSize = 1 shl 16
  StackSize = 1 shl 16
  StackFrameSize = 256

type
  EvalProc = proc(vm: VM, code: Code, rx: var VMValue): Code {.nimcall.}
  FindProc = proc (vm: VM, code: Code, s: Symbol, create: bool): HeapSlot {.nimcall.}  
  ToStringProc = proc(code: Code): string {.nimcall.}

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
    find: FindProc
    toString: ToStringProc
    # properties
    createWhenBind: bool
    kind: TypeKind
  
  VMValue = object
    typ: PType
    data: pointer

  HeapSlot* = ptr HeapLayout
  HeapLayout = object
    val: VMValue
    nxt: HeapSlot
    ext: HeapSlot # sometimes points to Symbol

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
# Primitive types representing correspondent 
#
type
  TNone = distinct int
  Word* = distinct Symbol
  SetWord* = distinct Symbol
  GetWord = distinct Symbol
  BlockHead* = distinct HeapSlot
  ObjectHead* = distinct HeapSlot

  # this goes directly to VMValue.data
  Value* = TNone | int | Word | SetWord | BlockHead | ObjectHead

const 
  None* = TNone(0)

#
# Accessors, Adapters, and Converters
#

template kind(s: HeapSlot): expr = s.val.typ.kind

template kind(T: typedesc[Value]): expr = 
  when T is TNone:
    tkNone
  elif T is int:
    tkInt
  elif T is Word:
    tkWord
  elif T is SetWord:
    tkSetWord
  elif T is BlockHead:
    tkBlock
  elif T is ObjectHead:
    tkObject
  else:
    {.fatal: "unhandled value type".}

template typeof(v: Value): expr =
  types[kind(type v)]

proc vmcast*[T: Value](slot: HeapSlot): T {.inline.} =
  assert slot.val.typ.kind == kind(T)
  cast[T](slot.val.data)

#
# Heap Operations
#

template alloc*(vm: VM): expr = 
  let slot = addr vm.heap[vm.tail]
  inc vm.tail
  slot

template alloc(vm: VM, s: Symbol): expr =
  let slot = addr vm.heap[vm.tail]
  slot.ext = cast[HeapSlot](s)  
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

#template first(s: HeapSlot): expr = cast[HeapSlot](s.val.data) 

iterator items(s: BlockHead | ObjectHead): HeapSlot =
  var i = HeapSlot(s)
  while i.nxt != nil:
    yield i
    i = i.nxt 

#
# String conversions
#

proc `$`*(slot: HeapSlot): string {.inline.} = 
  slot.val.typ.toString(slot)

proc toStringDefault(code: Code): string =
  result = "[to-string not implemented]"

proc `$`*(w: Word): string {.inline.} =
  $Symbol(w)

proc `$`*(blk: BlockHead): string =
  result = "["
  var i = cast[HeapSlot](blk)
  while true:
    result.add $i
    i = i.nxt
    if i.nxt == nil: 
      result.add ']'
      break
    result.add ' '

proc toString[T: Value](code: Code): string = $(vmcast[T](code)) 

#
# Eval
#

template eval(vm: VM, code: Code, ax: var VMValue): expr = code.val.typ.eval(vm, code, ax)

proc evalAll*(vm: VM, code: BlockHead, rx: var VMValue) {.inline.} =
  var ip = HeapSlot(code)
  while ip.nxt != nil:
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

proc evalFunc(vm: VM, code: Code, rx: var VMValue): Code =
  template params(): expr = vmcast[BlockHead](code)
  template body(): expr = vmcast[BlockHead](HeapSlot(params).nxt)
  vm.saveFrame
  result = code.nxt
  for i in params:
    result = eval (vm, result, vm.frame[vm.fp])
    inc vm.fp
  evalAll(vm, body, rx)
  vm.restoreFrame

proc eval*(vm: VM, code: BlockHead): VMValue {.inline.} = 
  evalAll(vm, code, result)

#
# Find - find symbol in structure
#

template find*(vm: VM, code: Code, s: Symbol, create: bool): expr =
  code.val.typ.find(vm, code, s, create)  

proc findDefault(vm: VM, code: Code, s: Symbol, create: bool): HeapSlot =
  raiseScriptError errMethodNotAllowed

proc findObject(vm: VM, code: Code, s: Symbol, create: bool): HeapSlot =
  result = HeapSlot(vmcast[ObjectHead](code))
  while true:
    if result.ext == cast[HeapSlot](s):
      break
    if result.nxt == nil:
      if create:
        result.nxt = vm.alloc(s)
        result = result.nxt
        break
      else:
        raiseScriptError errSymNotFound
        break
    result = result.nxt

#
# Bindings
#

proc bindAll*(vm: VM, code: BlockHead, ctx: Code) {.inline.} =
  for i in code:
    i.ext = find(vm, ctx, Symbol(i.val.data), i.val.typ.createWhenBind)

#
# Create VM
#

template defType(knd: TypeKind, ev: EvalProc, str: ToStringProc) =
  types[knd].eval = ev
  types[knd].find = findDefault
  types[knd].kind = knd
  types[knd].toString = str

defType tkNone, evalConst, toStringDefault
defType tkInt, evalConst, toString[int]
defType tkWord, evalWord, toString[Word]
defType tkSetWord, evalSetWord, toStringDefault
defType tkGetWord, evalGetWord, toStringDefault
defType tkNative, evalNative, toStringDefault
defType tkBlock, evalConst, toString[BlockHead]
defType tkObject, evalConst, toStringDefault
defType tkFunc, evalFunc, toStringDefault

types[tkSetWord].createWhenBind = true
types[tkObject].find = findObject

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

