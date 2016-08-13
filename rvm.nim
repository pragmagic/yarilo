

include sym

const 
  HeapSize = 1 shl 16
  StackSize = 1 shl 16
  StackFrameSize = 256

type
  EvalProc = proc (vm: VM, code: Code, rx: var VMValue): HeapSlot {.nimcall.}
  FindProc = proc (vm: VM, code: Code, s: Symbol): HeapSlot {.nimcall.}  
  BindProc = proc (vm: VM, code: Code, ctx: Code): HeapSlot {.nimcall.} 

  ToStringProc = proc(val: VMValue): string {.nimcall.}

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
    bindProc: BindProc
    find: FindProc
    toString: ToStringProc

    kind: TypeKind
  
  VMValue* = object
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
    
    sys: HeapSlot

var types: array[TypeKind, VMType]

#
# Primitive types representing correspondent 
#
type
  TNone = distinct int
  Word* = distinct Symbol
  SetWord* = distinct Symbol
  GetWord = distinct Symbol
  Native* = EvalProc
  BlockHead* = distinct HeapSlot
  ObjectHead* = distinct HeapSlot
  FuncHead* = distinct HeapSlot

  # this goes directly to VMValue.data
  Value* = TNone | int | Word | SetWord | Native | BlockHead | ObjectHead | FuncHead

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
  elif T is Native:
    tkNative
  elif T is BlockHead:
    tkBlock
  elif T is ObjectHead:
    tkObject
  elif T is FuncHead:
    tkFunc
  else:
    {.fatal: "unhandled value type".}

proc vmcast*[T: Value](val: VMValue): T {.inline.} =
  assert val.typ.kind == kind(T)
  cast[T](val.data)

proc vmcast*[T: Value](slot: HeapSlot): T {.inline.} = vmcast[T](slot.val)

template typeof(v: Value): expr =
  types[kind(type v)]

template store*(val: var VMValue, v: Value) = 
  val.typ = addr typeof(v)
  val.data = cast[pointer](v) 

#
# Heap Operations
#

template alloc*(vm: VM): expr = 
  let slot = addr vm.heap[vm.tail]
  inc vm.tail
  slot

template alloc*(vm: VM, v: Value): expr = 
  let slot = addr vm.heap[vm.tail]
  store slot.val, v
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

iterator items*(s: BlockHead | ObjectHead): HeapSlot =
  var i = HeapSlot(s)
  while i.nxt != nil:
    yield i
    i = i.nxt 

#
# String conversions
#

import strutils

proc `$`*(val: VMValue): string {.inline.} = val.typ.toString(val)

proc toStringDefault(val: VMValue): string = 
  "{to-string not implemented: " & $val.typ.kind & "}"

proc `$`*(x: TNone): string {.inline.} = "none"

proc `$`*(w: Word): string {.inline.} = $Symbol(w)

proc `$`*(w: SetWord): string {.inline.} = $Symbol(w) & ":"

proc `$`*(blk: BlockHead): string =
  result = "["
  var i = cast[HeapSlot](blk)
  while true:
    result.add $i.val
    i = i.nxt
    if i.nxt == nil: 
      result.add ']'
      break
    result.add ' '

proc toString[T: Value](val: VMValue): string = $(vmcast[T](val)) 

proc `$`*(slot: HeapSlot): string {.inline.} = "{" & $slot.val & "}"

#
# Eval
#

template eval*(vm: VM, code: Code, ax: var VMValue): expr = code.val.typ.eval(vm, code, ax)

proc evalAll*(vm: VM, code: BlockHead, rx: var VMValue) {.inline.} =
  var ip = HeapSlot(code)
  while ip.nxt != nil:
    ip = eval(vm, ip, rx)

proc evalConst(vm: VM, code: Code, rx: var VMValue): Code =
  rx = code.val
  result = code.nxt

proc evalWord(vm: VM, code: Code, rx: var VMValue): Code =
  vm.ax = cast[HeapSlot](code.ext).val
  vm.ip = code.nxt
  eval(vm, cast[HeapSlot](vm), rx) # tail call

proc evalGetWord(vm: VM, code: Code, rx: var VMValue): Code =
  rx = cast[HeapSlot](code.ext).val
  result = code.nxt

proc evalSetWord(vm: VM, code: Code, rx: var VMValue): Code =
  eval(vm, code.nxt, cast[HeapSlot](code.ext).val) # tail call

proc evalNative(vm: VM, code: Code, rx: var VMValue): Code  =
  (cast[EvalProc](code.val.data))(vm, code.nxt, rx) # tail call

proc evalFunc(vm: VM, code: Code, rx: var VMValue): Code =
  template first(): expr = cast[HeapSlot](code.val.data)
  template params(): expr = vmcast[ObjectHead](first())
  template body(): expr = vmcast[BlockHead](first.nxt)
  #vm.saveFrame
  result = code.nxt
  for i in params:
    result = eval (vm, result, i.val)
#    inc vm.fp
  evalAll(vm, body, rx)
  #vm.restoreFrame

proc eval*(vm: VM, code: BlockHead): VMValue {.inline.} = 
  evalAll(vm, code, result)

#
# Find - find symbol in structure
#

template find*(vm: VM, code: Code, s: Symbol): expr =
  code.val.typ.find(vm, code, s)  

proc findDefault(vm: VM, code: Code, s: Symbol): HeapSlot =
  raiseScriptError errMethodNotAllowed

proc findObject(vm: VM, code: Code, s: Symbol): HeapSlot =
  result = HeapSlot(vmcast[ObjectHead](code))
  while true:
    if result.ext == cast[HeapSlot](s):
      break
    if result.nxt == nil:
      break
    result = result.nxt

#
# Bindings
#

template bnd*(vm: VM, code: Code, ctx: Code): expr =
  code.val.typ.bindProc(vm, code, ctx)  

proc bindDefault(vm: VM, code: Code, ctx: Code): HeapSlot = discard

proc bindWord(vm: VM, code: Code, ctx: Code): HeapSlot =
  template sym(): expr = Symbol(code.val.data)
  result = find(vm, ctx, sym)
  if result.nxt == nil:
    result = find(vm, vm.sys, sym)
    if result.nxt == nil:
      raiseScriptError errSymNotFound

proc expand(vm: VM, code: Code, ctx: Code) {.inline.} =
  template sym(): expr = Symbol(code.val.data)
  var slot = find(vm, ctx, sym)
  if slot.nxt == nil:
    let sysSlot = find(vm, vm.sys, sym)
    if sysSlot.nxt == nil:
      slot.ext = cast[HeapSlot](sym)
      slot.nxt = vm.alloc()
      echo toHex(cast[int](slot))

proc bindAll*(vm: VM, code: BlockHead, ctx: Code) {.inline.} =
  for i in code:
    i.ext = bnd(vm, i, ctx)

proc expandAll*(vm: VM, code: BlockHead, ctx: Code) {.inline.} =
  for i in code:
    if i.val.typ.kind == tkSetWord:
      expand(vm, i, ctx)
        
#
# Create VM
#

template defType(knd: TypeKind, ev: EvalProc, str: ToStringProc) =
  types[knd].eval = ev
  types[knd].find = findDefault
  types[knd].bindProc = bindDefault
  types[knd].kind = knd
  types[knd].toString = str

defType tkNone, evalConst, toString[TNone]
defType tkInt, evalConst, toString[int]
defType tkWord, evalWord, toString[Word]
defType tkSetWord, evalSetWord, toString[SetWord]
defType tkGetWord, evalGetWord, toStringDefault
defType tkNative, evalNative, toStringDefault
defType tkBlock, evalConst, toString[BlockHead]
defType tkObject, evalConst, toStringDefault
defType tkFunc, evalFunc, toStringDefault

types[tkObject].find = findObject

types[tkWord].bindProc = bindWord
types[tkSetWord].bindProc = bindWord

proc createVM*(): VM =
  result = create(RVM)
  result.heap = cast[Heap](alloc(HeapSize * sizeof HeapLayout))
  result.frame = cast[StackFrame](alloc(StackFrameSize * sizeof VMValue))
  result.stack = cast[Stack](alloc(StackSize * sizeof VMValue))

proc bootstrap*(vm: VM, natives: ObjectHead) =
  vm.sys = vm.alloc
  vm.sys.val.store natives

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

