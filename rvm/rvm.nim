

include sym

const 
  HeapSize = 1 shl 16
  StackSize = 1 shl 16

type
  EvalProc = proc (vm: VM, code: Code): HeapSlot {.nimcall.}
  FindProc = proc (vm: VM, code: Code, s: Symbol): HeapSlot {.nimcall.}  
  BindProc = proc (vm: VM, code: Code, ctx: Code) {.nimcall.} 

  ToStringProc = proc(val: VMValue): string {.nimcall.}

  TypeKind = enum
    tkNone
    tkBool
    tkInt
    tkWord
    tkSetWord
    tkGetWord
    tkBoundWord
    tkBoundSetWord
    tkBoundGetWord
    tkOperation
    tkNative
    tkBlock
    tkObject
    tkFunc
    tkNativeFunc

  PType = ptr VMType
  VMType = object
    eval: EvalProc
    bindProc: BindProc
    find: FindProc
    toString: ToStringProc

    kind: TypeKind
  
  PValue = ptr VMValue
  VMValue* = object
    typ: PType
    data: pointer

  HeapSlot* = ptr HeapLayout
  HeapLayout = object
    nxt: HeapSlot
    val: VMValue
    ext: HeapSlot # sometimes points to Symbol

  Heap = ptr array[HeapSize, HeapLayout]
  Stack = ptr array[StackSize, VMValue]
  # StackFrame = ptr array[StackFrameSize, VMValue]

  Code = HeapSlot

  VM* = ptr RVM
  RVM = object
    rx: VMValue
    ax: HeapLayout # Register AX
    bx: HeapLayout # Register BX
    # fp: int           # current frame size
    # frame: StackFrame
    sp: int
    stack: Stack 
    tail: int
    heap: Heap
    
    null: HeapSlot
    none: VMValue

var types: array[TypeKind, VMType]

#
# Primitive types representing correspondent 
#
type
  TNone = distinct int
  Word* = distinct Symbol
  SetWord* = distinct Symbol
  GetWord* = distinct Symbol
  BoundWord* = distinct PValue 
  BoundSetWord* = distinct PValue
  BoundGetWord* = distinct PValue
  Native* = proc (vm: VM) {.nimcall.}
  Operation* = distinct Symbol
  BlockHead* = distinct HeapSlot
  ObjectHead* = distinct HeapSlot
  FuncHead* = distinct HeapSlot
  NativeHead* = distinct HeapSlot
  Paren* = distinct HeapSlot

  # this goes directly to VMValue.data
  Value* = TNone | bool | int | Word | SetWord | GetWord | Operation | Native | 
           BlockHead | ObjectHead | FuncHead | NativeHead | Paren

# const 
#   None* = TNone(0)

#
# Accessors, Adapters, and Converters
#

proc kind*(s: HeapSlot): TypeKind {.inline.} = s.val.typ.kind

proc next*(s: HeapSlot): HeapSlot {.inline.} = s.nxt

template kind(T: typedesc[Value]): expr = 
  when T is TNone:
    tkNone
  elif T is int:
    tkInt
  elif T is bool:
    tkBool
  elif T is Word:
    tkWord
  elif T is SetWord:
    tkSetWord
  elif T is Operation:
    tkOperation
  elif T is Native:
    tkNative
  elif T is BlockHead:
    tkBlock
  elif T is ObjectHead:
    tkObject
  elif T is FuncHead:
    tkFunc
  elif T is NativeHead:
    tkNativeFunc
  else:
    {.fatal: "unhandled value type".}

proc vmcast*[T: Value](val: VMValue): T {.inline.} =
  assert val.typ.kind == kind(T), "actual: " & $val.typ.kind
  cast[T](val.data)

proc param*(vm: VM, offset: int): var VMValue {.inline.} =
  vm.stack[vm.sp + offset]

proc vmparam*[T: Value](vm: VM, offset: int): T {.inline.} =
  vmcast[T](vm.stack[vm.sp + offset])

template typeof(v: Value): expr =
  types[kind(type v)]

proc store*(val: var VMValue, v: Value) {.inline.} = 
  val.typ = addr typeof(v)
  val.data = cast[pointer](v) 

proc isNil(slot: HeapSlot): bool {.inline.} = slot.nxt == nil

#
# Heap Operations
#

# template alloc*(vm: VM): expr = 
#   let slot = addr vm.heap[vm.tail]
#   inc vm.tail
#   slot

proc alloc*(vm: VM): HeapSlot {.inline.} = 
  result = addr vm.heap[vm.tail]
  result.val = vm.none
  inc vm.tail

proc alloc*(vm: VM, v: Value): HeapSlot {.inline.} = 
  result = addr vm.heap[vm.tail]
  store result.val, v
  inc vm.tail

proc alloc*(vm: VM, s: Symbol): HeapSlot {.inline.} =
  result = vm.alloc
  result.ext = cast[HeapSlot](s)  

#
# List Ops
#

#template first(s: HeapSlot): expr = cast[HeapSlot](s.val.data) 

iterator items*(s: BlockHead | ObjectHead): HeapSlot =
  var i = HeapSlot(s)
  while not isNil i:
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
proc `$`*(w: Operation): string {.inline.} = $Symbol(w)

proc `$`*(blk: BlockHead): string =
  result = "["
  var i = cast[HeapSlot](blk)
  while true:
    result.add $i.val
    i = i.nxt
    if i.isNil: 
      result.add ']'
      break
    result.add ' '

proc `$`*(blk: ObjectHead): string =
  result = "["
  var i = cast[HeapSlot](blk)
  while true:
    result.add $cast[Symbol](i.ext)
    result.add ": "
    result.add toHex(cast[int](i.val.typ))
    i = i.nxt
    if i.isNil: 
      result.add ']'
      break
    result.add ' '

proc toString[T: Value](val: VMValue): string = $(vmcast[T](val)) 

proc `$`*(slot: HeapSlot): string {.inline.} = 
  result = "@ "
  result.add toHex(cast[int](slot), 8) 
  result.add "<next: "
  result.add toHex(cast[int](slot.nxt), 8)
  result.add " kind: "
  result.add $slot.val.typ.kind
  result.add ">"

#
# Errors
#

proc raiseScriptError*(code: ErrCode, val: VMValue) {.noinline.} =
  raise newException(Exception, txt[code] % $val)

#
# Stack
#
# Stack Frame:
# .. [BP] [RX] [P1] [P2] .. [Pn] [BP] [RX] [P1] [P2] .. [Pn] [BP] <- BP [RX] <- SP - native calls
# .. [BP] [RX] [P1] [P2] .. [Pn] [BP] [OP1] [OP2] 


proc push*(vm: VM, val: Value) {.inline.} =
  inc vm.sp 
  store vm.stack[vm.sp], val

proc push*(vm: VM, val: VMValue) {.inline.} =
  inc vm.sp 
  vm.stack[vm.sp] = val

proc push*(vm: VM) {.inline.} =
  inc vm.sp 
  vm.stack[vm.sp] = vm.none

proc result*(vm: VM, val: Value) {.inline.} =
  store vm.rx, val

proc result*(vm: VM, val: VMValue) {.inline.} =
  vm.rx = val

proc result*(vm: VM): var VMValue {.inline.} =
  vm.rx

# proc top*(vm: VM): var VMValue {.inline.} = 
#   vm.stack[vm.sp]

proc pop(vm: VM, val: var VMValue) {.inline.} =
  val = vm.stack[vm.sp]
  dec vm.sp

proc pop(vm: VM) {.inline.} =
  dec vm.sp

#
# Eval
#

proc eval*(vm: VM, code: Code): Code {.inline.} =
  result = code.val.typ.eval(vm, code)
  if result.val.typ.kind == tkOperation:
      result = result.val.typ.eval(vm, result)

proc evalAll*(vm: VM, code: BlockHead) {.inline.} =
  var ip = HeapSlot(code)
  while not ip.isNil:
    ip = eval(vm, ip)

proc evalConst(vm: VM, code: Code): Code =
  vm.rx = code.val
  result = code.nxt

proc evalUnbound(vm: VM, code: Code): Code =
  raiseScriptError errSymNotFound, code.val

template getWord*(code: Code): expr = 
  cast[HeapSlot](code.ext).val

proc evalWord(vm: VM, code: Code): Code =
  vm.ax.val = code.getWord() 
  vm.ax.nxt = code.nxt
  eval(vm, addr vm.ax) 

proc evalGetWord(vm: VM, code: Code): Code =
  vm.rx = code.getWord()
  result = code.nxt

proc evalSetWord(vm: VM, code: Code): Code =
  result = eval(vm, code.nxt)
  code.getWord() = vm.rx

proc evalOperation(vm: VM, code: Code): Code =
  vm.push vm.rx 
  result = eval(vm, code.nxt)
  vm.push vm.rx
  (vmcast[Native](code.getWord()))(vm)
  vm.pop 
  vm.pop
  
proc evalFunc(vm: VM, code: Code): Code =
  let head = cast[HeapSlot](code.val.data)
  let params = vmcast[ObjectHead](head.val)
  let body = vmcast[BlockHead](head.nxt.val)

  result = code.nxt
  for i in params:
    vm.push i.val
    result = eval (vm, result)
    i.val = vm.rx

  evalAll(vm, body)

  for i in params:
    vm.pop i.val


proc evalNative(vm: VM, code: Code): Code =
  let head = cast[HeapSlot](code.val.data)
  let params = vmcast[ObjectHead](head.val)
  let impl = vmcast[Native](head.nxt.val)

  result = code.nxt
  for i in params:
    result = eval (vm, result)
    vm.push vm.rx 

  impl(vm)

  for i in params:
    vm.pop

proc eval*(vm: VM, code: BlockHead) {.inline.} = 
  evalAll(vm, code)

#
# Find - find symbol in structure
#

template find*(vm: VM, code: Code, s: Symbol): expr =
  code.val.typ.find(vm, code, s)  

proc findDefault(vm: VM, code: Code, s: Symbol): HeapSlot =
  raiseScriptError errMethodNotAllowed

proc findObject(vm: VM, code: Code, s: Symbol): HeapSlot =
  result = HeapSlot(vmcast[ObjectHead](code.val))
  while true:
    if result.isNil:
      break
    if result.ext == cast[HeapSlot](s):
      break
    result = result.nxt

#
# Bindings
#

proc bnd(vm: VM, code: Code, ctx: Code) {.inline.} = 
  code.val.typ.bindProc(vm, code, ctx)

proc bindAll*(vm: VM, code: BlockHead, ctx: Code) {.inline.} =
  for i in code:
    bnd(vm, i, ctx)

proc bindDefault(vm: VM, code: Code, ctx: Code) = discard

proc bindWord(vm: VM, code: Code, ctx: Code) = 
  let bnd = find(vm, ctx, Symbol(code.val.data))
  if not bnd.isNil:
    code.ext = bnd
    code.val.typ = addr types[tkBoundWord]

proc bindSetWord(vm: VM, code: Code, ctx: Code) = 
  let bnd = find(vm, ctx, Symbol(code.val.data))
  if not bnd.isNil:
    code.ext = bnd
    code.val.typ = addr types[tkBoundSetWord]

proc bindGetWord(vm: VM, code: Code, ctx: Code) = 
  let bnd = find(vm, ctx, Symbol(code.val.data))
  if not bnd.isNil:
    code.ext = bnd
    code.val.typ = addr types[tkBoundGetWord]

proc bindOperation(vm: VM, code: Code, ctx: Code) = 
  let bnd = find(vm, ctx, Symbol(code.val.data))
  if not bnd.isNil:
    code.ext = bnd

proc bindBlock(vm: VM, code: Code, ctx: Code) =
  vm.bindAll(vmcast[BlockHead](code.val), ctx)

proc expand(vm: VM, code: Code, ctx: Code) {.inline.} =
  let sym = Symbol(code.val.data)
  let slot = find(vm, ctx, sym)
  if slot.nxt == nil:
    slot.ext = cast[HeapSlot](sym)
    slot.nxt = vm.alloc()

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
defType tkBool, evalConst, toString[bool]
defType tkInt, evalConst, toString[int]
defType tkWord, evalUnbound, toString[Word]
defType tkSetWord, evalUnbound, toString[SetWord]
defType tkGetWord, evalUnbound, toStringDefault
defType tkBoundWord, evalWord, toString[Word]
defType tkBoundSetWord, evalSetWord, toString[SetWord]
defType tkBoundGetWord, evalGetWord, toStringDefault
defType tkOperation, evalOperation, toString[Operation]
defType tkNative, evalConst, toStringDefault
defType tkBlock, evalConst, toString[BlockHead]
defType tkObject, evalConst, toString[ObjectHead]
defType tkFunc, evalFunc, toStringDefault
defType tkNativeFunc, evalNative, toStringDefault

types[tkObject].find = findObject

types[tkWord].bindProc = bindWord
types[tkSetWord].bindProc = bindSetWord
types[tkBoundWord].bindProc = bindWord
types[tkBoundSetWord].bindProc = bindSetWord
types[tkBlock].bindProc = bindBlock

types[tkOperation].bindProc = bindOperation

proc createVM*(): VM =
  result = create(RVM)
  result.heap = cast[Heap](alloc(HeapSize * sizeof HeapLayout))
  #result.frame = cast[StackFrame](alloc(StackFrameSize * sizeof VMValue))
  result.stack = cast[Stack](alloc(StackSize * sizeof VMValue))
  result.sp = -1
  store result.none, TNone(0)
  result.null = result.alloc 

# proc bootstrap*(vm: VM, natives: ObjectHead) =
#   vm.sys = vm.alloc(natives)

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
  echo "SP = ", vm.sp 
  for i in 0..<vm.sp:
    echo vm.stack[i]

