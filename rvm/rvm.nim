

include sym

const 
  HeapSize = 1 shl 16
  StackSize = 1 shl 16

type
  EvalProc = proc (vm: VM, code: Code, rx: var VMValue): HeapSlot {.nimcall.}
  FindProc = proc (vm: VM, code: Code, s: Symbol): HeapSlot {.nimcall.}  
  BindProc = proc (vm: VM, code: Code, ctx: Code): HeapSlot {.nimcall.} 

  ToStringProc = proc(val: VMValue): string {.nimcall.}

  TypeKind = enum
    tkNone
    tkBool
    tkInt
    tkWord
    tkSetWord
    tkGetWord
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
    ax: HeapLayout # Register AX
    bx: HeapLayout # Register BX
    # fp: int           # current frame size
    # frame: StackFrame
    bp, sp: int
    stack: Stack 
    tail: int
    heap: Heap
    
    null: HeapSlot

var types: array[TypeKind, VMType]

#
# Primitive types representing correspondent 
#
type
  TNone = distinct int
  Word* = distinct Symbol
  SetWord* = distinct Symbol
  GetWord* = distinct Symbol
  Native* = proc (vm: VM, rx: var VMValue) {.nimcall.}
  Operation* = distinct Symbol
  BlockHead* = distinct HeapSlot
  ObjectHead* = distinct HeapSlot
  FuncHead* = distinct HeapSlot
  NativeHead* = distinct HeapSlot
  Paren* = distinct HeapSlot

  # this goes directly to VMValue.data
  Value* = TNone | bool | int | Word | SetWord | GetWord | Operation | Native | 
           BlockHead | ObjectHead | FuncHead | NativeHead | Paren

const 
  None* = TNone(0)

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

proc param*(vm: VM, offset: int): ptr VMValue {.inline.} =
  addr vm.stack[vm.bp + offset]

proc vmparam*[T: Value](vm: VM, offset: int): T {.inline.} =
  vmcast[T](vm.stack[vm.bp + offset])

template typeof(v: Value): expr =
  types[kind(type v)]

proc store*(val: var VMValue, v: Value) {.inline.} = 
  val.typ = addr typeof(v)
  val.data = cast[pointer](v) 

template isNil(slot: HeapSlot): expr = slot.nxt == nil

#
# Heap Operations
#

# template alloc*(vm: VM): expr = 
#   let slot = addr vm.heap[vm.tail]
#   inc vm.tail
#   slot

proc alloc*(vm: VM, v: Value): HeapSlot {.inline.} = 
  result = addr vm.heap[vm.tail]
  store result.val, v
  inc vm.tail

proc alloc*(vm: VM, s: Symbol): HeapSlot {.inline.} =
  result = vm.alloc None
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

proc push(vm: VM, val: Value) =
  store vm.stack[vm.sp], val
  inc vm.sp 

proc push(vm: VM, val: VMValue) =
  vm.stack[vm.sp] = val
  inc vm.sp 

proc pop(vm: VM): VMValue =
  dec vm.sp
  result = vm.stack[vm.sp]


#
# Eval
#

# bye bye tail calls
proc eval*(vm: VM, code: Code, ax: var VMValue): Code {.inline.} =
  result = code
  while true: 
    result = result.val.typ.eval(vm, result, ax)
    if result.val.typ.kind != tkOperation:
      break

proc evalAll*(vm: VM, code: BlockHead, rx: var VMValue) {.inline.} =
  var ip = HeapSlot(code)
  while not ip.isNil:
    ip = eval(vm, ip, rx)

proc evalConst(vm: VM, code: Code, rx: var VMValue): Code =
  # echo "const: ", code.val
  rx = code.val
  result = code.nxt

template getWord*(code: Code): expr = 
  if code.ext == nil:
   raiseScriptError errSymNotFound, code.val
  cast[HeapSlot](code.ext).val

proc evalWord(vm: VM, code: Code, rx: var VMValue): Code =
  # echo "word: ", code.val
  vm.ax.val = code.getWord() 
  vm.ax.nxt = code.nxt
  eval(vm, addr vm.ax, rx) 

proc evalGetWord(vm: VM, code: Code, rx: var VMValue): Code =
  # echo "get-word: ", code.val
  rx = code.getWord()
  result = code.nxt

proc evalSetWord(vm: VM, code: Code, rx: var VMValue): Code =
  # echo "set-word: ", code.val
  result = eval(vm, code.nxt, rx)
  code.getWord() = rx

proc evalOperation(vm: VM, code: Code, rx: var VMValue): Code =
  # echo "operation: ", code.val
  # TODO: potential problem with first arg to be re-evaluated twice  
  # we have to understand how to avoid resolutions to nulls
  # vm.ax.nxt = addr vm.bx
  # vm.ax.val = code.getWord() # op resolves to a function taking 2 arguments
  # vm.bx.nxt = code.nxt
  # vm.bx.val = rx
  # eval(vm, addr vm.ax, rx)
  let f = vmcast[Native](code.getWord())
  vm.push vm.bp
  vm.bp = vm.sp
  # assuming first val is in the RX, but we need to change this
  vm.push rx
  var second: VMValue
  result = eval(vm, code.nxt, second)
  vm.push second
  f(vm, rx)
  discard vm.pop
  discard vm.pop
  vm.bp = vmcast[int](vm.pop)

# proc evalNative(vm: VM, code: Code, rx: var VMValue): Code  =
#   # echo "native: ", code.val
#   (vmcast[Native](code.val))(vm, code.nxt, rx) # tail call

proc evalFunc(vm: VM, code: Code, rx: var VMValue): Code =
  let head = cast[HeapSlot](code.val.data)
  let params = vmcast[ObjectHead](head.val)
  let body = vmcast[BlockHead](head.nxt.val)

  result = code.nxt
  for i in params:
    result = eval (vm, result, i.val)
    vm.stack[vm.sp] = i.val
    inc vm.sp

  evalAll(vm, body, rx)

  for i in params:
    dec vm.sp
    i.val = vm.stack[vm.sp]

proc evalNative(vm: VM, code: Code, rx: var VMValue): Code =
  let head = cast[HeapSlot](code.val.data)
  let params = vmcast[ObjectHead](head.val)
  let impl = vmcast[Native](head.nxt.val)

  vm.push vm.bp 
  vm.bp = vm.sp

  result = code.nxt
  for i in params:
    var p: VMValue
    result = eval (vm, result, p)
    vm.push(p)

  impl(vm, rx)

  for i in params:
    discard vm.pop

  vm.bp = vmcast[int](vm.pop)

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

proc bnd(vm: VM, code: Code, ctx: Code): HeapSlot {.inline.} = 
  code.val.typ.bindProc(vm, code, ctx)

proc bindAll*(vm: VM, code: BlockHead, ctx: Code) {.inline.} =
  for i in code:
    let bnd = bnd(vm, i, ctx)
    if not bnd.isNil:
      i.ext = bnd

proc bindDefault(vm: VM, code: Code, ctx: Code): HeapSlot = vm.null

proc bindWord(vm: VM, code: Code, ctx: Code): HeapSlot = 
  find(vm, ctx, Symbol(code.val.data))

proc bindBlock(vm: VM, code: Code, ctx: Code): HeapSlot =
  result = vm.null
  vm.bindAll(vmcast[BlockHead](code.val), ctx)

proc expand(vm: VM, code: Code, ctx: Code) {.inline.} =
  let sym = Symbol(code.val.data)
  let slot = find(vm, ctx, sym)
  if slot.nxt == nil:
    slot.ext = cast[HeapSlot](sym)
    slot.nxt = vm.alloc(None)

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
defType tkWord, evalWord, toString[Word]
defType tkSetWord, evalSetWord, toString[SetWord]
defType tkGetWord, evalGetWord, toStringDefault
defType tkOperation, evalOperation, toString[Operation]
defType tkNative, evalConst, toStringDefault
defType tkBlock, evalConst, toString[BlockHead]
defType tkObject, evalConst, toString[ObjectHead]
defType tkFunc, evalFunc, toStringDefault
defType tkNativeFunc, evalNative, toStringDefault

types[tkObject].find = findObject

types[tkWord].bindProc = bindWord
types[tkSetWord].bindProc = bindWord
types[tkOperation].bindProc = bindWord
types[tkBlock].bindProc = bindBlock

proc createVM*(): VM =
  result = create(RVM)
  result.heap = cast[Heap](alloc(HeapSize * sizeof HeapLayout))
  #result.frame = cast[StackFrame](alloc(StackFrameSize * sizeof VMValue))
  result.stack = cast[Stack](alloc(StackSize * sizeof VMValue))
  result.null = result.alloc None

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
  echo "Stack: bp: ", vm.sp 
  for i in 0..vm.sp:
    let pi = cast[ptr array[2, int]](addr vm.stack[i])
    echo i, ": ", toHex(pi[0], 8), " ", toHex(pi[1], 8) 

