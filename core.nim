
include rvm

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
  Value* = TNone | int | Word | SetWord | BlockHead

const 
  None* = TNone(0)

template sys(vm: VM): expr = ObjectHead(vm.sysMod)

#
# Adapters
#

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
  else:
    quit("unhandled value type")

template typeof(v: Value): expr =
  types[kind(type v)]

template store*(val: var VMValue, v: Value) = 
  val.typ = addr typeof(v)
  val.data = cast[pointer](v) 

converter toSlot(s: BlockHead): HeapSlot {.inline.} = HeapSlot(s)
converter toSlot(s: ObjectHead): HeapSlot {.inline.} = HeapSlot(s)

#
# Object Ops
#

proc findSymbol(obj: ObjectHead, s: Symbol): HeapSlot {.inline.} =
  var i = HeapSlot(obj)
  while i != nil:
    if i.ext == cast[pointer](s):
      return i
    i = i.nxt   

proc findSymbol(obj: ObjectHead, s: Symbol, failover: ObjectHead): HeapSlot =
  result = obj.findSymbol(s)
  if result == nil:
    result = failover.findSymbol(s)

template find(res: expr, obj: ObjectHead, s: Symbol, notFound: stmt) =
  res = obj
  while true:
    if res.ext == cast[pointer](s):
      break
    if res.nxt == nil:
      notFound
  
# proc findOrAddSymbol(vm: VM, obj: var ObjectHead, s: Symbol): HeapSlot =
#   var i = HeapSlot(obj)
#   if i == nil:
#     result = findSymbol(vm.sys, s)
#     if result == nil:
#       result = vm.alloc(s)
#     obj = ObjectHead(result)
#   else: 
#     while true:
#       if i.ext == cast[pointer](s):
#         return i
#       if i.nxt == nil:
#         result = findSymbol(vm.sys, s)
#         if result == nil:
#           result = vm.alloc(s)
#           i.nxt = result
#       else:
#         i = i.nxt

#
# Bindings
#

proc bindWord(code: Code, ctx: ObjectHead) =
  findSymbol(ctx, Symbol(code.val.data))

proc bindAndExtend*(vm: VM, blk: BlockHead, ctx: var ObjectHead) = 
  for i in blk:
    if i.kind == tkWord or i.kind == tkSetWord:
      let sym = Symbol(i.val.data)
      var bound: HeapSlot
      bound.find(ctx, sym):        
        if i.kind == tkWord:
          raiseScriptError errSymNotFound
        else:
          bound.nxt = vm.alloc(sym)
          

# proc bindToContext*(vm: VM, blk: BlockHead, ctx: ObjectHead) =
#   for i in blk:
#     if i.kind == tkWord:
#       i.ext = findSymbol(ctx, Symbol(i.val.data))
#     elif i.kind == toSetWord:
#         vs = findSymbol(vm.sys, Symbol(i.val.data))
#         if vs == nil:
#           raiseScriptError errSymNotFound

#
# Eval
#

proc eval*(vm: VM, code: BlockHead): VMValue {.inline.} = 
  evalAll(vm, HeapSlot(code), result)

#
# String Ops
#

proc `$`*(val: Value): string {.inline.} =
  typeof(v).toString(cast[pointer](val))

#
# Builders
#

type
  BlockBuilder* = object
    head: BlockHead
    tail: HeapSlot

  ObjectBuilder* = object
    head: ObjectHead
    tail: HeapSlot


#
# Block Builder
#

converter toBlock*(bb: BlockBuilder): BlockHead {.inline.} = bb.head

template add*(vm: VM, blk: var BlockBuilder, v: Value)  =
  let slot = vm.alloc()
  store(slot.val, v)
  if blk.tail == nil:
    blk.head = BlockHead(slot)
  else:
    blk.tail.nxt = slot
  blk.tail = slot
