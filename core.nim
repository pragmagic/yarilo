
include rvm


template sys(vm: VM): expr = ObjectHead(vm.sysMod)

#
# Adapters
#


template store*(val: var VMValue, v: Value) = 
  val.typ = addr typeof(v)
  val.data = cast[pointer](v) 

#
# Object Ops
#

# proc findSymbol(obj: ObjectHead, s: Symbol): HeapSlot {.inline.} =
#   var i = HeapSlot(obj)
#   while i != nil:
#     if i.ext == cast[pointer](s):
#       return i
#     i = i.nxt   

# proc findSymbol(obj: ObjectHead, s: Symbol, failover: ObjectHead): HeapSlot =
#   result = obj.findSymbol(s)
#   if result == nil:
#     result = failover.findSymbol(s)

  
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

# template find(res: var pointer, obj: ObjectHead, s: Symbol, notFound: stmt) =
#   res = HeapSlot(obj)
#   while true:
#     if res.ext == cast[HeapSlot](s):
#       break
#     if res.nxt == nil:
#       notFound
#       break
#     else:
#       res = res.nxt

# type
#   BindProc = proc (vm: VM, code: Code, ctx: Code) {.nimcall.}


# proc bindGen(vm: VM, code: Code, ctx: Code) =
#   var i = code
#   while i != nil:
#     i.ext = (ctx.val.typ.find)(vm, ctx, Symbol(code.val.data), code.val.typ.createWhenBind)
#     i = i.nxt

# proc bindWord(vm: VM, code: Code, ctx: Code) =
#   template res(): expr = code.ext
#   template sym(): expr = Symbol(code.val.data) 
#   find(res, cast[ObjectHead](ctx.val.data), sym):
#     raiseScriptError errSymNotFound

# proc bindSetWord(vm: VM, code: Code, ctx: ObjectHead) =
#   template res(): expr = code.ext
#   template sym(): expr = Symbol(code.val.data) 
#   find(res, ctx, sym):

# var 
#   a, b: BindProc
#   c: FindProc

# a = bindWord
# b = bindGen
# c = findObject

# proc bindAndExtend*(vm: VM, blk: BlockHead, ctx: var ObjectHead) = 
#   for i in blk:
#     if i.kind == tkWord or i.kind == tkSetWord:
#       let sym = Symbol(i.val.data)
#       var bound: HeapSlot
#       bound.find(ctx, sym):        
#         if i.kind == tkWord:
#           raiseScriptError errSymNotFound
#         else:
#           bound.nxt = vm.alloc(sym)
          

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


#
# String Ops
#


#
# Builders
#

type
  BlockBuilder* = object
    self: HeapSlot
    tail: HeapSlot

  ObjectBuilder* = object
    self: HeapSlot
    tail: HeapSlot

#
# Block Builder
#

proc makeBlock*(vm: VM): BlockBuilder =
  result.self = vm.alloc()
  result.tail = vm.alloc()
  result.self.val.store BlockHead(result.tail)

converter toBlock*(builder: BlockBuilder): BlockHead {.inline.} = 
  vmcast[BlockHead](builder.self)

converter toSelf*(builder: BlockBuilder): HeapSlot {.inline.} = 
  builder.self

proc add*(vm: VM, blk: var BlockBuilder, v: Value) {.inline.} =
  store blk.tail.val, v
  let slot = vm.alloc()
  blk.tail.nxt = slot
  blk.tail = slot

#
# Object Builder
#

proc makeObject*(vm: VM): ObjectBuilder =
  result.self = vm.alloc()
  result.tail = vm.alloc()
  result.self.val.store ObjectHead(result.tail)

converter toObject*(builder: ObjectBuilder): ObjectHead {.inline.} = 
  vmcast[ObjectHead](builder.self)

converter toSelf*(builder: ObjectBuilder): HeapSlot {.inline.} = 
  builder.self

proc add*(vm: VM, obj: var BlockBuilder, s: Symbol) {.inline.} =
  obj.tail.ext = cast[HeapSlot](s)
  let slot = vm.alloc()
  obj.tail.nxt = slot
  obj.tail = slot

when isMainModule:
  var vm = createVM()
  var builder = makeBlock(vm)
  vm.add builder, 777
  vm.dumpHeap(0, 20)
