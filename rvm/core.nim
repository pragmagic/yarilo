
include rvm


template sys(vm: VM): expr = ObjectHead(vm.sysMod)

#
# Adapters
#

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
    head: HeapSlot
    tail: HeapSlot

  ObjectBuilder* = object
    head*: HeapSlot
    tail: HeapSlot

#
# Block Builder
#

proc makeBlock*(vm: VM): BlockBuilder =
  result.head = vm.alloc(None)
  result.tail = result.head

converter toBlock*(builder: BlockBuilder): BlockHead {.inline.} = 
  BlockHead(builder.head)

proc add*(vm: VM, blk: var BlockBuilder, v: Value) {.inline.} =
  store blk.tail.val, v
  let slot = vm.alloc(None)
  blk.tail.nxt = slot
  blk.tail = slot

#
# Object Builder
#

proc makeObject*(vm: VM): ObjectBuilder =
  result.head = vm.alloc(None)
  result.tail = result.head

converter toObject*(builder: ObjectBuilder): ObjectHead {.inline.} = 
  ObjectHead(builder.head)

proc add*(vm: VM, obj: var ObjectBuilder, s: Symbol, val: Value) {.inline.} =
  obj.tail.ext = cast[HeapSlot](s)
  store obj.tail.val, val
  let slot = vm.alloc(None)
  obj.tail.nxt = slot
  obj.tail = slot

#
# Func
#

proc locals(vm: VM, params: BlockHead): ObjectHead =
  var builder = vm.makeObject
  for p in params:
    let w = vmcast[Word](p.val)
    vm.add builder, Symbol(w), None 
  result = builder

proc makeFunc*(vm: VM, params: BlockHead, impl: BlockHead): FuncHead =
  let localCtx = vm.locals params
  let head = vm.alloc localCtx
  let body = vm.alloc impl
  let tail = vm.alloc None
  head.nxt = body
  body.nxt = tail
  vm.bindAll(impl, head)
  result = FuncHead(head)

proc makeFunc*(vm: VM, params: BlockHead, impl: Native): NativeHead =
  let localCtx = vm.locals params
  let head = vm.alloc localCtx
  let body = vm.alloc impl
  let tail = vm.alloc None
  head.nxt = body
  body.nxt = tail
  result = NativeHead(head)  

when isMainModule:
  var vm = createVM()
  var builder = makeBlock(vm)
  vm.add builder, 777
  vm.dumpHeap(0, 20)
