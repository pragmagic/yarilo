
import err

const 
  MaxSymbols = 65536
  mask = MaxSymbols - 1
  TextSize = 65536

type
  Symbol* = distinct pointer

  TabArray = ptr array[MaxSymbols, cstring]

  SymTab = object
    table: TabArray
    count: int
    syms: cstring
    last: int

proc javaHash(s: cstring): int {.inline.} = 
  result = 0
  var i = 0
  while true:
    let c = ord(s[i])
    if c == 0: break
    result = 31 * result + c
    inc i

proc initSymtab(table: var SymTab) =
  table.table = cast[TabArray](alloc(MaxSymbols * sizeof(cstring)))
  table.syms = cast[cstring](alloc(TextSize))
  table.last = 1

proc findOrInsert(table: var SymTab, key: cstring, size: int): cstring =
  var probes = 0
  var idx = javaHash(key) and mask
  while true:
    if table.table[idx] == nil:
      if table.count >= MaxSymbols or table.last + size > TextSize: 
        raiseScriptError fatalSymtableOutOfSpace
      let cstr = addr table.syms[table.last]
      copyMem(cstr, key, size)
      table.table[idx] = cstr
      inc table.last, size
      break
    elif table.table[idx] == key:
      break
    inc probes
    idx = (idx + probes) and mask
  result = table.table[idx]

# type 
#   Word* = distinct int

#   WordKind* = enum
#     wkNormal
#     wkGetWord
#     wkSetWord

#template word*(s: Symbol, wk: WordKind): expr = Word(int(wk) or (int(s) shl 8))
#template sym*(w: Word): expr = Symbol(int(w) shr 8)
#template kind*(w: Word): expr = WordKind(int(w) and 0xff)

var symtab: SymTab 
initSymtab(symtab)

proc `!`*(s: string): Symbol {.inline.} = 
  Symbol(symtab.findOrInsert(cstring(s), s.len + 1)) 
proc `$`*(s: Symbol): string {.inline.} = $cast[cstring](s) 

#proc `==`*(a, b: Symbol): bool {.inline.} = int(a) == int(b) 

#proc `$`*(w: Word): string {.inline.} = $w.sym
