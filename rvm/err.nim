
type
  ErrCode* = enum
    errMethodNotAllowed
    errSymNotFound
    fatalSymtableOutOfSpace

const
  txt: array[ErrCode, string] = [ 
    "Internal: this method sould not be called.",
    "Word '$#' has no value.",
    "Fatal: Out of symbol space."
  ]

proc raiseScriptError*(code: ErrCode) {.noinline.} =
  raise newException(Exception, txt[code])

