local C = require('C_ffi')
local test = C.Str("abcdefg")
print(test,C.AddressOf(test),C.SizeOfValue(test))
print(C.MemDump())
-- print((C.MemDump():gsub(".",function(a)return ("%02x"):format(a:byte()).." "end)))