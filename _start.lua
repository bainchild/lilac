local file = (...)
local C = require("C_ffi")
require('libc')
local args = {...}
local argc,argv = C.Obj(#args), C.Ptr(C.Ptr(C.Obj((function()
   local n = {}
   for i=1,#args do
      n[i-1] = args[i]
   end
   return n
end)()))) 
require(file)
if main then
   main(argc,argv)
else
   print("No main defined!")
end
