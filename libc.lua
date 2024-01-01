if ____C == nil then require('C_ffi') end
local C = ____C
function printf(fs,...)
   if C.Object.is(fs) or C.Pointer.is(fs) then
      fs=fs[C.Escape]
   end
   local args = {...}
   for i,v in next, args do
      if C.Object.is(v) or C.Pointer.is(v) then
         args[i] = v[C.Escape]
      end
   end
   io.write(fs:format(table.unpack(args)))
end
