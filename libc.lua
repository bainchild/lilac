if ____C == nil then require('C_ffi') end
local C = ____C
NULL = C.Ptr(C.Cst(0))
local m = {}
local function wrap(a,retptr)
   return function(...)
      local n = {}
      for i,v in next, {...} do
         if C.Object.is(v) then
            v=rawget(v,"real")
         elseif C.Pointer.is(v) then
            v=rawget(v,"obj")
         end
         n[i]=v
      end
      local res = {a((unpack or table.unpack)(n))}
      for i,v in next, res do
         if retptr then
            if not C.Pointer.is(v) then
               if type(v)=="string" then
                  res[i]=C.Str(v)
               else
                  res[i]=C.Ptr(v)
               end
            end
         else
            if not C.Pointer.is(v) and not C.Object.is(v) then
               res[i]=C.Obj(v)
            end
         end
      end      
      return (unpack or table.unpack)(res)
   end
end
m.strlen = wrap(function(s,maxlen)
   local l = -1
   repeat l=l+1 until NULL==C.Memory[s+l] or l==maxlen
   return l
end)
-- wcslen
m.strnlen = wrap(function(s)
   local l = -1
   repeat l=l+1 until NULL==C.Memory[s+l]
   return l
end)
-- wcsnlen
m.memcpy = wrap(function(to,from,size)
   for i=0,size do
      C.Memory[to+i] = C.Memory[from+i]
   end
   return to
end)
m.wmemcpy = wrap(function(wto,wfrom,size)
   -- maybe?
   for i=0,size*C.SizeOfTypeStr("wchar_t") do
      C.Memory[wto+i] = C.Memory[wfrom+i]
   end
   return wto
end)
m.mempcpy = wrap(function(to, from, size)
   for i=0,size do
      C.Memory[to+i] = C.Memory[from+i]
   end
   return C.Obj(to+size+1)
end)
m.wmempcpy = wrap(function(wto,wfrom,size)
   -- maybe?
   for i=0,size*C.SizeOfTypeStr("wchar_t") do
      C.Memory[wto+i] = C.Memory[wfrom+i]
   end
   return C.Obj(wto+size*C.SizeOfTypeStr("wchar_t")+1)
end)
m.memmove = wrap(function(to,from,size)
   local origs = {}
   for i=0,size do
      origs[i] = C.Memory[to+i]
   end
   for i=0,size do
      C.Memory[from+i] = origs[i]
   end
   return to
end)
m.wmemmove = wrap(function(wto,wfrom,size)
   local origs = {}
   for i=0,size*C.SizeOfTypeStr("wchar_t") do
      origs[i] = C.Memory[wfrom+i]
   end
   for i=0,size*C.SizeOfTypeStr("wchar_t") do
      C.Memory[wto+i] = origs[i]
   end
   return C.Obj(wto+size*C.SizeOfTypeStr("wchar_t")+1)
end)
m.memccpy = wrap(function(to,from,c,size)
   local pt
   for i=0,size do
      local f = C.Memory[from+i]
      if f==c then
         pt=to+i
         break
      end
      C.Memory[to+i] = f
   end
   if pt~=nil then
      return pt+1
   else
      return NULL
   end
end,true)
m.memset = wrap(function(block,c,size)
   for i=0,size do
      C.Memory[block+i] = c
   end
   return block
end,true)
m.wmemset = wrap(function(block,c,size)
   for i=0,size*C.SizeOfTypeStr("wchar_t") do
      C.Memory[block+i] = c
   end
   return block
end,true)
m.strcpy = wrap(function(to,from)
   local s = m.strlen(from)
   for i=0,s do
      C.Memory[to+i] = C.Memory[from+i]
   end
   return to
end,true)
-- wcscpy
m.strdup = wrap(function(s)
   local size = m.strlen(s)
   local p = m.malloc(size)
   if p==-1 then return NULL end
   for i=0,size do
      C.Memory[p+i] = C.Memory[s+i]
   end
   return p
end,true)
-- wcsdupi
m.stpcpy = wrap(function(s)
   local size = strlen(s)
   local p = malloc(size)
   if p==-1 then return NULL end
   for i=0,size do
      C.Memory[p+i] = C.Memory[s+i]
   end
   return C.Ptr(p+size)
end,true)
-- wcpcpy
-- stdupa (?)
m.bcopy = wrap(function(from,to,size)
   local origs = {}
   for i=0,size do
      origs[i] = C.Memory[to+i]
   end
   for i=0,size do
      C.Memory[from+i] = origs[i]
   end
end)
m.bzero = wrap(function(block,size)
   for i=0,size do C.Memory[block+i] = 0 end
end)
m.malloc = wrap(function(size)
   local total = count*eltsize
   local ob = C.Object.new()
   local fs = C.GetFreeSpace()
   if fs==-1 then return -1 end
   rawset(ob,"region",{begin=fs,_end=fs+total})
   return ob
end,true)
m.calloc = wrap(function(count,eltsize)
   local total = count*eltsize
   local ob = C.Obj(("\0"):rep(total))
   rawset(ob,"real",nil)
   return ob
end,true)
m.strcat = wrap(function(to,from)
   local l1,l2 = strlen(from),strlen(to)
   for i=0,l1 do
      C.Memory[to+l2-1] = C.Memory[from+i]
   end
   return to
end,true)
--wcscat
--strncpy
--wcsncpy
--will the real slim shady please strndup
--strndupa
--stpncpy
--wcpncpy
--strncat
--wcsncat
--strlcpy
--wcslcpy
--strlcat
--wcslcat
m.memcmp = wrap(function(a1,a2,size)
   for i=0,size do
      if C.Memory[a1+i]~=C.Memory[a2+i] then
         return C.Memory[a1+i]-C.Memory[a2+i]
      end
   end
   return 0
end,true)
-- wmemcmp
m.strcmp = wrap(function(s1,s2)
   return m.memcmp(s1,s2,math.max(strlen(s1),strlen(s2)))
end,true)
-- wcscmp
-- TODO: strcasecmp
-- wcscasecmp
m.strncmp = wrap(function(s1,s2,size)
   return m.memcmp(s1,s2,math.min(math.max(strlen(s1),strlen(s2)),size))
end,true)
-- wcsncmp
-- TODO: strncasecmp
-- wcsncasecmp
-- TODO: strverscmp
m.bcmp = m.memcmp
-- TODO: strcoll
-- wcscoll
-- TODO: you're listening to strxfrm, only REAL ROCK FM
-- wcsxfrm
m.memchr = wrap(function(block,c,size)
   for i=0,size do
      if C.Memory[block+i] == c then return block+i end
   end
   return NULL
end,true)
-- wmemchr
m.rawmemchr = wrap(function(blk,c)
   local i=0
   repeat i=i+1 until i>2^31 or C.Memory[blk+i] == c
   if i>2^31 then return NULL end
   return blk+i
end,true)
m.memrchr = wrap(function(block,c,size)
   for i=size,0,-1 do
      if C.Memory[block+i] == c then return block+i end
   end
   return NULL
end,true)
m.strchr = wrap(function(str,c)
   for i=0,m.strlen(str) do
      if C.Memory[str+i] == c then return str+i end
   end
   return NULL
end,true)
--wcschr
m.strchrnul = wrap(function(str,c)
   local len = m.strlen(str)
   for i=0,len do
      if C.Memory[str+i] == c then return str+i end
   end
   return str+len
end,true)
--wcschrnul
m.strrchr = wrap(function(str,c)
   for i=m.strlen(str),0,-1 do
      if C.Memory[str+i] == c then return str+i end
   end
   return NULL
end,true)
--wcsrchr
m.strstr = wrap(function(hay,needle)
   for i=0,m.strlen(hay) do
      local mat = true
      for i2=0,m.strlen(needle) do
         if C.Memory[hay+i]~=C.Memory[needle+i2] then
            mat=false; break
         end
      end
      if mat then return hay+i end
   end
   return NULL
end,true)
--wcsstr
--wcswcs
--TODO: strcasestr
m.memmem = wrap(function(hay,haylen,needle,needlelen)
   for i=0,haylen do
      local mat = true
      for i2=0,needlelen do
         if C.Memory[hay+i]~=C.Memory[needle+i2] then
            mat=false; break
         end
      end
      if mat then return hay+i end
   end
   return NULL
end,true)
m.strspn = wrap(function(string,skipset)
   local len = m.strlen(string)
   for i=0,len do
      local char = C.Memory[string+i]
      local good = false
      for n=0,m.strlen(skipset) do
         if C.Memory[skipset+n]==char then good=true; break end
      end
      if not good then return i end
   end
   return len
end,false)
--wcsspn
m.strcspn = wrap(function(string,stopset)
   local len = m.strlen(string)
   for i=0,len do
      local char = C.Memory[string+i]
      local good = true
      for n=0,m.strlen(stopset) do
         if C.Memory[stopset+n]==char then good=false; break end
      end
      if not good then return i end
   end
   return len
end,false)
--wcscpn
m.strcspn = wrap(function(string,stopset)
   local len = m.strlen(string)
   for i=0,len do
      local char = C.Memory[string+i]
      local good = true
      for n=0,m.strlen(stopset) do
         if C.Memory[stopset+n]==char then good=false; break end
      end
      if not good then return string+i end
   end
   return NULL
end,true)
-- wcspbrk
m.index = m.strchr
m.rindex = m.strrchr
local internal_strtok_state = nil
m.strtok = wrap(function(newstring,delim)
   if newstring == NULL and internal_strtok_state == nil then
      return NULL
   elseif newstring~=NULL then
      internal_strtok_state={newstring,0}
   end
   local str = internal_strtok_state[1]
   local strslen = m.strlen(str)
   local count = 0
   local first
   for i=internal_strtok_state[2],strslen do
      local c = C.Memory[str+i]
      for i2=0,m.strlen(delim) do
         if c==C.Memory[delim+i2] then
            if first==nil then first=i end
            count=count+1
            break
         end
      end
   end
   if count==strslen or count==0 or first==nil then
      return NULL
   end
   for i=internal_strtok_state[2],first do
      C.Memory[str+i-internal_strtok_state[2] ] = C.Memory[str+i]
   end
   C.Memory[str+first-internal_strtok_state[2]+1] = 0 -- close str
   internal_strtok_state[2] = i+1
   return 
end)
--wcstok
m.strtok_r = wrap(function(new,delim,save)
   if newstring == NULL and C.Memory[save] == 0 then
      return NULL
   elseif newstring~=NULL then
      local ser = C.Serialize(new)
      for i=0,#ser do
         C.Memory[save+i]=ser:sub(i,i):byte()
      end
      local ser2 = C.Serialize(0)
      for i=0,#ser2 do
         C.Memory[save+#ser+i+1]=ser2:sub(i,i):byte()
      end
   end
   local str = C.Read("Ptr<char[]>",C.Memory[save])
   local strslen = m.strlen(str)
   local startc = C.Memory[save+C.SizeOfTypeStr("Ptr<char[]>")]
   local tokn = C.Read("int",startc)
   local count = 0
   local first
   for i=tokn,strslen do
      local c = C.Memory[str+i]
      for i2=0,m.strlen(delim) do
         if c==C.Memory[delim+i2] then
            if first==nil then first=i end
            count=count+1
            break
         end
      end
   end
   if count==strslen or count==0 or first==nil then
      return NULL
   end
   for i=tokn,first do
      C.Memory[str+i-tokn] = C.Memory[str+i]
   end
   C.Memory[str+first-tokn+1] = 0 -- close str
   C.Write("int",tokn,i+1)
   return str
end,true)
-- TODO: strsep, also rework above 2 for the storage
-- TODO: basename + dirname
m.explicit_bzero = bzero
-- TODO: in the kitchen, wrist twistin' like it's strfry
m.memfrob = wrap(function(mem,length)
   for i=0,length do
      C.Memory[mem+i] = bit32.xor(C.Memory[mem+i],0x2a)
   end
   return mem
end,true)
m.l64a = wrap(function(n)
   if n==0 then return "" end
   return base64e(C.Read("char[4]",n))
end,true)
m.a64l = wrap(function(n)
   return base64d(C.Read("char[4]",n))
end,true)
-- BIG TODO: argz + envz
-- BIG TWODO: searching and sorting
-- and the pattern matching...
function m.printf(fs,...)
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
function m.print(...)
   local args = {...}
   for i,v in next, args do
      if C.Object.is(v) or C.Pointer.is(v) then
         args[i] = v[C.Escape]
      end
   end
   local n = ""
   for i,v in next, args do
      if i~=1 then
         n=n.." "
      end
      n=n..tostring(v)
   end
   return print(n)
end
return m
