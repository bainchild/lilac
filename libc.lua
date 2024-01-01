if ____C == nil then require('C_ffi') end
local C = ____C
NULL = C.Ptr(C.Cst(0))
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
strlen = wrap(function(s,maxlen)
   local l = -1
   repeat l=l+1 until NULL==C.Memory[s+l] or l==maxlen
   return l
end)
-- wcslen
strnlen = wrap(function(s)
   local l = -1
   repeat l=l+1 until NULL==C.Memory[s+l]
   return l
end)
-- wcsnlen
memcpy = wrap(function(to,from,size)
   for i=0,size do
      C.Memory[to+i] = C.Memory[from+i]
   end
   return to
end)
wmemcpy = wrap(function(wto,wfrom,size)
   -- maybe?
   for i=0,size*C.SizeOfTypeStr("wchar_t") do
      C.Memory[wto+i] = C.Memory[wfrom+i]
   end
   return wto
end)
mempcpy = wrap(function(to, from, size)
   for i=0,size do
      C.Memory[to+i] = C.Memory[from+i]
   end
   return C.Obj(to+size+1)
end)
wmempcpy = wrap(function(wto,wfrom,size)
   -- maybe?
   for i=0,size*C.SizeOfTypeStr("wchar_t") do
      C.Memory[wto+i] = C.Memory[wfrom+i]
   end
   return C.Obj(wto+size*C.SizeOfTypeStr("wchar_t")+1)
end)
memmove = wrap(function(to,from,size)
   local origs = {}
   for i=0,size do
      origs[i] = C.Memory[to+i]
   end
   for i=0,size do
      C.Memory[from+i] = origs[i]
   end
   return to
end)
wmemmove = wrap(function(wto,wfrom,size)
   local origs = {}
   for i=0,size*C.SizeOfTypeStr("wchar_t") do
      origs[i] = C.Memory[wfrom+i]
   end
   for i=0,size*C.SizeOfTypeStr("wchar_t") do
      C.Memory[wto+i] = origs[i]
   end
   return C.Obj(wto+size*C.SizeOfTypeStr("wchar_t")+1)
end)
memccpy = wrap(function(to,from,c,size)
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
memset = wrap(function(block,c,size)
   for i=0,size do
      C.Memory[block+i] = c
   end
   return block
end,true)
wmemset = wrap(function(block,c,size)
   for i=0,size*C.SizeOfTypeStr("wchar_t") do
      C.Memory[block+i] = c
   end
   return block
end,true)
strcpy = wrap(function(to,from)
   local s = strlen(from)
   for i=0,s do
      C.Memory[to+i] = C.Memory[from+i]
   end
   return to
end,true)
-- wcscpy
strdup = wrap(function(s)
   local size = strlen(s)
   local p = malloc(size)
   if p==-1 then return NULL end
   for i=0,size do
      C.Memory[p+i] = C.Memory[s+i]
   end
   return p
end,true)
-- wcsdupi
stpcpy = wrap(function(s)
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
bcopy = wrap(function(from,to,size)
   local origs = {}
   for i=0,size do
      origs[i] = C.Memory[to+i]
   end
   for i=0,size do
      C.Memory[from+i] = origs[i]
   end
end)
bzero = wrap(function(block,size)
   for i=0,size do C.Memory[block+i] = 0 end
end)
malloc = wrap(function(size)
   local total = count*eltsize
   local ob = C.Object.new()
   local fs = C.GetFreeSpace()
   if fs==-1 then return -1 end
   rawset(ob,"region",{begin=fs,_end=fs+total})
   return ob
end,true)
calloc = wrap(function(count,eltsize)
   local total = count*eltsize
   local ob = C.Obj(("\0"):rep(total))
   rawset(ob,"real",nil)
   return ob
end,true)
strcat = wrap(function(to,from)
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
memcmp = wrap(function(a1,a2,size)
   for i=0,size do
      if C.Memory[a1+i]~=C.Memory[a2+i] then
         return C.Memory[a1+i]-C.Memory[a2+i]
      end
   end
   return 0
end,true)
-- wmemcmp
strcmp = wrap(function(s1,s2)
   return memcmp(s1,s2,math.max(strlen(s1),strlen(s2)))
end,true)
-- wcscmp
-- TODO: strcasecmp
-- wcscasecmp
strncmp = wrap(function(s1,s2,size)
   return memcmp(s1,s2,math.min(math.max(strlen(s1),strlen(s2)),size))
end,true)
-- wcsncmp
-- TODO: strncasecmp
-- wcsncasecmp
-- TODO: strverscmp
bcmp = memcmp
-- TODO: strcoll
-- wcscoll
-- TODO: you're listening to strxfrm, only REAL ROCK FM
-- wcsxfrm
memchr = wrap(function(block,c,size)
   for i=0,size do
      if C.Memory[block+i] == c then return block+i end
   end
   return NULL
end,true)
-- wmemchr
rawmemchr = wrap(function(blk,c)
   local i=0
   repeat i=i+1 until i>2^31 or C.Memory[blk+i] == c
   if i>2^31 then return NULL end
   return blk+i
end,true)
memrchr = wrap(function(block,c,size)
   for i=size,0,-1 do
      if C.Memory[block+i] == c then return block+i end
   end
   return NULL
end,true)
strchr = wrap(function(str,c)
   for i=0,strlen(str) do
      if C.Memory[str+i] == c then return str+i end
   end
   return NULL
end,true)
--wcschr
strchrnul = wrap(function(str,c)
   local len = strlen(str)
   for i=0,len do
      if C.Memory[str+i] == c then return str+i end
   end
   return str+len
end,true)
--wcschrnul
strrchr = wrap(function(str,c)
   for i=strlen(str),0,-1 do
      if C.Memory[str+i] == c then return str+i end
   end
   return NULL
end,true)
--wcsrchr
strstr = wrap(function(hay,needle)
   for i=0,strlen(hay) do
      local mat = true
      for i2=0,strlen(needle) do
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
memmem = wrap(function(hay,haylen,needle,needlelen)
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
strspn = wrap(function(string,skipset)
   local len = strlen(string)
   for i=0,len do
      local char = C.Memory[string+i]
      local good = false
      for n=0,strlen(skipset) do
         if C.Memory[skipset+n]==char then good=true; break end
      end
      if not good then return i end
   end
   return len
end,false)
--wcsspn
strcspn = wrap(function(string,stopset)
   local len = strlen(string)
   for i=0,len do
      local char = C.Memory[string+i]
      local good = true
      for n=0,strlen(stopset) do
         if C.Memory[stopset+n]==char then good=false; break end
      end
      if not good then return i end
   end
   return len
end,false)
--wcscpn
strcspn = wrap(function(string,stopset)
   local len = strlen(string)
   for i=0,len do
      local char = C.Memory[string+i]
      local good = true
      for n=0,strlen(stopset) do
         if C.Memory[stopset+n]==char then good=false; break end
      end
      if not good then return string+i end
   end
   return NULL
end,true)
-- wcspbrk
index = strchr
rindex = strrchr
local internal_strtok_state = nil
strtok = wrap(function(newstring,delim)
   if newstring == NULL and internal_strtok_state == nil then
      return NULL
   elseif newstring~=NULL then
      internal_strtok_state={newstring,0}
   end
   local str = internal_strtok_state[1]
   local strslen = strlen(str)
   local count = 0
   local first
   for i=internal_strtok_state[2],strslen do
      local c = C.Memory[str+i]
      for i2=0,strlen(delim) do
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
strtok_r = wrap(function(new,delim,save)
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
   local strslen = strlen(str)
   local startc = C.Memory[save+C.SizeOfTypeStr("Ptr<char[]>")]
   local tokn = C.Read("int",startc)
   local count = 0
   local first
   for i=tokn,strslen do
      local c = C.Memory[str+i]
      for i2=0,strlen(delim) do
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
explicit_bzero = bzero
-- TODO: in the kitchen, wrist twistin' like it's strfry
memfrob = wrap(function(mem,length)
   for i=0,length do
      C.Memory[mem+i] = bit32.xor(C.Memory[mem+i],0x2a)
   end
   return mem
end,true)
l64a = wrap(function(n)
   if n==0 then return "" end
   return base64e(C.Read("char[4]",n))
end,true)
a64l = wrap(function(n)
   return base64d(C.Read("char[4]",n))
end,true)
-- BIG TODO: argz + envz
-- BIG TWODO: searching and sorting
-- and the pattern matching...
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
