if ___C~=nil then return ____C end
local escape = {}
local obj,ptr
do
   local objmt = {}
   obj = {}
   function objmt:__index(k)
      if k==escape then
         local new = rawget(self,"List")[self]
         if obj.is(new) or ptr.is(new) then
            return new[escape]
         else
            return new
         end
      end
      if obj.is(k) or ptr.is(k) then k=k[escape] end
      return rawget(self,"List")[self][k]
   end
   function objmt:__newindex(k,v)
      rawget(self,"List")[self][k] = v
   end
   function objmt:__tostring()
      return tostring(rawget(self,"List")[self])
   end
   function objmt:__add(o)
      if obj.is(o) then
         return rawget(self,"List")[self]+rawget(o,"List")[o]
      elseif ptr.is(o) then
         return rawget(self,"List")[self]+rawget(o,"obj")
      else
         return rawget(self,"List")[self]+o
      end
      error("ummm.... ("..type(o)..")")
   end
   function objmt:__lt(o)
      if obj.is(o) then
         return rawget(self,"List")[self]<rawget(o,"List")[o]
      elseif ptr.is(o) then
         return rawget(self,"List")[self]<rawget(o,"obj")
      else
         return rawget(self,"List")[self]<o
      end
      error("ummm.... ("..type(o)..")")
   end
   function obj.new(list)
      return setmetatable({
         List=list;
      },objmt)
   end
   function obj.is(o)
      return type(o)=="table" and getmetatable(o)==objmt
   end
end
do
   local ptrmt = {}
   function ptrmt:__index(k)
      if k==escape then
         local new = rawget(self,"obj")
         if obj.is(new) or ptr.is(new) then
            return new[escape]
         else
            return new
         end
      end
      if obj.is(k) or ptr.is(k) then k=k[escape] end
      return rawget(self,"obj")[k]
   end
   function ptrmt:__newindex(k,v)
      rawget(self,"obj")[k]=v
   end
   function ptrmt:__tostring()
      return "*"..tostring(rawget(self,"obj"))
   end
   ptr = {}
   function ptr.new(a,addr)
      return setmetatable({obj=a,addr=addr},ptrmt)
   end
   function ptr.is(a)
      return getmetatable(a)==ptrmt
   end
end

local void = 0
local C = {
   Pointers = {}, -- obj -> *obj
   Objects = {},
   TypeSizes = {},
   Object = obj,
   Pointer = ptr,
   Escape = escape
}
function C.Deref(a)
   return find(C.Pointers,a)
end
function C.SizeOfType(t)
   return C.TypeSizes[t]
end
function C.SizeOfValue(v)
   -- todo
end
function C.Set(a,b)
   C.Objects[a] = b
end
function C.AddressOf(v)
   -- todo
end
function C.Obj(a)
   local ob = obj.new(C.Objects)
   C.Objects[ob] = a
   return ob
end
function C.Ptr(a)
   local pt = ptr.new(a)
   C.Pointers[a] = pt
   return pt
end
function C.Str(str)
   return C.Ptr(C.Obj(str))
end
function C.Cst(constant)
   return C.Obj(constant)
end
function C.List(tab) -- tab is uhhh {string}

end
function C.Uninitialized()
   local new = obj.new(C.Objects)
   C.Objects[new] = void
   return new
end
function C.PracticalType(n)
   return tostring(n)
end
____C = C
return C
