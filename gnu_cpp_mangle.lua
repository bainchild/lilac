local parse_symbol,symbol_to_signature
local signature_to_symbol
do
   local types = {
     ["i"] = true,
     ["c"] = true,
     ["v"] = true,
     ["S"] = 2 -- So, Ss
   }
   function parse_symbol(symb)
     symb = symb:sub(3) -- _Z
     -- print(symb)
     local tab = {}
     while true do
       if #symb == 0 then break end
       local typ = symb:sub(1, 1)
       print(typ, symb, "->", symb:sub(2))
       symb = symb:sub(2)
       if typ == "N" or typ:match("%d") then
         if typ == "N" then
           local n = {}
           while true do
             if symb:sub(1, 1) == "E" then
               symb = symb:sub(2); break
             end
             local len_s, len_e = symb:find("^%d+")
             if len_s == nil or len_e == nil then
               error("arghhhh (" .. symb .. ")")
             end
             local len = assert(tonumber(symb:sub(len_s, len_e)))
             symb = symb:sub(len_e + 1)
             local id = symb:sub(1, len)
             symb = symb:sub(len + 1)
             table.insert(n, id)
           end
           tab.Name = n
         elseif typ:match("%d") then
           do
             local len_s, len_e = symb:find("^%d+")
             local len
             if len_s == nil or len_e == nil then
               len = typ
             else
               len = assert(tonumber(symb:sub(len_s, len_e)))
               symb = symb:sub(len_e + 1)
             end
             local id = symb:sub(1, len)
             symb = symb:sub(len + 1)
             tab.Name = { id }
           end
         end
         local parms = {}
         local parmed = ""
         while true do
           if symb:sub(1, 1) == "R" then
             parmed = "*" .. parmed
             symb=symb:sub(2)
           elseif symb:sub(1, 1) == "K" then
             parmed = "const " .. parmed
             symb=symb:sub(2)
           elseif types[symb:sub(1, 1)] then
             if type(types[symb:sub(1, 1)]) == "number" then
               parmed = parmed..symb:sub(1, types[symb:sub(1, 1)])
               symb = symb:sub(types[symb:sub(1, 1)] + 1)
             else
               parmed = parmed..symb:sub(1, 1)
               symb = symb:sub(2)
             end
             table.insert(parms, parmed)
             parmed = ""
           else
             break
           end
         end
         tab.Parameters = parms
       else
         break
       end
     end
     return tab
   end
   local typemap = {
     ["i"] = "int",
     ["c"] = "char",
     ["v"] = "void",
     ["So"] = "std::ostream",
     ["Ss"] = "std::string"
   }
   local function typ_to_name(a)
      for i,v in next, typemap do
         if a:sub(-#i) == i then
            return a:sub(1,-(#i+1))..v
         end
      end
      return nil
   end
   -- return types have NEVER existed, what do you mean?
   function symbol_to_signature(parsed)
     local s = ""
     -- for i, v in next, parsed.ReturnTypes or { "v" } do
     --   if i ~= 1 then s = s .. ", " end
     --   s = s .. (typ_to_name(v) or ("unknowntype_" .. v))
     -- end
     s = s .. table.concat(parsed.Name, "::")
     s = s .. "("
     for i, v in next, parsed.Parameters or { "v" } do
       if i ~= 1 then s = s .. ", " end
       s = s .. (typ_to_name(v) or ("unknowntype_" .. v))
     end
     s = s .. ")"
     return s
   end
end
do
   local function split(a,b,c)
      local mat = {}
      for m in ((c and a..c) or (a..b)):gmatch("(.-)"..b) do
         table.insert(mat,m)
      end
      return mat
   end
   function signature_to_symbol(signature)
      local name, params = signature:match("([^%(]+)%((.-)%)")
      name = split(name,"::")
      params = split(params,",")
      local out = "_Z"
      if #name==1 then
         out=out..tostring(#name[1])..name[1]
      else
         out=out.."N"
         for _,v in next, name do
            out=out..tostring(#v)..v
         end
         out=out.."E"
      end
      local typ_to_mangled = {
         ["void"]="v",
         ["int"]="i",
         ["char"]="c",
         ["std::string"]="Ss",
         ["std::ostream"]="So"
      }
      for _,v in next, params do
         ---@diagnostic disable-next-line: deprecated
         local cons,typ = (unpack or table.unpack)(split(v,"%s+"," "))
         local const = false
         if typ==nil and cons~=nil then
            typ=cons
            const=true
         end
         local parm = ""
         while true do
            if typ:sub(1,1)=="*" then
               parm="R"..(const and "K" or "")..parm
               typ=typ:sub(2)
            else
               break
            end
         end
         for from,m in next,typ_to_mangled do
            if typ:sub(1,#from) == from then
               typ=typ:sub(#from+1)
               parm=parm..m
            end
         end
         while true do
            if typ:sub(1,1)=="*" then
               parm="R"..(const and "K" or "")..parm
               typ=typ:sub(2)
            else
               break
            end
         end
         out=out..parm
      end
      return out
   end
end
return {
   parse_symbol=parse_symbol,
   symbol_to_signature=symbol_to_signature,
   signature_to_symbol=signature_to_symbol
}
