-- Search for 6 mark ruler of max length 17
var list  : 18 = 1
var dist  : 18 = 0
var marks : 5  = 1

while marks /= 6 do
  if msb(list) == 1 then fail end ;
  if (list & dist) == 0 then
      (list := list << 1)
    ? (marks := marks + 1 ||
       dist := dist | list ;
       list := (list << 1) | 1)
  else
    list := list << 1
  end
end
