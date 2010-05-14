pro str_replace,string1,old_substring,new_substring,reverse_search = rev

pos = strpos(reverse_search=rev,string1,old_substring)
if pos lt 0 then return
string1 = strmid(string1,0,pos) + new_substring + strmid(string1,pos+strlen(old_substring))
return
end

