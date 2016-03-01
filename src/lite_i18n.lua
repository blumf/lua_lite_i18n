-----------------------------------------------------------------------------
-- simple i18n tool
-- parse a script pulling out all instances of translatable strings
-----------------------------------------------------------------------------



-- customization options

--- the function name to search for.
local i18n_func = "TR"



-----------------------------------------------------------------------------

--- Turns a table into a CSV line with escaping.
-- @param t array table of values to write
-- @param comma (optional) The character to use for field seperation
-- @param quote (optional) The character to use for quoting field text
-- @param newline (optional) The string to use in replacing newlines
-- @return String in CSV format
local function make_csv(t, comma, quote, newline)
	comma = comma or ','
	quote = quote or '"'
	newline = newline or '|'

	local dbl_quote = quote..quote

	local function escape_value(v)
		local vt = type(v)

		assert(vt ~= "table", "Can't escape a table")
		assert(vt ~= "function", "Can't escape a function")
		assert(vt ~= "userdata", "Can't escape a userdata")
		assert(vt ~= "thread", "Can't escape a thread")

		if vt == "number" then return v; end
		if vt == "nil" then return ""; end
		if vt == "boolean" then return quote..tostring(v)..quote; end

		v = string.gsub(tostring(v), quote, dbl_quote)
		v = string.gsub(tostring(v), '\n', newline)
		v = string.gsub(tostring(v), '\r', '')

		return quote..v..quote
	end

	local tmp_t = {}
	for i = 1,#t do
		tmp_t[i] = escape_value(t[i])
	end

	return table.concat(tmp_t, comma)
end

--- Splits a string according to CSV rules.
-- @param s The string to split
-- @param comma (optional) The character to use for field seperation
-- @param quote (optional) The character to use for quoting field text
-- @return Table containing the split data, or nil + error message
local function split_csv(s, comma, quote)
	comma = comma or ','
	quote = quote or '"'
	
	local comma_inv = '^'..comma
	local quote_inv = '^'..quote
	local quote_match = quote.."("..quote.."?)"
	
	s = s .. comma		-- ending comma
	local t = {}		-- table to collect fields
	local fieldstart = 1
	
	repeat
		-- next field is quoted? (start with `"'?)
		if string.find(s, quote_inv, fieldstart) then
			local a, c
			local i  = fieldstart
			repeat
				-- find closing quote
				a, i, c = string.find(s, quote_match, i+1)
			until c ~= quote	-- quote not followed by quote?
			
			if not i then
				return nil, "Error : unmatched "..quote
			end
			
			local f = string.sub(s, fieldstart+1, i-1)
			table.insert(t, (string.gsub(f, quote..quote, quote)))
			fieldstart = string.find(s, comma, i) + 1
		else                -- unquoted; find next comma
			local nexti = string.find(s, comma, fieldstart)
			table.insert(t, string.sub(s, fieldstart, nexti-1))
			fieldstart = nexti + 1
		end
	until fieldstart > string.len(s)
	
	return t
end

--- Iterate over a CSV file.
-- @param path The path of the file to parse
-- @param comma (optional) The character to use for field seperation
-- @param quote (optional) The character to use for quoting field text
-- @return An iterator function that returns parsed CSV rows
-- usage:
--   for row in read_csv("test.csv") do
--     print(row[1], row[2])
--   end
local function read_csv(path, comma, quote)
	local f = io.open(path, "r")

	-- skip UTF8 BOM (if found)
	do
		local bom = f:read(3)
		if bom ~= string.char(239,187,191) then
			-- no BOM, reset location
			f:seek("set", 0)
		end
	end

	return function()
		local l = f:read("*l")
		if not l then
			f:close()
			return nil
		end
		
		local row = split_csv(l, comma, quote)
		while not row do
			l = l.."\n"..f:read("*l")
			row = split_csv(l, comma, quote)
		end

		return row
	end
end

-- stuff used by the lex_lua function
local function lookupify(tb)
	for _, v in pairs(tb) do
		tb[v] = true
	end
	return tb
end

local WhiteChars = lookupify{' ', '\n', '\t', '\r'}
local EscapeLookup = {['\r'] = '\\r', ['\n'] = '\\n', ['\t'] = '\\t', ['"'] = '\\"', ["'"] = "\\'"}
local LowerChars = lookupify{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
							 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
							 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'}
local UpperChars = lookupify{'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
							 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
							 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'}
local Digits = lookupify{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'}
local HexDigits = lookupify{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
							'A', 'a', 'B', 'b', 'C', 'c', 'D', 'd', 'E', 'e', 'F', 'f'}

local Symbols = lookupify{'+', '-', '*', '/', '^', '%', ',', '{', '}', '[', ']', '(', ')', ';', '#'}

local Keywords = lookupify{
	'and', 'break', 'do', 'else', 'elseif',
	'end', 'false', 'for', 'function', 'goto', 'if',
	'in', 'local', 'nil', 'not', 'or', 'repeat',
	'return', 'then', 'true', 'until', 'while',
};

--- parse Lua script, turns it into token stream.
-- lexer taken from LuaMinify : https://github.com/stravant/LuaMinify
-- @param src Lua source code to parse
-- @return a token stream object
local function lex_lua(src)
	--token dump
	local tokens = {}

	local st, err = pcall(function()
		--line / char / pointer tracking
		local p = 1
		local line = 1
		local char = 1

		--get / peek functions
		local function get()
			local c = src:sub(p,p)
			if c == '\n' then
				char = 1
				line = line + 1
			else
				char = char + 1
			end
			p = p + 1
			return c
		end
		local function peek(n)
			n = n or 0
			return src:sub(p+n,p+n)
		end
		local function consume(chars)
			local c = peek()
			for i = 1, #chars do
				if c == chars:sub(i,i) then return get() end
			end
		end

		--shared stuff
		local function generateError(err)
			return error(">> :"..line..":"..char..": "..err, 0)
		end

		local function tryGetLongString()
			local start = p
			if peek() == '[' then
				local equalsCount = 0
				local depth = 1
				while peek(equalsCount+1) == '=' do
					equalsCount = equalsCount + 1
				end
				if peek(equalsCount+1) == '[' then
					--start parsing the string. Strip the starting bit
					for _ = 0, equalsCount+1 do get() end

					--get the contents
					local contentStart = p
					while true do
						--check for eof
						if peek() == '' then
							generateError("Expected `]"..string.rep('=', equalsCount).."]` near <eof>.", 3)
						end

						--check for the end
						local foundEnd = true
						if peek() == ']' then
							for i = 1, equalsCount do
								if peek(i) ~= '=' then foundEnd = false end
							end
							if peek(equalsCount+1) ~= ']' then
								foundEnd = false
							end
						else
							if peek() == '[' then
								-- is there an embedded long string?
								local embedded = true
								for i = 1, equalsCount do
									if peek(i) ~= '=' then
										embedded = false
										break
									end
								end
								if peek(equalsCount + 1) == '[' and embedded then
									-- oh look, there was
									depth = depth + 1
									for i = 1, (equalsCount + 2) do
										get()
									end
								end
							end
							foundEnd = false
						end
						--
						if foundEnd then
							depth = depth - 1
							if depth == 0 then
								break
							else
								for i = 1, equalsCount + 2 do
									get()
								end
							end
						else
							get()
						end
					end

					--get the interior string
					local contentString = src:sub(contentStart, p-1)

					--found the end. Get rid of the trailing bit
					for i = 0, equalsCount+1 do get() end

					--get the exterior string
					local longString = src:sub(start, p-1)

					--return the stuff
					return contentString, longString
				else
					return nil
				end
			else
				return nil
			end
		end

		--main token emitting loop
		while true do
			--get leading whitespace. The leading whitespace will include any comments
			--preceding the token. This prevents the parser needing to deal with comments
			--separately.
			local leading = { }
			local leadingWhite = ''
			local longStr = false
			while true do
				local c = peek()
				if c == '#' and peek(1) == '!' and line == 1 then
					-- #! shebang for linux scripts
					get()
					get()
					leadingWhite = "#!"
					while peek() ~= '\n' and peek() ~= '' do
						leadingWhite = leadingWhite .. get()
					end
					local token = {
						Type = 'Comment',
						CommentType = 'Shebang',
						Data = leadingWhite,
						Line = line,
						Char = char
					}
					token.Print = function()
						return "<"..(token.Type .. string.rep(' ', 7-#token.Type)).."  "..(token.Data or '').." >"
					end
					leadingWhite = ""
					table.insert(leading, token)
				end
				if c == ' ' or c == '\t' then
					--whitespace
					--leadingWhite = leadingWhite..get()
					local c2 = get() -- ignore whitespace
					table.insert(leading, { Type = 'Whitespace', Line = line, Char = char, Data = c2 })
				elseif c == '\n' or c == '\r' then
					local nl = get()
					if leadingWhite ~= "" then
						local token = {
							Type = 'Comment',
							CommentType = longStr and 'LongComment' or 'Comment',
							Data = leadingWhite,
							Line = line,
							Char = char,
						}
						token.Print = function()
							return "<"..(token.Type .. string.rep(' ', 7-#token.Type)).."  "..(token.Data or '').." >"
						end
						table.insert(leading, token)
						leadingWhite = ""
					end
					table.insert(leading, { Type = 'Whitespace', Line = line, Char = char, Data = nl })
				elseif c == '-' and peek(1) == '-' then
					--comment
					get()
					get()
					leadingWhite = leadingWhite .. '--'
					local _, wholeText = tryGetLongString()
					if wholeText then
						leadingWhite = leadingWhite..wholeText
						longStr = true
					else
						while peek() ~= '\n' and peek() ~= '' do
							leadingWhite = leadingWhite..get()
						end
					end
				else
					break
				end
			end
			if leadingWhite ~= "" then
				local token = {
					Type = 'Comment',
					CommentType = longStr and 'LongComment' or 'Comment',
					Data = leadingWhite,
					Line = line,
					Char = char,
				}
				token.Print = function()
					return "<"..(token.Type .. string.rep(' ', 7-#token.Type)).."  "..(token.Data or '').." >"
				end
				table.insert(leading, token)
			end

			--get the initial char
			local thisLine = line
			local thisChar = char
			local errorAt = ":"..line..":"..char..":> "
			local c = peek()

			--symbol to emit
			local toEmit = nil

			--branch on type
			if c == '' then
				--eof
				toEmit = { Type = 'Eof' }

			elseif UpperChars[c] or LowerChars[c] or c == '_' then
				--ident or keyword
				local start = p
				repeat
					get()
					c = peek()
				until not (UpperChars[c] or LowerChars[c] or Digits[c] or c == '_')
				local dat = src:sub(start, p-1)
				if Keywords[dat] then
					toEmit = {Type = 'Keyword', Data = dat}
				else
					toEmit = {Type = 'Ident', Data = dat}
				end

			elseif Digits[c] or (peek() == '.' and Digits[peek(1)]) then
				--number const
				local start = p
				if c == '0' and peek(1) == 'x' then
					get();get()
					while HexDigits[peek()] do get() end
					if consume('Pp') then
						consume('+-')
						while Digits[peek()] do get() end
					end
				else
					while Digits[peek()] do get() end
					if consume('.') then
						while Digits[peek()] do get() end
					end
					if consume('Ee') then
						consume('+-')
						while Digits[peek()] do get() end
					end
				end
				toEmit = {Type = 'Number', Data = src:sub(start, p-1)}

			elseif c == '\'' or c == '\"' then
				local start = p
				--string const
				local delim = get()
				local contentStart = p
				while true do
					local c = get()
					if c == '\\' then
						get() --get the escape char
					elseif c == delim then
						break
					elseif c == '' then
						generateError("Unfinished string near <eof>")
					end
				end
				local content = src:sub(contentStart, p-2)
				local constant = src:sub(start, p-1)
				toEmit = {Type = 'String', Data = constant, Constant = content}

			elseif c == '[' then
				local content, wholetext = tryGetLongString()
				if wholetext then
					toEmit = {Type = 'String', Data = wholetext, Constant = content}
				else
					get()
					toEmit = {Type = 'Symbol', Data = '['}
				end

			elseif consume('>=<') then
				if consume('=') then
					toEmit = {Type = 'Symbol', Data = c..'='}
				else
					toEmit = {Type = 'Symbol', Data = c}
				end

			elseif consume('~') then
				if consume('=') then
					toEmit = {Type = 'Symbol', Data = '~='}
				else
					generateError("Unexpected symbol `~` in source.", 2)
				end

			elseif consume('.') then
				if consume('.') then
					if consume('.') then
						toEmit = {Type = 'Symbol', Data = '...'}
					else
						toEmit = {Type = 'Symbol', Data = '..'}
					end
				else
					toEmit = {Type = 'Symbol', Data = '.'}
				end

			elseif consume(':') then
				if consume(':') then
					toEmit = {Type = 'Symbol', Data = '::'}
				else
					toEmit = {Type = 'Symbol', Data = ':'}
				end

			elseif Symbols[c] then
				get()
				toEmit = {Type = 'Symbol', Data = c}

			else
				local contents, all = tryGetLongString()
				if contents then
					toEmit = {Type = 'String', Data = all, Constant = contents}
				else
					generateError("Unexpected Symbol `"..c.."` in source.", 2)
				end
			end

			--add the emitted symbol, after adding some common data
			toEmit.LeadingWhite = leading -- table of leading whitespace/comments
			--for k, tok in pairs(leading) do
			--	tokens[#tokens + 1] = tok
			--end

			toEmit.Line = thisLine
			toEmit.Char = thisChar
			toEmit.Print = function()
				return "<"..(toEmit.Type..string.rep(' ', 7-#toEmit.Type)).."  "..(toEmit.Data or '').." >"
			end
			tokens[#tokens+1] = toEmit

			--halt after eof has been emitted
			if toEmit.Type == 'Eof' then break end
		end
	end)
	if not st then
		return nil, err
	end

	--public interface:
	local tok = {}
	local savedP = {}
	local p = 1
	
	function tok:getp()
		return p
	end
	
	function tok:setp(n)
		p = n
	end
	
	function tok:getTokenList()
		return tokens
	end
	
	--getters
	function tok:Peek(n)
		n = n or 0
		return tokens[math.min(#tokens, p+n)]
	end
	function tok:Get(tokenList)
		local t = tokens[p]
		p = math.min(p + 1, #tokens)
		if tokenList then
			table.insert(tokenList, t)
		end
		return t
	end
	function tok:Is(t)
		return tok:Peek().Type == t
	end

	--save / restore points in the stream
	function tok:Save()
		savedP[#savedP+1] = p
	end
	function tok:Commit()
		savedP[#savedP] = nil
	end
	function tok:Restore()
		p = savedP[#savedP]
		savedP[#savedP] = nil
	end

	--either return a symbol if there is one, or return true if the requested
	--symbol was gotten.
	function tok:ConsumeSymbol(symb, tokenList)
		local t = self:Peek()
		if t.Type == 'Symbol' then
			if symb then
				if t.Data == symb then
					self:Get(tokenList)
					return true
				else
					return nil
				end
			else
				self:Get(tokenList)
				return t
			end
		else
			return nil
		end
	end

	function tok:ConsumeKeyword(kw, tokenList)
		local t = self:Peek()
		if t.Type == 'Keyword' and t.Data == kw then
			self:Get(tokenList)
			return true
		else
			return nil
		end
	end

	function tok:IsKeyword(kw)
		local t = tok:Peek()
		return t.Type == 'Keyword' and t.Data == kw
	end

	function tok:IsSymbol(s)
		local t = tok:Peek()
		return t.Type == 'Symbol' and t.Data == s
	end

	function tok:IsEof()
		return tok:Peek().Type == 'Eof'
	end

	return tok
end

--- turns a quoted source string into a normal string
local function make_string(qstr)
	return load("return "..qstr)()
end

--- parses a string containg Lua code, returns all texts wrapped in TR calls.
-- @param src the Lua source code to parse
-- @param res optional table to store the results in
-- @return a map of found strings 
local function parse_string(src, res)
	res = res or {}
	local lex, err = lex_lua(src)
	if not lex then error("Failed to parse script: "..err or "unknown reason"); end

	local token = lex.Get()
	while token.Type ~= "Eof" do
		if token.Type == "Ident" and token.Data == i18n_func then
			token = lex.Get()

			-- skip open bracket of function call
			if token.Type == "Symbol" and token.Data == "(" then
				token = lex.Get()
			end

			-- found the text
			if token.Type == "String" then
				local txt = make_string(token.Data)
				res[txt] = txt
			end
		end

		token = lex.Get()
	end

	return res
end

--- parses a Lua file, returns all texts wrapped in TR calls.
-- @param path the Lua file to parse
-- @param res optional table to store the results in
-- @return a map of found strings
-- @see pstring
local function parse_file(path, res)
	local f = io.open(path, "r")
	local src = f:read"a"
	f:close()

	return parse_string(src, res)
end

--- write language pack to open file
local function dump_pack_to_file(pack, lang, f)
	for txt, trans in pairs(pack) do
		f:write(make_csv({txt,lang,trans}), "\n")
	end
end

--- dumps a text map into a CSV file for translation.
-- @param pack the text map
-- @param lang the language/region identifier
-- @param path the path to write the CSV to
-- CSV format is "<original text>","<lang/region code>","<translated text>"
local function dump_pack(pack, lang, path)
	local f = io.open(path, "w+")
	dump_pack_to_file(pack, lang, f)
	f:close()
end

--- dumps all language packs into a CSV file for storage.
-- @param packs a table containing all the language packs
-- @param path the path to write the CSV to
-- CSV format is "<original text>","<lang/region code>","<translated text>"
local function dump_packs(packs, path)
	local f = io.open(path, "w+")
	for lang, pack in pairs(packs) do
		dump_pack_to_file(pack, lang, f)
	end
	f:close()
end

--- loads translation CSV dump into pack
-- @param path the path to the CSV translation file
-- @param packs (optional) table load with out languages
-- @return table of language packs
local function load_from_trans(path, packs)
	packs = packs or {}
	for row in read_csv(path) do
		local org, lang, trans = row[1], row[2], row[3]
		local pack = packs[lang]
		if not pack then
			pack = {}
			packs[lang] = pack
		end
		pack[org] = trans
	end

	return packs
end

--- turns a set of language packs into a language module ready to `require` into a Lua session.
-- @param packs a table containing all the language packs
-- @param path the path to write the language module to
local function write_package(packs, path)
	local f = io.open(path, "w+")

	f:write("local packs = {\n");
	for lang, pack in pairs(packs) do
		f:write("\t[\"", lang:format"%q", "\"] = {\n")

		for org, trans in pairs(pack) do
			f:write("\t\t[\"", org:format"%q", "\"] = \"", trans:format"%q", "\",\n")
		end

		f:write("\t},\n")
	end
	f:write("}\n\n")

	f:write([[
local tr_repos = {}
tr_repos["__default"] = function(str)
	return str
end

return function(lang)
	local repos = tr_repos[lang]

	-- build translation closure
	if not repos then
		local pack = packs[lang]
		if pack then
			repos = function(str)
				return pack[str] or str
			end
			tr_repos[lang] = repos
		end
	end

	return repos or tr_repos["__default"]
end]])

	f:close()
end

return {
	parse_string = parse_string,
	parse_file = parse_file,

	dump_pack = dump_pack,
	dump_packs = dump_packs,
	load_packs = load_from_trans,

	write_package = write_package,
}
