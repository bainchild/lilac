local _namespace, _exports, _imports, _memory, _strtab = {}, {}, {}, {}

_imports.raise = {
    type = 'function',
    parameters = {
        'int',
    },
    variadic = false,
    size = 1,
}
_imports.strcmp = {
    type = 'function',
    parameters = {
        'char',
        'char',
    },
    variadic = false,
    size = 1,
}

local func_main = function(argc, argv)
    local _pointersize, retur = _namespace.sizeof'*char', nil

    if (argc >= 1) then
        if ((_namespace.strcmp(_memory[argv + (0) * _pointersize], _strtab[1]) == 0) or (_namespace.strcmp(_memory[argv + (0) * _pointersize], _strtab[2]) == 0)) then
            retur = 0
        else
            if ((_namespace.strcmp(_memory[argv + (0) * _pointersize], _strtab[3]) == 0) or (_namespace.strcmp(_memory[argv + (0) * _pointersize], _strtab[4]) == 0)) then
                retur = 1
            else
                retur = 3
            end
        end
    else
        retur = 4
    end

    return retur
end

_namespace.main = func_main
_exports.main = {
    parameters = {
        'int',
        'char',
    },
    return_ = 'int',
    size = 1,
}
_strtab = {
    'yeah',
    'yes',
    'nah',
    'no',
}

return {
    namespace = _namespace,
    exports = _exports,
    imports = _imports,
    memory = _memory,
    set_memory = function(mem)
        _memory = mem
    end,
    strtab = _strtab,
}
