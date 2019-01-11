(module 
  (import "system" "printInt" (func $Std_printInt (param i32) (result i32)))
  (import "system" "printString" (func $Std_printString (param i32) (result i32)))
  (import "system" "readString0" (func $js_readString0 (param i32) (result i32)))
  (import "system" "readInt" (func $Std_readInt (result i32)))
  (import "system" "mem" (memory 100))
  (global (mut i32) i32.const 0) 

  (func $String_concat (param i32 i32) (result i32) (local i32 i32)
    get_global 0
    set_local 3
    get_local 0
    set_local 2
    loop $label_1
      get_local 2
      i32.load8_u
      if
        get_local 3
        get_local 2
        i32.load8_u
        i32.store8
        get_local 3
        i32.const 1
        i32.add
        set_local 3
        get_local 2
        i32.const 1
        i32.add
        set_local 2
        br $label_1
      else
      end
    end
    get_local 1
    set_local 2
    loop $label_2
      get_local 2
      i32.load8_u
      if
        get_local 3
        get_local 2
        i32.load8_u
        i32.store8
        get_local 3
        i32.const 1
        i32.add
        set_local 3
        get_local 2
        i32.const 1
        i32.add
        set_local 2
        br $label_2
      else
      end
    end
    loop $label_0
      get_local 3
      i32.const 0
      i32.store8
      get_local 3
      i32.const 4
      i32.rem_s
      if
        get_local 3
        i32.const 1
        i32.add
        set_local 3
        br $label_0
      else
      end
    end
    get_global 0
    get_local 3
    i32.const 1
    i32.add
    set_global 0
  )

  (func $Std_digitToString (param i32) (result i32) 
    get_global 0
    get_local 0
    i32.const 48
    i32.add
    i32.store
    get_global 0
    get_global 0
    i32.const 4
    i32.add
    set_global 0
  )

  (func $Std_readString (result i32) 
    get_global 0
    get_global 0
    call $js_readString0
    set_global 0
  )

  (func $|| (param i32 i32) (result i32) 
    get_local 0
    get_local 1
    i32.or
  )

  (func $&& (param i32 i32) (result i32) 
    get_local 0
    get_local 1
    i32.and
  )

  (func $== (param i32 i32) (result i32) 
    get_local 0
    get_local 1
    i32.eq
  )

  (func $< (param i32 i32) (result i32) 
    get_local 0
    get_local 1
    i32.lt_s
  )

  (func $<= (param i32 i32) (result i32) 
    get_local 0
    get_local 1
    i32.le_s
  )

  (func $+ (param i32 i32) (result i32) 
    get_local 0
    get_local 1
    i32.add
  )

  (func $- (param i32 i32) (result i32) 
    get_local 0
    get_local 1
    i32.sub
  )

  (func $++ (param i32 i32) (result i32) 
    get_local 0
    get_local 1
    call $String_concat
  )

  (func $* (param i32 i32) (result i32) 
    get_local 0
    get_local 1
    i32.mul
  )

  (func $/ (param i32 i32) (result i32) 
    get_local 0
    get_local 1
    i32.div_s
  )

  (func $% (param i32 i32) (result i32) 
    get_local 0
    get_local 1
    i32.rem_s
  )

  (func $Std_printBoolean (param i32) (result i32) 
    get_local 0
    call $Std_booleanToString
    call $Std_printString
  )

  (func $Std_intToString (param i32) (result i32) (local i32 i32)
    get_local 0
    i32.const 0
    call $<
    if (result i32)
      get_global 0
      i32.const 0
      i32.add
      i32.const 45
      i32.store8
      get_global 0
      i32.const 1
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 2
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 3
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      get_global 0
      i32.const 4
      i32.add
      set_global 0
      i32.const 0
      get_local 0
      i32.sub
      call $Std_intToString
      call $++
    else
      get_local 0
      i32.const 10
      call $%
      set_local 1
      get_local 0
      i32.const 10
      call $/
      set_local 2
      get_local 2
      i32.const 0
      call $==
      if (result i32)
        get_local 1
        call $Std_digitToString
      else
        get_local 2
        call $Std_intToString
        get_local 1
        call $Std_digitToString
        call $++
      end
    end
  )

  (func $Std_booleanToString (param i32) (result i32) 
    get_local 0
    if (result i32)
      get_global 0
      i32.const 0
      i32.add
      i32.const 116
      i32.store8
      get_global 0
      i32.const 1
      i32.add
      i32.const 114
      i32.store8
      get_global 0
      i32.const 2
      i32.add
      i32.const 117
      i32.store8
      get_global 0
      i32.const 3
      i32.add
      i32.const 101
      i32.store8
      get_global 0
      i32.const 4
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 5
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 6
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 7
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      get_global 0
      i32.const 8
      i32.add
      set_global 0
    else
      get_global 0
      i32.const 0
      i32.add
      i32.const 102
      i32.store8
      get_global 0
      i32.const 1
      i32.add
      i32.const 97
      i32.store8
      get_global 0
      i32.const 2
      i32.add
      i32.const 108
      i32.store8
      get_global 0
      i32.const 3
      i32.add
      i32.const 115
      i32.store8
      get_global 0
      i32.const 4
      i32.add
      i32.const 101
      i32.store8
      get_global 0
      i32.const 5
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 6
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 7
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      get_global 0
      i32.const 8
      i32.add
      set_global 0
    end
  )

  (func $Arithmetic_pow (param i32 i32) (result i32) (local i32)
    get_local 1
    i32.const 0
    call $==
    if (result i32)
      i32.const 1
    else
      get_local 1
      i32.const 2
      call $%
      i32.const 0
      call $==
      if (result i32)
        get_local 0
        get_local 1
        i32.const 2
        call $/
        call $Arithmetic_pow
        set_local 2
        get_local 2
        get_local 2
        call $*
      else
        get_local 0
        get_local 0
        get_local 1
        i32.const 1
        call $-
        call $Arithmetic_pow
        call $*
      end
    end
  )

  (func $** (param i32 i32) (result i32) 
    get_local 0
    get_local 1
    call $Arithmetic_pow
  )

  (func $Arithmetic_gcd (param i32 i32) (result i32) 
    get_local 0
    i32.const 0
    call $==
    get_local 1
    i32.const 0
    call $==
    call $||
    if (result i32)
      get_local 0
      get_local 1
      call $+
    else
      get_local 0
      get_local 1
      call $<
      if (result i32)
        get_local 0
        get_local 1
        get_local 0
        call $%
        call $Arithmetic_gcd
      else
        get_local 0
        get_local 1
        call $%
        get_local 1
        call $Arithmetic_gcd
      end
    end
  )

  (func $^ (param i32 i32) (result i32) 
    get_local 0
    get_local 1
    call $Arithmetic_gcd
  )

  (func $<< (param i32 i32) (result i32) 
    get_local 1
    i32.const 0
    call $==
    if (result i32)
      get_local 0
    else
      i32.const 2
      get_local 0
      call $*
      get_local 1
      i32.const 1
      call $-
      call $<<
    end
  )
  (export "Arithmetic_main" (func $Arithmetic_main))
  (func $Arithmetic_main 
    get_global 0
    i32.const 0
    i32.add
    i32.const 83
    i32.store8
    get_global 0
    i32.const 1
    i32.add
    i32.const 104
    i32.store8
    get_global 0
    i32.const 2
    i32.add
    i32.const 111
    i32.store8
    get_global 0
    i32.const 3
    i32.add
    i32.const 117
    i32.store8
    get_global 0
    i32.const 4
    i32.add
    i32.const 108
    i32.store8
    get_global 0
    i32.const 5
    i32.add
    i32.const 100
    i32.store8
    get_global 0
    i32.const 6
    i32.add
    i32.const 32
    i32.store8
    get_global 0
    i32.const 7
    i32.add
    i32.const 98
    i32.store8
    get_global 0
    i32.const 8
    i32.add
    i32.const 101
    i32.store8
    get_global 0
    i32.const 9
    i32.add
    i32.const 32
    i32.store8
    get_global 0
    i32.const 10
    i32.add
    i32.const 52
    i32.store8
    get_global 0
    i32.const 11
    i32.add
    i32.const 58
    i32.store8
    get_global 0
    i32.const 12
    i32.add
    i32.const 0
    i32.store8
    get_global 0
    i32.const 13
    i32.add
    i32.const 0
    i32.store8
    get_global 0
    i32.const 14
    i32.add
    i32.const 0
    i32.store8
    get_global 0
    i32.const 15
    i32.add
    i32.const 0
    i32.store8
    get_global 0
    get_global 0
    i32.const 16
    i32.add
    set_global 0
    call $Std_printString
    drop
    i32.const 2
    i32.const 1
    i32.const 2
    call $**
    call $<<
    call $Std_printInt
    drop
  )
)