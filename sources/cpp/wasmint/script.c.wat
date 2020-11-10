(module
  (type (;0;) (func (param i32 i32)))
  (type (;1;) (func (param i32 i32 i32) (result i32)))
  (type (;2;) (func (param i32) (result i32)))
  (type (;3;) (func (param i32 i32) (result i32)))
  (type (;4;) (func))
  (import "env" "Print" (func $Print (type 0)))
  (import "env" "WalkTo" (func $WalkTo (type 1)))
  (import "env" "Wait" (func $Wait (type 2)))
  (import "env" "StartTalk" (func $StartTalk (type 3)))
  (func $Look_CannonFoddler (type 4)
    i32.const 6
    i32.const 1024
    call $Print
    i32.const 0
    i32.const 281
    i32.const 329
    call $WalkTo
    call $Wait
    drop
    i32.const 0
    i32.const 0
    call $StartTalk
    call $Wait
    drop)
  (table (;0;) 1 1 anyfunc)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66576))
  (export "memory" (memory 0))
  (export "Look_CannonFoddler" (func $Look_CannonFoddler))
  (data (i32.const 1024) "Daniel\00"))
