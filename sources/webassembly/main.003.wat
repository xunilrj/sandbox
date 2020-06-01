(module
  (type (;0;) (func (param f32 f32 f32) (result f32 f32 f32)))
  (func $make_vec3f_float__float__float_ (type 0) (param f32 f32 f32)
    local.get 0
    local.get 1
    local.get 2)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "make_vec3f" (func $make_vec3f_float__float__float_)))
