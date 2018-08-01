(module
 (type $0 (func (result i32)))
 (global $global$0 (mut i32) (i32.const 66560))
 (table 1 1 anyfunc)
 (memory $0 2)
 (export "memory" (memory $0))
 (func $main (; 0 ;) (type $0) (result i32)
  (i32.const 0)
 )
 ;; custom section "linking", size 3
)

