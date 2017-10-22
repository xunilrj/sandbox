open System
open FsCheck

let IsCommutative f x y = 
        let a = f x y
        let b = f y x
        a = b
let IsAssociative f x y z = 
        let a = f x (f y z)
        let b = f (f x y) z
        a = b
let HasLeftIdentity f id x = f id x = x
let HasRightIdentity f id x = f x id = x
let HasInverseElement f inv zero x = 
    let a = f x (inv x)
    let b = f (inv x) x
    (a = (zero)) && (b = (zero))

let HasClosure (f: 'a -> 'a -> 'a) = true

module Number =
    type N = INT of int
    let zeroN = INT 0
    let addN (INT l) (INT r) = INT (l + r)
    let invN (INT x) = INT -x

let IsGroup dot zero inv =
    Check.Quick <| HasClosure dot
    Check.Quick <| IsAssociative dot
    Check.Quick <| HasLeftIdentity dot zero
    Check.Quick <| HasRightIdentity dot zero
    Check.Quick <| HasInverseElement dot inv zero

module MonadI =
    type M<'a> = I of 'a
    let unitI = I
    let bindI (I a) f = I <| f a

module MonadE = 
    type M<'a> = Mok of 'a | Mfail of String
    let unitE = Mok
    let failE = Mfail
    let bindE m f = 
        match m with
        | Mok v -> f v
        | Mfail s -> Mfail s

[<EntryPoint>]
let main argv =
    IsGroup Number.addN Number.zeroN Number.invN
    // Check.QuickAll<AddSpecification>()
    0 // return an integer exit code
