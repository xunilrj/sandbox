// Learn more about F# at http://fsharp.org

open System
open System.Security.Principal
open System.Threading.Tasks

[<Measure>] type Length;

let mm (x:float) : float<Length> = x/1000.0 |> LanguagePrimitives.FloatWithMeasure
let cm (x:float) : float<Length> = x/100.0 |> LanguagePrimitives.FloatWithMeasure
let dm (x:float) : float<Length> = x/100.0 |> LanguagePrimitives.FloatWithMeasure
let m (x:float) : float<Length> = x |> LanguagePrimitives.FloatWithMeasure

type Radius<[<Measure>] 'u> = float<'u> 
type Side<[<Measure>] 'u> = float<'u>
type Vertex<[<Measure>] 'u> = float<'u> * float<'u>
type Shape<[<Measure>] 'u> = 
    | Rectangle of Side<'u>  * Side<'u>  
    | Ellipse of Radius<'u>  * Radius<'u>  
    | RtTriangle of Side<'u>  * Side<'u>  
    | Polygon of Vertex<'u>  list

//Exercise 2.2 Define a function regularPolygon :: Int-* Side-* Shape 
//such that regularPolygon n s is a regular polygon with n sides, each of 
//length s. (Hint: Consider using some of Haskell's trigonometric functions, 
//such as sin :: Float — Float, cos :: Float-^Float, and tan :: Float — Float.) 

let sqrt (x : float<'u^2>) : float<'u> = (float x) ** 0.5 |> LanguagePrimitives.FloatWithMeasure

[<Literal>]
let pi = System.Math.PI

let distance (x1, y1) (x2, y2)  =
    let x1x2 = x1-x2
    let y1y2 = y1-y2    
    sqrt (x1x2*x1x2 + y1y2*y1y2)

let areaTriangle v1 v2 v3 =
    let a = distance v1 v2 
    let b = distance v2 v3 
    let c = distance v3 v1 
    let s = 0.5 * (a + b + c) 
    sqrt (s * (s - a) * (s - b) * (s - c)) 

let rec areaPolygon vs = 
    match vs with
    | v1::v2::v3::rest ->
        let currentArea = areaTriangle v1 v2 v3
        currentArea + areaPolygon (v3::rest)
    | _ -> 0.0<Length^2>

let area = function
    | Rectangle (w,h) -> w*h
    | Ellipse (r1,r2) -> pi*r1*r2
    | RtTriangle (s1,s2) -> (s1*s2) / 2.0
    | Polygon vs -> areaPolygon vs

//Exercise 2.4 Define a function convex :: Shape — Bool that determines 
//whether or not its argument is a convex shape (although we are mainly 
//interested in the convexity of polygons, you might as well define it for 
//each kind of shape). 

//Exercise 2.5 Here is an alternative way to compute the area of a polygon. 
//Consider a polygon in quadrant 1 of the Cartesian plane (i.e., every vertex 
//has positive x and y coordinates). Then every7 pair of adjacent vertices 
//forms a trapeziod with respect to the x-axis. Starting at any vertex and 
//working clockwise, compute these areas one-by-one, counting the area 
//as positive if the x-coordinate increases, and negative if it decreases. The 
//sum of these areas is then the area of the polygon. 

module LinearAlgebra =
    type Matrix = 
        | Matrix of int * int * float list
        | Column of int * float list
        // | Row of int * float list        
        | Identity
        // | Diagonal of float list
        // | UpperTriangular of float * float * float list
    let show = function
        | Matrix (n,m,values) -> values |> List.iter (printf "%f ")
        | Column (n,values) -> values |> List.iter (printf "%f ")
        | _ -> printfn ""
    let at columns row column = (row*columns)+column
    let (*) l r = 
        match (l,r) with
        | (Matrix (lrows,lcolumns,lvalues), Column (rm, rvalues)) ->
            let at = at lcolumns
            let r = Array.zeroCreate lrows
            for i in 0..(lrows-1) do
                for j in 0..(lcolumns-1) do
                    r.[i] <- r.[i] + lvalues.[at i j]*rvalues.[j]
            Column (lrows, List.ofArray r)
        | _ -> Identity


open LinearAlgebra

[<EntryPoint>]
let main argv =
    let a = area (Rectangle (m 1.0, m 1.0))
    let b = area (Rectangle (cm 1.0, cm 1.0))
    printfn "Hello World from F#!"
    //LINEAR ALGEBRA
    
    let lmatrix = Matrix(2, 3, [1.0;2.0;3.0;4.0;5.0;6.0])
    let rcolumn = Column (3, [7.0;8.0;9.0])
    let result = lmatrix*rcolumn
    show result
    0 // return an integer exit code
