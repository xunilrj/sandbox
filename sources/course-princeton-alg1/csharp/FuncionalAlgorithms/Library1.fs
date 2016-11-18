namespace FuncionalAlgorithms

module QuickFind =
    type data = {Data: int[]}
    let init size = {Data =  Array.init size id}
    let union uf p q =
        let pid = uf.Data.[p]
        let qid = uf.Data.[q]
        seq { 1..uf.Data.Length-1 }
            |> Seq.iter (fun x -> if uf.Data.[x] = pid then uf.Data.[x] <- qid)
    let isConnected uf p q =
        uf.Data.[p] = uf.Data.[q]

module QuickUnion =
    type Data = {Data: int[]}
    let init size = {Data = Array.init size id}
    let rec root uf p =
        let rootp = uf.Data.[p]
        if rootp = p then p else root uf rootp
    let union uf p q = 
        let rootp = root uf p
        let rootq = root uf q
        uf.Data.[rootp] <- rootq
    let isConnected uf p q =
        let rootp = root uf p
        let rootq = root uf q
        rootp = rootq

module TestHelpers =
    let NonEqualPairs size = seq {
        for x in 0..(size-1) do
        for y in 0..(size-1) do
            if x <> y then yield (x,y)
    }
    let EqualPairs size = seq {
        for x in 0..(size-1) do
        for y in 0..(size-1) do
            if x = y then yield (x,y)
    }

module ListExtensionsTests = 
    open Xunit
    let AssertUF isConnected union uf =
        let assertIsConnected items =
            let result = items |> Seq.forall (fun (a,b) -> (isConnected uf a b) && (isConnected uf b a))
            Assert.True result
        let result =
            TestHelpers.NonEqualPairs 10 
            |> Seq.forall (fun (a,b) -> isConnected uf a b = false)
        Assert.True result
        let result = 
            TestHelpers.EqualPairs 10
            |> Seq.forall ((<||) <| isConnected uf)
        Assert.True result
        union uf 2 8
        [| (2,8) |] |> assertIsConnected
        Assert.True (isConnected uf 2 8)
        Assert.True (isConnected uf 8 2)
        union uf 1 8
        [| (1,8); (2,1) |] |> assertIsConnected
    [<Fact>] 
    let QuickFindTests() =
        let qfAssert = AssertUF QuickFind.isConnected QuickFind.union
        let uf = QuickFind.init 10
        qfAssert uf
    [<Fact>] 
    let QuickUnionTests() =
        let quAssert = AssertUF QuickUnion.isConnected QuickUnion.union
        let uf = QuickUnion.init 10
        quAssert uf
        
        