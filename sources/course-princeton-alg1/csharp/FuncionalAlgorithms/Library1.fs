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
    let doubleBackPipe f (a,b) = f a b

module ListExtensionsTests = 
    open Xunit    
    [<Fact>] 
    let QuickFindTests() =
        let AssertUF uf =
            let result =
                TestHelpers.NonEqualPairs 10 
                |> Seq.forall (fun (a,b) -> QuickFind.isConnected uf a b = false)
            Assert.True result
            let result = 
                TestHelpers.EqualPairs 10
                |> Seq.forall (TestHelpers.doubleBackPipe <| QuickFind.isConnected uf)
            Assert.True result
            QuickFind.union uf 2 8
            Assert.True (QuickFind.isConnected uf 2 8)
            Assert.True (QuickFind.isConnected uf 8 2)
            QuickFind.union uf 1 8
            Assert.True (QuickFind.isConnected uf 1 8)
            Assert.True (QuickFind.isConnected uf 8 1)
            Assert.True (QuickFind.isConnected uf 2 1)
            Assert.True (QuickFind.isConnected uf 1 2)
        let uf = QuickFind.init 10
        AssertUF uf
    [<Fact>] 
    let QuickUnionTests() =
        let AssertUF uf =
            let result =
                TestHelpers.NonEqualPairs 10 
                |> Seq.forall (fun (a,b) -> QuickUnion.isConnected uf a b = false)
            Assert.True result
            let result = 
                TestHelpers.EqualPairs 10
                |> Seq.forall (TestHelpers.doubleBackPipe <| QuickUnion.isConnected uf)
            Assert.True result
            QuickUnion.union uf 2 8
            Assert.True (QuickUnion.isConnected uf 2 8)
            Assert.True (QuickUnion.isConnected uf 8 2)
            QuickUnion.union uf 1 8
            Assert.True (QuickUnion.isConnected uf 1 8)
            Assert.True (QuickUnion.isConnected uf 8 1)
            Assert.True (QuickUnion.isConnected uf 2 1)
            Assert.True (QuickUnion.isConnected uf 1 2)
        let uf = QuickUnion.init 10
        AssertUF uf
        
        