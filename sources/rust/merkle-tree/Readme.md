# Merkle Tree

https://en.wikipedia.org/wiki/Merkle_tree  
https://academic.microsoft.com/topic/205383636/publication/search?q=Merkle%20tree&qe=And(Composite(F.FId%253D205383636)%252CTy%253D%270%27)&f=&orderBy=0  
https://brilliant.org/wiki/merkle-tree  
https://xlinux.nist.gov/dads/HTML/MerkleTree.html  
https://golden.com/wiki/Merkle_tree-W3DMKV

## cargo bench

```
merkle/build_tree       time:   [280.90 ps 281.53 ps 282.37 ps]                              
Found 9 outliers among 100 measurements (9.00%)
  2 (2.00%) low mild
  2 (2.00%) high mild
  5 (5.00%) high severe
```

## cargo tarpaulin

```
Aug 15 16:36:39.657  INFO cargo_tarpaulin::report: Coverage Results:
|| Tested/Total Lines:
|| src/main.rs: 0/22 +0.00%
|| src/merkle.rs: 123/133 +0.00%
|| 
79.35% coverage, 123/155 lines covered
```