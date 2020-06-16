# Original Paper

```
The Google SSTable file format is used internally to
store Bigtable data. An SSTable provides a persistent,
ordered immutable map from keys to values, where both
keys and values are arbitrary byte strings. 

Operations are provided to look up the value associated 
with a specified key, and to iterate over all key/value 
pairs in a specified key range.

Internally, each SSTable contains a sequence
of blocks (typically each block is 64KB in size, but this
is configurable). 

A block index (stored at the end of the
SSTable) is used to locate blocks; the index is loaded
into memory when the SSTable is opened. A lookup
can be performed with a single disk seek: we first find
the appropriate block by performing a binary search in
the in-memory index, and then reading the appropriate
block from disk. Optionally, an SSTable can be completely 
mapped into memory, which allows us to perform
lookups and scans without touching disk.
```

https://storage.googleapis.com/pub-tools-public-publication-data/pdf/68a74a85e1662fe02ff3967497f31fda7f32225c.pdf

![SSTable](https://www.igvita.com/posts/12/xsstable.png.pagespeed.ic.IkMoqaKZX9.webp "SSTable")

```
A “Sorted String Table” then is exactly what it sounds 
like, it is a file which contains a set of arbitrary, 
sorted key-value pairs inside. 

Duplicate keys are fine, there is no need for “padding” 
for keys or values, and keys and values are arbitrary 
blobs. 

Read in the entire file sequentially and you have a 
sorted index. 

Optionally, if the file is very large, we can also 
prepend, or create a standalone key:offset index for 
fast access. 

That’s all an SSTable is: very simple, but also a 
very useful way to exchange large, sorted data segments.
```

https://www.igvita.com/2012/02/06/sstable-and-log-structured-storage-leveldb  