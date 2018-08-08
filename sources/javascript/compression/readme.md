# Data Compression

Compression is one of those subjects that always scare developers. Specially those used to just use libraries. It is a set of black boxes that, hopefully, just works.

This does not need to be the case. And to prove it we will implement here the most important (but not used anymore) of these algorithms, the LZ77. In 1977, two Israeli mathematicians/computer scientists created this algorithm:

A Universal Algorithm for Sequential Data Compression  
Jacob Ziv, Abraham Lempel  
http://citeseer.ist.psu.edu/viewdoc/download?doi=10.1.1.118.8921&rep=rep1&type=pdf  
  
https://en.wikipedia.org/wiki/Yaakov_Ziv  
https://en.wikipedia.org/wiki/Abraham_Lempel  
  
## Encoder/Decoder

The encoder/compressor will read the string/bytes to compress and will emit a series of tuples (B,L,C).

B = Go back B bytes
L = Append the next L bytes
C = Append the byte C

for example compress("abracadabrad") would generate.

    0 0 a 0 0 b 0 0 r 3 1 c 2 1 d 7 4 d

This means that the decoder would do:

    Pos = 0, Buffer = []
    Pass 1:        
        go back 0
        append 0 bytes
        append a
        Pos = 1, Buffer = [a]
    Pass 2:
        go back 0
        append 0 bytes
        append b
        Pos = 2, Buffer = [ab]
    Pass 3:
        go back 0
        append 0 bytes
        append r
        Pos = 3, Buffer = [abr]
    Pass 4:
        go back 3 (appendPos = Pos - 3 = 0)
        append 1 bytes (Buffer = Buffer + Buffer[0])
        append c
        Pos = 5, Buffer = [abrac]
    Pass 5:
        go back 2 (appendPos = Pos - 2 = 3)
        append 1 byte (Buffer = Buffer + Buffer[3])
        append d
        Pos = 7 = Buffer = [abracad]
    Pass 6:
        go back 7 (appendPos = Pos - 7 = 0)
        append 4 bytes 
        append d
        Pos = 12, Buffer = [abracadabrad]

## Algorithm 

With this in mind we can tackle the algorithm.  
We start with "pos" equals zero. Then we try to find the longest sequence that exactly
match the characters ahead, starting at our current position.
The best example here is at:
   
    0 1 2 3 4 5 6 7 8 9 10 11 
    a b r a c a d a b r a  d
                  ^    
    Pos = 7
    Possible matches:  
        [pos 0, length 4] = "abra" will match [pos 7, length 4] and will generate tuple <7,4,d>
        [pos 3, length 1] = "a" will match [pos 7, length 1] and will generate tuple <4,1,d>
        [pos 5, length 1] = "a" will match [pos 7, length 1] and will generate tuple <2,1,d>

Since we want the best, the longest match, we will choose the [0,4] and generate the tuple <7,4,d>.
When at position 7, copy 4 bytes from position 0 and append "d".
With this we have the code for compress as:

    compress = function (str) {
      var encode = [];
      var pos = 0;
      while (pos < str.length) {
        var best = findBestMatch(str, pos);
        encode.push(best);
        pos += best[1] + 1;
      }
      return {
        original: str,
        encode: encode
      };
    }

That will generate:

    {
        "original": "abracadabrad",
        "encode": [
            [0,0,"a"],
            [0,0,"b"],
            [0,0,"r"],
            [3,1,"c"],
            [2,1,"d"],
            [7,4,"d"]
        ]
    }

And for the decompress, as we already did the step-by-step above, we have:

    decompress = function(encodeArray) {
        var str = "";
        var pos = 0;
        for(var i = 0;i<encodeArray.length;++i) {
            var current = encodeArray[i];
            var appendPos = pos - current[0];
            for(var j = 0; j < current[1];++j) {
                str += str[appendPos + j];
                pos++;
            }
            str+=current[2];
            pos++;
        }
        return str;
    }

This left us just the code for finding the best match. Here I search backwards, from "pos - 1) to the beggining.

    const findBestMatch = function (str, pos) {
        var originalPos = pos;
        var best = [0, 0, str[pos]]
        if (pos == 0) return best;
        pos--;
        while (pos >= 0) {
        var count = 0;
        var currentPos = pos;
        var aheadPos = originalPos;

        while (str[currentPos] == str[aheadPos]) {
            if (aheadPos > str.length) break;
            count++;
            currentPos++;
            aheadPos++;
        }

        if (count > best[1]) {
            best = [originalPos - pos, count, str[aheadPos]];
        }

        --pos;
        }
        return best;
    }