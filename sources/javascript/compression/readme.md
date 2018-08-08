# Data Compression

All tutorials about data compression must start with the classic LZ77 algorithm. So we will first wrongly encode and decode the string "abracadabrad".

## Encoder/Decoder

The encoder/compressor will read the string/bytes  to compress and will emit a series of tuples (B,L,C).

B = Go back B bytes
L = Append the next L bytes
C = Append the byte C

for example:  

    0 0 a 0 0 b 0 0 r 3 1 c 2 1 d 7 4 d

We would decode like:

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
We start with pos equal zero. We try to find since the start of the string
the longest exactly match of characters starting at our current position.
The best example here is at:
   
    0 1 2 3 4 5 6 7 8 9 10 11 
    a b r a c a d a b r a  d
                  ^    
    Pos = 7
    Mathes
        [0, 4] = abra will generate tuple <7,4,d>
        [3, 1] = a will generate tuple <4,1,d>
        [5, 1] = a will generate tuple <2,1,d>

Since we want the best, the longest, we will chose the [0,4] and generate the tuple <7,4,d>.
When at position 7, copy 4 bytes from position 0 and append d. With this we have the code.

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

And the decompress we already did the step-by-step above. And the code is even simpler:

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

The more complicated part is the bestMatch function.

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

With this we have finished the lz77. A more realistic approach would works directly with bytes and files, off course.