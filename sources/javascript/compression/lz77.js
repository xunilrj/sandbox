function LZ77() {
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
    this.compress = function (str) {
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
    this.decompress = function(encodeArray) {
      var str = "";
      var pos = 0;
      for(var i = 0;i<encodeArray.length;++i){
        var current = encodeArray[i];
        var appendPos = pos - current[0];
        for(var j = 0; j < current[1];++j){
          str += str[appendPos + j];
          pos++;
        }
        str+=current[2];
        pos++;
      }
      return str;
    }
  }
  
  // Write Javascript code!
  const original = "abracadabrad";
  const lz77 = new LZ77();
  const compressResult = lz77.compress(original);
  const decompressedResult = lz77.decompress(compressResult.encode);
  const appDiv = document.getElementById('app');
  appDiv.innerHTML = JSON.stringify(decompressedResult == original, null, 4);