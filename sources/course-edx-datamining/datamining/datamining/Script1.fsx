#r "C:/github/xunilrj-sandbox/sources/course-edx-datamining/datamining/packages/FSharp.Data.2.3.2/lib/net40/FSharp.Data.dll"
open FSharp.Data
type Iris = CsvProvider<"C:/github/xunilrj-sandbox/sources/course-edx-datamining/datamining/datamining/iris.txt">
let iris = Iris.Load("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data")
let getPetalWidth x = x.PetalWidth
iris.Rows |> Seq.map getPetalWidth |> Seq.sum