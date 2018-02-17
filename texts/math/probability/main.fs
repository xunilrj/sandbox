type Person = {Name:string;Sex:int}

let Main=
  let population = [ 
    {Name="Stephen";Sex=0};
    {Name="Conrad";Sex=0};
    {Name="Michael";Sex=0};
    {Name="Carlos";Sex=0};
    {Name="Maeve";Sex=1};
    {Name="Dawn";Sex=1}
  ]
  
  let (./) a b = (float a) / (float b)
  let setSize = List.length
  
  let isMale x = x.Sex = 0
  let isFemale x = x.Sex = 1
  let startsWithM x = x.Name.StartsWith("M")
  let notStartsWithM x =  not (startsWithM x)
  
  let filterMales = List.filter isMale
  let filterFemales = List.filter isFemale
  let filterStartM = List.filter startsWithM
  let filterNotStartM = List.filter notStartsWithM
  
  let males = filterMales population
  let females = filterFemales population
  let withM = filterStartM population
  let notWithM = filterNotStartM population
  
  let populationSize = setSize population
  let malesSize = setSize males
  let femalesSize = setSize females
  let withMSize = setSize withM
  let notWithMSize = setSize notWithM 
  
  printf "Absolute Probabilities\n"
  printf "MALES/POPULATION %f\n" (malesSize ./ populationSize)
  printf "FEMALES/POPULATION %f\n" (femalesSize ./ populationSize)
  printf "START WITH M/POPULATION %f\n" (withMSize ./ populationSize)
  printf "NOT START WITH M/POPULATION %f\n" (notWithMSize ./ populationSize)
  
  printf "Conditional Probabilities\n"
  printf "STARTS M / FEMALE %f\n" (setSize (filterStartM females) ./ femalesSize)
  System.Console.WriteLine("")