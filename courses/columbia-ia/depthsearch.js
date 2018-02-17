function DepthFirstSearch(state, graph)
{
  state = Object.assign({}, state);
  if(state.frontier.length !== 0){
    var current = state.frontier.pop();
    state.explored.push(current);    
    if(current === state.goal){
      state.finished = true;
      state.success = true;
    }
    else{
      var neighbors = graph.neighbors(current);
      neighbors.sort();
      neighbors.reverse();      
      for(var i in neighbors){
        var n = neighbors[i];
        if(state.frontier.includes(n) === false&&state.explored.includes(n)===false){
          state.frontier.push(n)
        }
      }
    }
  }else
  {
    state.finished = true;
    state.success = false;
  }  
  return state;
}

function BreadthFirstSearch(state, graph)
{
  state = Object.assign({}, state);
  if(state.frontier.length !== 0){
    var current = state.frontier.shift();
    state.explored.push(current);    
    if(current === state.goal){
      state.finished = true;
      state.success = true;
    }
    else{
      var neighbors = graph.neighbors(current);
      neighbors.sort();   
      for(var i in neighbors){
        var n = neighbors[i];
        if(state.frontier.includes(n) === false&&state.explored.includes(n)===false){
          state.frontier.push(n)
        }
      }
    }
  }else
  {
    state.finished = true;
    state.success = false;
  }  
  return state;
}

function UniformCostSearch(state, graph)
{
  state = Object.assign({}, state);
  if(state.frontier.length !== 0){
    state.frontier.sort(function(a,b){return b.score-a.score;});
    var current = state.frontier.pop();
    state.explored.push(current);    
    if(current === state.goal){
      state.finished = true;
      state.success = true;
    }
    else{
      var neighbors = graph.neighbors(current);
      var isSameTarget = R.curry(function(n){
        return R.filter(function(x){
          return x.target == n.target;
      });});
      
      for(var i in neighbors){
        var inFrontier = R.find(isSameTarget(neighbors[i]), state.frontier);
        var inExplored = R.find(isSameTarget(neighbors[i]), state.explored);
        
        if(inFrontier && inExplored){
          state.frontier.push(neighbors[i])
        }else if(inFrontier){          
          inFrontier.score -= n.score;
        }
      }
    }
  }else
  {
    state.finished = true;
    state.success = false;
  }  
  return state;
}

function SearchStart(initial, goal){
  return state ={
    frontier:[initial],
    explored:[],
    finished:false,
    goal:goal
  };
}

var graph = {
  edges:[],
  neighbors:function(source){
    var l = [];
    for(var i in this.edges){
      if(source===this.edges[i].source){
        l.push({
          target:this.edges[i].target,
          score:this.edges[i].score
        });
      }      
    }
    return l;
  },
  add:function(str){
    var splitted = str.split(";")
    this.edges.push({
      source:splitted[0],
      target:splitted[1],
      score:splitted[2]
    });
    this.edges.push({
      source:splitted[1],
      target:splitted[0],
      score:splitted[2]
    });
  }
};
graph.add("Vancouver;Calgary;1");
graph.add("Vancouver;Seattle;1");
graph.add("Seattle;Calgary;1");
graph.add("Seattle;Portland;1");
graph.add("Seattle;Helena;1");
graph.add("Portland;San Francisco;1");
graph.add("Portland;Salt Lake City;1");
graph.add("San Francisco;Salt Lake City;1");
graph.add("San Francisco;Los Angeles;1");
graph.add("Los Angeles;Las Vegas;1");
graph.add("Los Angels;Phoenix;1");
graph.add("Los Angeles;El Paso;1");
graph.add("Calgary;Winnipeg;1");
graph.add("Calgary;Helena;1");
graph.add("Helena;Salt Lake City;1");
graph.add("Helena;Duluth;1");
graph.add("Helena;Omaha;1");
graph.add("Helena;Denver;1");
graph.add("Helena;Winnipeg;1");
graph.add("Salt Lake City;Las Vegas;1");
graph.add("Salt Lake City;Denver;1");
graph.add("Winnipeg;Sault Ste Marie;1");
graph.add("Winnipeg;Duluth;1");
graph.add("Duluth;Sault Ste Marie;1");
graph.add("Duluth;Chicago;1");
graph.add("Duluth;Omaha;1");
graph.add("Omaha;Chicago;1");
graph.add("Omaha;Denver;1");
graph.add("Denver;Kansas City;1");
graph.add("Denver;Santa Fe;1");
graph.add("Denver;Phoenix;1");
graph.add("Santa Fe;El Paso;1");
graph.add("Santa Fe;Oklahoma City;1");
graph.add("Santa Fe;Phoenix;1");
graph.add("El Paso;Dallas;1");
graph.add("Kansas City;Saint Louis;1");
graph.add("Kansas City;Oklahoma City;1");
graph.add("Oklahoma City;Little Rock;1");
graph.add("Little Rock;Saint Louis;1");
graph.add("Little Rock;Dallas;1");
graph.add("Little Rock;New Orleans;1");
graph.add("Little Rock;Nashville;1");
graph.add("Dallas;Houston;1");
graph.add("Houston;New Orleans;1");
graph.add("New Orleans;Miami;1");
graph.add("New Orleans;Atlanta;1");
graph.add("Atlanta;Miami;1");
graph.add("Atlanta;Charleston;1");
graph.add("Atlanta;Nashville;1");
graph.add("Atlanta;Raleigh;1");
graph.add("Miami;Charleston;1");
graph.add("Nashville;Saint Louis;1");
graph.add("Nashville;Raleigh;1");
graph.add("Raleigh;Charleston;1");
graph.add("Raleigh;Washington;1");
graph.add("Washington;Pittsburgh;1");
graph.add("Washington;New York;1");
graph.add("New York;Boston;1");
graph.add("New York;Montreal;1");
graph.add("Boston;Montreal;1");
graph.add("Montreal;Toronto;1");
graph.add("Montreal;Sault Ste Marie;1");
graph.add("Toronto;Sault Ste Marie;1");
graph.add("Toronto;Pittsburgh;1");
graph.add("Pittsburgh;Chicago;1");
graph.add("Chicago;Saint Louis;1");

console.log("-------------");
var state = UniformCostSearch("Pittsburgh","Saint Louis");
while(state.finished === false){
  state = UniformCostSearch(state, graph);
}
console.log(state);
console.log(state.explored.join(";"))
console.log(1);
console.log(2);
console.log(3);
console.log(4);