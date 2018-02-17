# -*- coding: utf-8 -*-
"""
Created on Wed May 24 02:30:33 2017

@author: Daniel Leite
"""

# -*- coding: utf-8 -*-
"""
Created on Sun May 21 19:46:36 2017

@author: xunil
"""

import sys
from collections import deque
from queue import PriorityQueue
#import resource

algo = sys.argv[1]
initialcsv = sys.argv[2]

#print("args")
#print("-------------------")
#print("algorithm: {0}".format(algo))
#print("initial state: {0}".format(initialcsv))

def printProgressBar (iteration, total, prefix = '', suffix = '', decimals = 1, length = 80, fill = '█'):
    percent = ("{0:." + str(decimals) + "f}").format(100 * (iteration / float(total)))
    filledLength = int(length * iteration // total)
    bar = fill * filledLength + '-' * (length - filledLength)
    print('\r%s |%s| %s%% %s %s' % (prefix, bar, percent, suffix, iteration), end = '\r')
    # Print New Line on Complete
    if iteration == total: 
        print()
    
w=3
def isGoal(y):
    return y == 912345678
def getMoves(state,strstate):
    zeroi = strstate.index("9") 
    zerox = zeroi % w
    zeroy = int(zeroi / w)
    if(zeroy > 0):
        yield 1#"Up"
    if(zeroy < (w-1)):
        yield 2#"Down"   
    if(zerox > 0):
        yield 3#"Left"
    if(zerox < (w-1)):
        yield 4#"Right"   
def swap(state,strstate,path,mov):
    zeroi = strstate.index("9")
    newzeroi=-1
    if(mov==1):
        newzeroi=zeroi-w
    if(mov==2):
        newzeroi=zeroi+w
    if(mov==3):
        newzeroi=zeroi-1
    if(mov==4):
        newzeroi=zeroi+1
    newzeronumber = int(strstate[newzeroi])
    tenzeroi = 10**(8-zeroi)
    tennewzeroi = 10**(8-newzeroi)
    return (state-(9*tenzeroi)+(newzeronumber*tenzeroi)-(newzeronumber*tennewzeroi)+(9*tennewzeroi),[])    
def getNeighbors(state,strstate,path):   
    return [swap(state,strstate,path, mov) for mov in getMoves(state,strstate)]
def isnotIn(d,x):
    return x not in d
def dist(real,expected):
    if(real==expected):return 0
    realx=real%w
    realy=int(real/w)
    expx=expected%w
    expy=int(expected/w)
    return abs(realx-expx)+abs(realy-expy)
def mandist(state,statestr):
    return sum([dist(int(statestr[x]),x) for x in range(0,8)])/2

total = (9*8*7*6*5*4*3*2*1)/2
expanded = -1
max_depth = 0
def getpath(target, parentmap):
    path = []
    curr = target
    while (curr != None):
        if(curr in parentmap):
            nextstate = parentmap[curr]
            next0 = str(nextstate).index("9")
            curr0 = str(curr).index("9")
            if(curr0 == next0+1):
                path.append(4)
            if(curr0 == next0-1):
                path.append(3)
            if(curr0 == next0-w):
                path.append(1)
            if(curr0 == next0+w):
                path.append(2)
            curr = nextstate
        else:
            curr = None
    return reversed(path)   
def bfs(initialState):
    global expanded
    global max_depth
    parentmap = {}
    frontierstate = deque([initialState])
    frontierset = set([initialState])
    frontierpath = deque([0])
    explored = set()
    while len(frontierstate) > 0:
        state = frontierstate.popleft()
        statestr=str(state)
        path = frontierpath.popleft()    
#        printProgressBar(expanded, total)
        expanded=expanded+1
        explored.add(state)
        frontierset.remove(state)
        if(path > max_depth):
            max_depth = path
        if(isGoal(state)):
            return (state,getpath(state,parentmap))
        for (newstate,newpath) in getNeighbors(state,statestr,path):
            if((newstate not in frontierset)and(newstate not in explored)):
                frontierstate.append(newstate)
                frontierset.add(newstate)
                frontierpath.append(path+1)
                parentmap[newstate] = state
    ([],[])
def dfs(initialState):
    global expanded
    global max_depth
    parentmap = {}
    frontierstate = deque([initialState])
    frontierset = set([initialState])
    frontierpath = deque([0])
    explored = set()
    while len(frontierstate) > 0:
        state = frontierstate.pop()
        statestr=str(state)
        path = frontierpath.pop()    
#        printProgressBar(expanded, total)
        expanded=expanded+1
        explored.add(state)
        frontierset.remove(state)
        if(path > max_depth):
            max_depth = path
        if(isGoal(state)):
            return (state,getpath(state,parentmap))
        for (newstate,newpath) in reversed(getNeighbors(state,statestr,path)):
            if((newstate not in frontierset)and(newstate not in explored)):
                frontierstate.append(newstate)
                frontierset.add(newstate)
                frontierpath.append(path+1)
                parentmap[newstate] = state
    ([],[])
def ast(initialstate):
    global expanded
    global max_depth
    parentmap = {}
    h = mandist
    explored=set()
    frontierset=set()
    frontier=PriorityQueue()    
    nextpriority = h(initialstate,str(initialstate))
    frontier.put((nextpriority,initialstate, 0))
    frontierset.add(initialstate)
    while(frontier.empty() == False):
        _,state,realcost = frontier.get()
        statestr=str(state)
#       printProgressBar(expanded, total)
        expanded=expanded+1
        explored.add(state)
        if(realcost > max_depth):
            max_depth = realcost
        if(isGoal(state)):
            return (state,getpath(state,parentmap))
        for (nextstate,nextpath) in getNeighbors(state,statestr,[]):
            if((nextstate not in explored)and(nextstate not in frontierset)):
                nextpriority = realcost+h(nextstate,statestr)
                frontier.put((nextpriority,nextstate,realcost+1))
                frontierset.add(nextstate)
                parentmap[nextstate] = state
    (0,[])
    
import time

deltat = 0
def runBFS(x):
    global deltat 
    start = time.perf_counter()
    result = bfs(x)
    end = time.perf_counter()
    deltat = end - start
    return result
def runDFS(x):
    global deltat 
    start = time.perf_counter()
    result = dfs(x)
    end = time.perf_counter()
    deltat = end - start
    return result
def runAST(x):
    global deltat 
    start = time.perf_counter()
    result = ast(x)
    end = time.perf_counter()
    deltat = end - start
    return result

initialpos = int(initialcsv.replace(",","").replace("0","9"))
result = {
  'bfs': lambda x:runBFS(x),
  'dfs': lambda x:runDFS(x),
  'ast': lambda x:runAST(x),
}[algo](initialpos)

def dirToName(x):
    if(x==1):
        return "Up"
    if(x==2):
        return "Down"
    if(x==3):return "Left"
    if(x==4):
        return "Right"

file_object  = open("output.txt", "w")
path = list(map(dirToName,result[1]))
file_object.write("path_to_goal: {0}\n".format(path))
file_object.write("cost_of_path: {0}\n".format(len(path)))
file_object.write("nodes_expanded: {0}\n".format(expanded))
file_object.write("search_depth: {0}\n".format(len(path)))
file_object.write("max_search_depth: {0}\n".format(max_depth))
file_object.write("running_time: {0:0.8f}\n".format(deltat))
#file_object.write("max_ram_usage: {0}".format(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss / 1000))
file_object.close()