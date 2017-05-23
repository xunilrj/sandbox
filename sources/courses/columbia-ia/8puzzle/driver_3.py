# -*- coding: utf-8 -*-
"""
Created on Sun May 21 19:46:36 2017

@author: xunil
"""

import sys
from collections import deque
import copy
#import resource

algo = sys.argv[1]
initialcsv = sys.argv[2]

print("args")
print("-------------------")
print("algorithm: {0}".format(algo))
print("initial state: {0}".format(initialcsv))

def printProgressBar (iteration, total, prefix = '', suffix = '', decimals = 1, length = 80, fill = '█'):
    percent = ("{0:." + str(decimals) + "f}").format(100 * (iteration / float(total)))
    filledLength = int(length * iteration // total)
    bar = fill * filledLength + '-' * (length - filledLength)
    print('\r%s |%s| %s%% %s %s' % (prefix, bar, percent, suffix, iteration), end = '\r')
    # Print New Line on Complete
    if iteration == total: 
        print()

initialpos = [int(x) for x in initialcsv.split(",")]

w=3
def isGoal(x):
    return all(map(lambda xx: xx[0]==xx[1], zip(range(0,9),list(x[0]))))
def getMoves(x):
    zeroi = x[0].index(0)
    zerox = zeroi % w
    zeroy = int(zeroi / w)
    if(zeroy > 0):
        yield "Up"
    if(zerox > 0):
        yield "Left"
    if(zerox < (w-1)):
        yield "Right"    
    if(zeroy < (w-1)):
        yield "Down"   
def swap(x,mov):
    newstate = copy.deepcopy(x[0])
    zeroi = newstate.index(0)
    newzeroi = 0
    if(mov == "Right"):
        newzeroi = zeroi + 1
    if(mov == "Left"):
        newzeroi = zeroi - 1
    if(mov == "Down"):
        newzeroi = zeroi + w
    if(mov == "Up"):
        newzeroi = zeroi - w
    temp = newstate[newzeroi]
    newstate[newzeroi] = newstate[zeroi]
    newstate[zeroi] = temp
    return (newstate, x[1] + [mov])
def getNeighbors(x):    
    return [swap(x, mov) for mov in getMoves(x)]
def isnotIn(d,x):
    return all(i[0] != x[0] for i in d)   

total = (9*8*7*6*5*4*3*2*1)
expanded = 0
max_depth = 0
def bfs(initialState):
    global expanded
    global max_depth
    frontier = deque([(initialState,[])])
    explored = deque()
    while len(frontier) > 0:
        #printProgressBar(expanded, total)
        expanded=expanded+1
        state = frontier.popleft()
        explored.append(state)
        
        if(len(state[1]) > max_depth):
            max_depth = len(state[1])
        
        if(isGoal(state)):
            return state
        for node in getNeighbors(state):
            if isnotIn(frontier, node) and isnotIn(explored, node):
                frontier.append(node)
    ([],[])
    
import time

deltat = 0
def runBFS(x):
    global deltat 
    start = time.time()
    result = bfs(x)
    end = time.time()
    deltat = end - start
    return result

result = {
  'bfs': lambda x:runBFS(x),
}[algo](initialpos)

path = list(map(lambda x: x,result[1]))
print("path_to_goal: {0}".format(path))
print("cost_of_path: {0}".format(len(path)))
print("nodes_expanded: {0}".format(expanded))
print("search_depth: {0}".format(len(path)))
print("max_search_depth: {0}".format(max_depth))
print("runningtime: {0}".format(deltat))
#print("max_ram_usage: {0}".format(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss / 1000))