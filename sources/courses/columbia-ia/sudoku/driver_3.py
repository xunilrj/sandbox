#!/usr/bin/python3

class SudokuBoard:
    board = {}
    def parse(self,board):
        row = 'A'
        column = '1'
        for c in board:
            key = "%s%s" % (row, column)
            self.board[key] = int(c)            
            column = chr(ord(column) + 1)
            if(column == ':'):
                column = '1'
                row = chr(ord(row) + 1)
    def getValue(self,node):
        return self.board[node]
        

    
class CSP:
    nodes = {}
    arcs = []
    def addNode(self,node,domain):
        self.nodes[node] = list(domain)
    def getArcs(self):
        return self.arcs
    def getNodeArcs(self,node):
        return [(xi,xj,t) for xi,xj,t in self.arcs if (xi == node or xj == node)]
    def getNodeArcsWithout(self,node, toremove):
        return [(xi,xj,t) for xi,xj,t in self.arcs if (xi == node or xj == node) and (xi != toremove and xj != toremove)]
    def Domain(self, node):
        return self.nodes[node].copy()
    def deleteFromDomain(self,node,value):
        self.nodes[node].remove(value)
    def AddDiff(self,nodeA,nodeB):
        self.arcs.append((nodeA,nodeB,'diff'))
    def AddAllDiff(self,nodes):
        
        return []
    def isSatisfiedBy(self,nodeA,nodeB,va,vb):
        def satisfy(nodea,nodeb,typ):
            if(typ == "diff"):
                return va != vb
            return False            
        arcs = [(xi,xj,t) for xi,xj,t in self.arcs if (xi == nodeA and xj == nodeB) or (xi == nodeB and xj == nodeA)]
        satisfied = [satisfy(a,b,t) for a,b,t in arcs]
        return all(satisfied)
    def isDomainEmpty(self,node):
        return len(self.nodes[node]) == 0
        
import queue

def AC3(csp):
    def CanXiXjBeSatisfied(csp, Xi, Xj, x):
        Dj = csp.Domain(Xj)
        return any([csp.isSatisfiedBy(Xi,Xj,x,y) for y in Dj])
    def Revise(csp,Xi,Xj):
        revised = False
        Di = list(csp.Domain(Xi))
        for x in Di:
            if(CanXiXjBeSatisfied(csp, Xi, Xj, x) == False):
                print("          Cannot satisfy (%s,%s) with %s - deleting %s from %s" % (Xi,Xj,x,x,Xi))
                csp.deleteFromDomain(Xi, x)
                revised = True
#        print("Domain of %s")
        return revised
    q = queue.Queue()
    for xi,xj,t in csp.getArcs():
        q.put((xi,xj,t))
    while (q.empty() == False):
        Xi, Xj, t = q.get()
        print("Arc %s %s %s" % (Xi,Xj,t))
        print("     %s Domain %s" % (Xi,csp.Domain(Xi)))
        print("     %s Domain %s" % (Xj,csp.Domain(Xj)))
        if(Revise(csp,Xi,Xj)):
            if(csp.isDomainEmpty(Xi)):
                print("     Arc modified is empty. Returning False.")
                return False
            print("     Arc modified. Expanding its children.")
            for a,b,t in csp.getNodeArcsWithout(Xi,Xj):
                print("          Expanding (%s,%s,%s)" % (a,b,t))
                q.put((a,b,t))
        else:
            print("     Arc not modified.")
        print("     New %s Domain %s" % (Xi,csp.Domain(Xi)))
        print("     New %s Domain %s" % (Xj,csp.Domain(Xj)))
    return True

board = SudokuBoard()
#            A23456789B23456789C
board.parse("000000000302540000050301070000000004409006005023054790000000050700810000080060009")

csp = CSP()
for row in range(65,74):
    for column in range(1,10):
        key = "%s%d" % (chr(row),column)
        value = board.getValue(key)
        if(value == 0):
            csp.addNode(key, range(1,10))
        else:
            csp.addNode(key, [value])
            
import itertools
#rows must be different
for row in range(65,74):
    for xi,xj in itertools.permutations(["%s%d" % (chr(row),column) for column in range(1,10)], 2):
        value = board.getValue(xi)
        if(value == 0):
            csp.AddDiff(xi,xj)
#columns must be different
for column in range(1,10):
    for xi,xj in itertools.permutations(["%s%d" % (chr(row),column) for row in range(65,74)], 2):
        value = board.getValue(xi)
        if(value == 0):
            csp.AddDiff(xi,xj)
#box 1
def boxArc(rowstart,rowend,colstart,colend):
    boxcells = ["%s%s" % (chr(row),column) for row in range(rowstart,rowend) for column in range(colstart,colend) ]
    for xi,xj in itertools.permutations(boxcells, 2):
            value = board.getValue(xi)
            if(value == 0):
                csp.AddDiff(xi,xj)
                
boxArc(65,68,1,4)
boxArc(65,68,4,7)
boxArc(65,68,7,10)

boxArc(68,71,1,4)
boxArc(68,71,4,7)
boxArc(68,71,7,10)

boxArc(71,74,1,4)
boxArc(71,74,4,7)
boxArc(71,74,7,10)
 
print("Before")
print("---------------------------------")
print(csp.nodes)
print("---------------------------------")
result = AC3(csp)
print("---------------------------------")
print("After")
print("---------------------------------")
print(csp.nodes)
print("---------------------------------")
print(result)