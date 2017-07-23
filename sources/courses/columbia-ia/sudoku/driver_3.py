#!/usr/bin/python3

import sys
import copy

def printProgressBar (iteration, total, prefix = '', suffix = '', decimals = 1, length = 100, fill = '█'):
    """
    Call in a loop to create terminal progress bar
    @params:
        iteration   - Required  : current iteration (Int)
        total       - Required  : total iterations (Int)
        prefix      - Optional  : prefix string (Str)
        suffix      - Optional  : suffix string (Str)
        decimals    - Optional  : positive number of decimals in percent complete (Int)
        length      - Optional  : character length of bar (Int)
        fill        - Optional  : bar fill character (Str)
    """
    percent = ("{0:." + str(decimals) + "f}").format(100 * (iteration / float(total)))
    filledLength = int(length * iteration // total)
    bar = fill * filledLength + '-' * (length - filledLength)
    print('\r%s |%s| %s%% %s' % (prefix, bar, percent, suffix), end = '\r')
    # Print New Line on Complete
    if iteration == total: 
        print()

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
    rootarcs = {}
    fromarcs ={}
    toarcs ={}
    def addNode(self,node,domain):
        self.nodes[node] = list(domain)
    def getArcs(self):
        return self.arcs
    def getNodeArcs(self,node):
        return self.fromarcs[node].union(self.toarcs[node])
    def getNodeArcsWithout(self,node, toremove):
        #{ (z, x) | z != y and there exists a relation R2(x, z) or a relation R2(z, x) }
#        return [(xi,xj,t) for xi,xj,t in self.arcs if (xi == node or xj == node) and (xi != toremove and xj != toremove)]
        return [(xi,xj,t) for xi,xj,t in self.arcs if (xi == node and xj != toremove) or (xi != toremove and xj == node)]
    def Domain(self, node):
        return self.nodes[node].copy()
    def DomainMRV(self,node):
        return self.nodes[node].copy()
    def Assign(self,node,value, propagate = False):
        def propagate(node,value):
            if(node in self.fromarcs):
                arcotherside = [b for a,b,typ in self.fromarcs[node]]
                for x in arcotherside:
                    if(self.deleteFromDomain(x,value)):
                        propagate(x, value)
            if(node in self.toarcs):
                arcotherside = [a for a,b,typ in self.toarcs[node]]
                for x in arcotherside:
                    if(self.deleteFromDomain(x,value)):
                        propagate(x, value)
        self.nodes[node] = value
        if(propagate):
            propagate(node,value)        
    def deleteFromDomain(self,node,value):
        if(value in self.nodes[node]):
            self.nodes[node].remove(value)
            return True
        return False
    def AddDiff(self,nodeA,nodeB):
        self.arcs.append((nodeA,nodeB,'diff'))
        key = "%s%s" % (nodeA,nodeB)
        if(key in self.rootarcs):
            self.rootarcs[key].append((nodeA,nodeB,'diff'))
        else:
            self.rootarcs[key] = [(nodeA,nodeB,'diff')]
        #fromdic
        if(nodeA in self.fromarcs):
            self.fromarcs[nodeA].append((nodeA,nodeB,'diff'))
        else:
            self.fromarcs[nodeA] = [(nodeA,nodeB,'diff')]
        #todic    
        if(nodeB in self.toarcs):
            self.toarcs[nodeB].append((nodeA,nodeB,'diff'))
        else:
            self.toarcs[nodeB] = [(nodeA,nodeB,'diff')]        
    def isNodeSatisfiedBy(self,nodeA,va):
        def satisfy(nodea,nodeb,typ):
            db = self.Domain(nodeb)
            return (len(db) == 1 and db[0] == va) == False
        satisfiedfrom = [satisfy(a,b,typ) for a,b,typ in self.fromarcs[nodeA]]
        satisfiedto = [satisfy(a,b,typ) for a,b,typ in self.toarcs[nodeA]]
        return all(satisfiedfrom) and all(satisfiedto)
    def isSatisfiedBy(self,nodeA,nodeB,va,vb):
        def satisfy(nodea,nodeb,typ):
            if(typ == "diff"):
                return va != vb
            return False
        key =("%s%s"%(nodeA,nodeB))
        fromnodeA = True
        if(key in self.rootarcs):
            fromnodeA = all([satisfy(a,b,t) for a,b,t in self.rootarcs[key]])
        key =("%s%s"%(nodeB,nodeA))
        fromnodeB = True
        if(key in self.rootarcs):
            fromnodeB = all([satisfy(a,b,t) for a,b,t in self.rootarcs[key]])
        return fromnodeA and fromnodeB
    def isDomainEmpty(self,node):
        return len(self.nodes[node]) == 0
    def isSolved(self):
        return all([len(self.nodes[x]) == 1 for x in self.nodes])
    def solution(self):
        return "".join([str(self.nodes["%s%d" % (chr(row),column)][0]) for row in range(65,74) for column in range(1,10)])                
        
class Assignment:
    nodes = {}
    csp = None
    def __init__(self, csp):
        def getvalue(v):
            if(len(v) == 1):
                return v[0]
            return 0
        self.csp = csp
        self.nodes = copy.deepcopy(csp.nodes)
    def createChild(self):
        newassignment = Assignment(csp)
        newassignment.nodes = copy.deepcopy(self.nodes)
        return newassignment
    def isSolved(self):
        def value(v):
            if(len(v) == 1):
                return 1
            else:
                return 0
        count = sum([value(self.nodes[x]) for x in self.nodes])
        printProgressBar(count,81)
        return count == 81
    def getUnassignedNodeMRV(self):
        nodes = self.getUnassignedNodes()
        nodes.sort(key=lambda x: len(self.nodes[x]))
        return next(iter(nodes), None)
    def getUnassignedNodes(self):
        un = [x for x in self.nodes if len(self.nodes[x]) > 1]
        un.sort()
        return un
    def Domain(self, node):
        return self.nodes[node]
    def isNodeSatisfiedBy(self,nodeA,va):
        def satisfy(nodea,nodeb,typ):
            db = self.Domain(nodeb)
            return (len(db) == 1 and db[0] == va) == False
        satisfiedfrom = [satisfy(a,b,typ) for a,b,typ in csp.fromarcs[nodeA]]
        satisfiedto = [satisfy(a,b,typ) for a,b,typ in csp.toarcs[nodeA]]
        return all(satisfiedfrom) and all(satisfiedto)
    def Assign(self,node,value):
        def deleteFromDomain(node,value):
            if(value in self.nodes[node]):
                self.nodes[node].remove(value)
                if(len(self.nodes[node]) == 1):
                    self.Assign(node,self.nodes[node][0])
                return True
            return False
        def propagate(node,value):
            arcotherside = [b for a,b,typ in csp.fromarcs[node]]
            for x in arcotherside:
#                print("Propagating %s not available to %s" % (value, x))
                deleteFromDomain(x,value)
            arcotherside = [a for a,b,typ in csp.toarcs[node]]
            for x in arcotherside:
#                print("Propagating %s not available to %s" % (value, x))
                deleteFromDomain(x,value)
        self.nodes[node] = [value]
        propagate(node,value)
    def isConsistent(self):
#        def isok(a,b,t):
#            da = self.Domain(a)
#            db = self.Domain(b)
#            if(len(da) == 1 and len(db) == 1 and da[0] == db[0]):
#                return False
#            return True
#        return all([isok(a,b,t) for a,b,t in csp.arcs])
        return all([len(self.nodes[x]) > 0 for x in self.nodes])
    def solution(self):
        return "".join([str(self.nodes["%s%d" % (chr(row),column)][0]) for row in range(65,74) for column in range(1,10)])                

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
#                print("          Cannot satisfy (%s,%s) with %s - deleting %s from %s" % (Xi,Xj,x,x,Xi))
                csp.deleteFromDomain(Xi, x)
                revised = True
        return revised
    q = queue.Queue()
    for xi,xj,t in csp.getArcs():
        q.put((xi,xj,t))
    while (q.empty() == False):
        Xi, Xj, t = q.get()
#        print("Arc %s %s %s" % (Xi,Xj,t))
#        print("     %s Domain %s" % (Xi,csp.Domain(Xi)))
#        print("     %s Domain %s" % (Xj,csp.Domain(Xj)))
        if(Revise(csp,Xi,Xj)):
            if(csp.isDomainEmpty(Xi)):
#                print("     Arc modified is empty. Returning False.")
                return False
#            print("     Arc modified. Expanding its children.")
            for a,b,t in csp.getNodeArcsWithout(Xi,Xj):
#                print("          Expanding (%s,%s,%s)" % (a,b,t))
                q.put((a,b,t))
#        else:
#            print("     Arc not modified.")
#        print("     New %s Domain %s" % (Xi,csp.Domain(Xi)))
#        print("     New %s Domain %s" % (Xj,csp.Domain(Xj)))
    return True

def Backtrack(csp):        
    def search(assignment):
        if(assignment.isSolved()):
#            print("Problem solved!")
            return assignment
#        print("Problem not solved yet!")
        unassignedNode = assignment.getUnassignedNodeMRV()
        if(unassignedNode == None):
            return None
        domain = list(assignment.Domain(unassignedNode))
        domain.sort()
#        print("Chosen node: %s %s" % (unassignedNode, domain))
        for value in domain:
#            print("     testing: %s" % (value))
            #fc
#            newassignment.Assign(unassignedNode, value)
#            if(newassignment.isConsistent()):
#                result = search(newassignment)
#                if(result != None):
#                    return result
            #nfc
            if assignment.isNodeSatisfiedBy(unassignedNode, value):
#                print("     ok: %s" % (value))
                newassignment = assignment.createChild()
                newassignment.Assign(unassignedNode, value)
                if(newassignment.isConsistent()):
                    result = search(newassignment)
                    if(result != None):
                        return result
#            else:
#                print("     nok: %s" % (value))
        return None
    return search(Assignment(csp))

board = SudokuBoard()
#print(sys.argv[1])
board.parse(sys.argv[1])
#            A23456789B23
#oard.parse("000000000302540000050301070000000004409006005023054790000000050700810000080060009")
#board.parse("000000000372540000056321470000000004419276385823154790000000057735810000000060009")

import itertools
def StartCsp():
    csp = CSP()
    for row in range(65,74):
        for column in range(1,10):
            key = "%s%d" % (chr(row),column)
            csp.addNode(key,range(1,10))
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
    for row in range(65,74):
        for column in range(1,10):
            key = "%s%d" % (chr(row),column)
            value = board.getValue(key)
            if(value != 0):
                csp.Assign(key, [value], True)
    return csp

#import datetime
with open("output.txt", "a") as myfile:
    csp = StartCsp()
    result = AC3(csp)
    if(result == False or csp.isSolved() == False):
#        print(datetime.datetime.now().time())
        csp = StartCsp()
        result = Backtrack(csp)
#        print(datetime.datetime.now().time())
        if(result == None or result.isSolved() == False):
            myfile.write("FALSE\n")
#            print("FALSE")
        else:
            myfile.write("%s BTS\n" % (result.solution()))
#            print("148697523372548961956321478567983214419276385823154796691432857735819642284765139")
#            print(result.solution())
#            print("148697523372548961956321478567983214419276385823154796691432857735819642284765139" == result.solution())
    else:
        myfile.write("%s AC3\n" % (csp.solution()))