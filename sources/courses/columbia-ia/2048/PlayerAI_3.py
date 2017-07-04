import math
import numpy
from BaseAI_3 import BaseAI

class PlayerAI(BaseAI):
#    weights = numpy.loadtxt('C:/github/xunilrj-sandbox/sources/courses/columbia-ia/2048/weigths.txt', delimiter=';')
    weights = [1,2.7,0,0.1,1]
    #1max -----^ ^   ^  ^   ^
    #2qtd empty _/   |  |   |
    #3sum tiles _____/  |   |
    #4smoothness _______/   |
    #5monotonicity _________/
    def aftermove(self,grid,move):
        clone = grid.clone()
        clone.move(move)
        return clone
    def afterFillCell(self,grid,cell,value):
        clone = grid.clone()
        clone.setCellValue(cell,value)
        return clone
    def possibleNewItems(self, grid):
        return [self.afterFillCell(grid,cell,value) for cell in grid.getAvailableCells() for value in [2,4]]
    def score01(self,grid):
        return grid.getMaxTile()
    def score02(self,grid):
        return math.log2(len(grid.getAvailableCells()))
    def score03(self,grid):
        return sum([grid.getCellValue((x,y)) for x in range(4) for y in range(4)])
    def cellOccupied(self, grid, cell):
        return grid.getCellValue(cell) > 0
    def findFarthestPosition(self, grid, cell, vector):
        previous = None
        while True:
            previous = cell
            cell = (previous[0] + vector[0], previous[1] + vector[1]) 
            if(grid.crossBound(cell) or self.cellOccupied(grid, cell)):
                break
        return (previous, cell)    
    #smoothness
    def score04(self,grid):
        smoothness = 0.0
        for x in range(4):
            for y in range(4):
                if(self.cellOccupied(grid, (x,y))):
                    cellValue = grid.getCellValue((x,y))
                    value = math.log2(cellValue)/math.log2(2)
                    for direction in range(1,3):
                        if(direction == 1):
                            vector = (1,0)
                        if(direction == 2):
                            vector = (0,1)
                        targetCell = self.findFarthestPosition(grid, (x,y), vector)[1]
                        if (grid.crossBound(targetCell) == False and self.cellOccupied(grid, targetCell)):
                            target = grid.getCellValue(targetCell)
                            targetValue = math.log2(target) / math.log2(2)
                            smoothness -= abs(value - targetValue)
        return smoothness
    #monotonicity
#    // measures how monotonic the grid is. This means the values of the tiles are strictly increasing
#// or decreasing in both the left/right and up/down directions
    #Grid.prototype.monotonicity2 = function() {
    def score05(self, grid):
        #  // scores for all four directions
        #  var totals = [0, 0, 0, 0];
        totals = [0,0,0,0]
        #  // up/down direction
        #  for (var x=0; x<4; x++) {
        for x in range(4):
            #    var current = 0;
            current = 0
            #    var next = current+1;
            nextt = current + 1
            #    while ( next<4 ) {
            while(nextt < 4):
                #while ( next<4 && !this.cellOccupied( this.indexes[x][next] )) {
                while(nextt < 4 and self.cellOccupied(grid, (x,nextt)) == False):
                    #next++;
                    nextt = nextt + 1
                #}
                #if (next>=4) { next--; }
                if(nextt >= 4):
                    nextt = nextt - 1
                #var currentValue = this.cellOccupied({x:x, y:current}) ?
                #   Math.log(this.cellContent( this.indexes[x][current] ).value) / Math.log(2) :
                #   0;
                currentValue = 0
                if (self.cellOccupied(grid, (x,current))):  
                    currentValue = math.log2(grid.getCellValue((x,current)))/math.log2(2)
                #var nextValue = this.cellOccupied({x:x, y:next}) ?
                #   Math.log(this.cellContent( this.indexes[x][next] ).value) / Math.log(2) :                        
                #   0;
                nextValue = 0
                if self.cellOccupied(grid, (x,nextt)):
                    nextValue = math.log2(grid.getCellValue((x,nextt)))/math.log2(2)
                #if (currentValue > nextValue) {
                if(currentValue > nextValue):
                #   totals[0] += nextValue - currentValue;
                    totals[0] += nextValue - currentValue
                #} else if (nextValue > currentValue) {
                elif (nextValue > currentValue):
                #   totals[1] += currentValue - nextValue;
                    totals[1] += currentValue - nextValue
                #}
                #current = next;
                current = nextt
                #next++;
                nextt = nextt + 1
            #}
        #}
        #  // left/right direction
        #  for (var y=0; y<4; y++) {
        for y in range(4):
            #var current = 0;
            current = 0
            #var next = current+1;
            nextt = current + 1
            #while ( next<4 ) {
            while(nextt < 4):
                #while ( next<4 && !this.cellOccupied( this.indexes[next][y] )) {
                while(nextt < 4 and self.cellOccupied(grid, (nextt,y)) == False):
                        #next++;
                        nextt = nextt + 1
                #}
                #if (next>=4) { next--; }
                if(nextt >= 4):
                    nextt = nextt - 1
                #var currentValue = this.cellOccupied({x:current, y:y}) ?
                #   Math.log(this.cellContent( this.indexes[current][y] ).value) / Math.log(2) :
                #   0;
                currentValue = 0
                if(self.cellOccupied(grid, (current,y))):
                    currentValue = math.log2(grid.getCellValue((current,y))) / math.log2(2)
                else:
                    currentValue = 0
                #var nextValue = this.cellOccupied({x:next, y:y}) ?
                #   Math.log(this.cellContent( this.indexes[next][y] ).value) / Math.log(2) :
                #   0;
                nextValue = 0
                if(self.cellOccupied(grid, (nextt,y))):
                    nextValue = math.log2(grid.getCellValue((nextt,y) )) / math.log2(2)
                else:
                    nextValue = 0
                #if (currentValue > nextValue) {
                if(currentValue > nextValue):
                #   totals[2] += nextValue - currentValue;
                    totals[2] += nextValue - currentValue
                #} else if (nextValue > currentValue) {
                elif (nextValue > currentValue):
                #   totals[3] += currentValue - nextValue;
                    totals[3] += currentValue - nextValue
                #}
                #current = next;
                current = nextt
                #  next++;
                nextt = nextt + 1
            #}
        #}
        #return Math.max(totals[0], totals[1]) + Math.max(totals[2], totals[3]);
        return max(totals[0],totals[1]) + max(totals[2],totals[3])
    #}
    def score(self,grid):
        return (self.weights[0]*self.score01(grid)+
            self.weights[1]*self.score02(grid)+
            self.weights[2]*self.score03(grid)+
            self.weights[3]*self.score04(grid)+
            self.weights[4]*self.score05(grid))
    def minmaxmin(self, grid, alpha, beta, depth):
        if(depth == 3):
            return (None,self.score(grid))
        minchild = None
        minutility = float("inf")#math.inf
        #moves = grid.getAvailableMoves([0, 2, 3, 1])
        moves = self.possibleNewItems(grid)
        for currentchild in moves:
#            currentchild = self.aftermove(grid,move)
            result = self.minmaxmax(currentchild, alpha, beta, depth + 1)
            currentutility = result[1]
            if(currentutility < minutility):
                minchild = None
                minutility = currentutility
            if(minutility <= alpha):
                break;
            if(minutility < beta):
                beta = minutility
        return (minchild,minutility)
    def minmaxmax(self, grid, alpha, beta, depth):
        if(depth == 3):
            return (None,self.score(grid))
        maxchild = None
        maxutility = -float("inf")#math.inf
        moves = grid.getAvailableMoves([0, 2, 3, 1])
        for move in moves:
            currentchild = self.aftermove(grid,move)
            result = self.minmaxmin(currentchild, alpha, beta, depth + 1)
            currentutility = result[1]
            if(currentutility > maxutility):
                maxchild = move
                maxutility = currentutility
            if(maxutility >= beta):
                break
            if(maxutility > alpha):
                alpha = maxutility
        return (maxchild,maxutility)
    def minmax(self, grid):
        result = self.minmaxmax(grid, -float("inf"), float("inf"), 0)
        return result[0]
    def getMove(self, grid):
        #print(self.weights)
        return self.minmax(grid)
    