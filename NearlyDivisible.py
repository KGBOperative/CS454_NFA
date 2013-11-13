import Automata

import sys

__name__ = "__main__"

def NearlyDivisible():
    runAutomata(5)

def runAutomata(number):
    print ("Building Automata for %i" % number)

    N = Automata.NFA()
    N.initial = "R0"
    print ("Set initial state for NFA to %s" % N.initial)
    for i in range(0, number):
        i % number


def buildAutomata():
    N = NFA()

if __name__ == "__main__":
    NearlyDivisible()
    
    states = Set()
    for i in range(0, 2 * n):
        states.add(i)

    '''x_i -> x_j'''
    for i in range(0, n):
        transition = i
        nextState = (currentState * 10 + i) % n
        nextState = currentState + n

