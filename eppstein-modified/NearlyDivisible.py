from Automata import *

import sys

    
try:
    from set import frozenset as ImmutableSet, set as Set
except ImportError:
    from sets import ImmutableSet, Set

class InputError(Exception): pass

class DFA:
    def __init__(self,Sigma,delta,S0,F):
        """Create deterministic finite automaton with alphabet Sigma,
            transition function delta(state,letter)->state, initial state
            S0, and predicate F(state)->boolean. The full sets of states and
            final states are determined implicitly from the delta and F
            functions.
            """
        self.alphabet = ImmutableSet(Sigma)
        self.transition = delta
        self.initialState = S0
        self.isfinal = F
    
    def states(self):
        """Generate all states of the DFA."""
        explored = Set()
        unexplored = Set([self.initialState])
        while unexplored:
            s = unexplored.pop()
            explored.add(s)
            yield s
            for c in self.alphabet:
                t = self.transition(s,c)
                if t not in explored:
                    unexplored.add(t)
    
    def __call__(self,input):
        """Test whether input is accepted by the DFA."""
        state = self.initialState
        for c in input:
            if c not in self.alphabet:
                raise InputError("Symbol " + repr(c) +
                                 " not in input alphabet")
            state = self.transition(state,c)
        return self.isfinal(state)

class NFA:
    def __init__(self,Sigma,delta,S0,F):
        """Create nondeterministic finite automaton (without
            epsilon-transitions). Arguments are the same as for a
            deterministic finite automaton, except that the initial state
            and result of the transition function are both sets.
            """
        self.alphabet = ImmutableSet(Sigma)
        self.transition = delta
        self.initialStates = ImmutableSet(S0)
        self.isfinal = F
    
    def setTransition(self,states,c):
        """States reachable from input set by input c."""
        result = Set()
        for s in states:
            result |= self.transition(s,c)
        return ImmutableSet(result)
    
    def finalSet(self,states):
        """Test whether any of given set of states is final."""
        for s in states:
            if self.isfinal(s):
                return True
        return False
    
    def makeDeterministic(self):
        """Convert NFA to DFA."""
        return DFA(self.alphabet,self.setTransition,
                   self.initialStates,self.finalSet)
    
    def __call__(self,input):
        """Test whether input is accepted by the NFA."""
        return self.makeDeterministic()(input)

# Example from Sipser, _Introduction to the Theory of Computation_
# (Preliminary Edition), Example 1.13, pages 47-48
if __name__ == "__main__":
    
    
    """ Build the transitions here and initialize a NFA with delta """
    def buildTransitions(number):
        x = {}
        # state:(states for transition on 0, states for transition on 1)
        for i in range(0, number):
            for j in range(0, number):
                # build the list of states transitioned to from i on input j
                
                
                # adds a list to the dict at [i][j] where i is the key and j is the set of inputs from 0 to number
                trans[i][j] = []
            
            transitions.update (x)

    # return an immutable set for the delta function
    def delta(state, langSymbol):
        return ImmutableSet(Sipser_1_13[s][int(langSymbol)])

    def final(s):
        return s == 4

    N2 = NFA("01",delta,[1],final)
    
    def test(input):
        print input,(N2(input) and "is" or "is not"),"in L(N2)"

    test("000100")
    test("0011")
    
    print
    print "Conversion to DFA:"
    print
    D2 = N2.makeDeterministic()
    for s in D2.states():
        print s
        for c in "01":
            print "  --[" + c + "]-->", D2.transition(s,c)