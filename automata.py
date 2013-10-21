class NFA
    def __init__(self, k):
        self.modNum = k
        self.states = range(0, 2*modNum)
        self.alphabet = range(0, 10)
        self.init = 0
        self.final = Set()
        self.final.add(0)
        self.final.add(modNum)
    
    def transition(self, state, inputChar):
        nextStates = Set()
        nextStates.add((state * 10 + inputChar) % self.modNum)
        if (state < self.modNum):
            nextStates.add(state + self.modNum)
        return nextStates



