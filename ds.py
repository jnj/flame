# ds.py: data structures
# Josh Joyce, 2004

class stack:
    'A stack class.'
    def __init__(self):
        self.items = []
        self.top = -1

    def size(self):
        'Returns the current size of the stack.'
        return self.top + 1

    def top(self):
        '''
        Returns the element that is at the top of the stack without
        removing it. Raises an EmptyStackException if it is empty.
        '''
        if self.top < 0:                # empty
            raise RuntimeError, 'empty stack'
        else:
            return self.items[self.top]

    def pop(self):
        '''
        Removes and returns the element from the top of the stack.
        '''
        if self.top < 0:                # empty
            raise RuntimeError, 'empty stack'
        else:
            x = self.items[self.top]
            self.items = self.items[:-1]
            self.top = self.top - 1
            return x

    def push(self, x):
        'Pushes an item onto the top of the stack.'
        self.items.append(x)
        self.top = self.top + 1

    
