# flamegen.py
# Flame compiler code generation module
# Josh Joyce
# jnjoyce@uchicago.edu
# $Id: flamegen.py,v 1.1.1.1 2001/06/05 06:11:41 josh Exp $



# Modules used in flamegen
import string
import cStringIO
import symtab
import math



# Buffers for the various data segments
stringdata = cStringIO.StringIO()
worddata = cStringIO.StringIO()



# A function to print a nicely formatted instruction to out.
# This is how all of the instructions are written to the .s file.
def prinstr(out, instr, comment=''):
    # Expand the tabs before printing
    instr = instr.expandtabs()
    
    # A field width of 40 chars should be plenty
    print >>out, '%-40s%s' % (instr, comment)



# A function to print to a string and return it.
def mkstr(fmt, args):
    s = cStringIO.StringIO()
    print >>s, fmt % args
    return s.getvalue()[:-1]



# A function to conveniently do a load with offset size checking
def do_load(out, r, fp_offset, offset):
    if -4096 <= offset and offset <= 4095:
        op = ' + '
        if offset < 0:
            op = ' - '
        offset = abs(offset)
        offset = str(offset)
        prinstr(out, mkstr('\tld [%s%s%s], %s',(fp_offset, op, offset, r)))
    else:
        offset = str(offset)
        prinstr(out, mkstr('\tsethi %%hi(%s), %%g1', (offset)))
        prinstr(out, mkstr('\tor %%g1, %%lo(%s), %%g1', (offset)))
        prinstr(out, mkstr('\tld [%s + %%g1], %s', (fp_offset, r)))



# A function to conveniently do a store with offset size checking
def do_store(out, result, fp_offset, offset, flag=0):
    if -4096 <= offset and offset <= 4095:
        op = ' + '
        if offset < 0:
            op = ' - '
        offset = abs(offset)
        offset = str(offset)
        if flag:
            prinstr(out, mkstr('\tst %s, [%s]',(result,fp_offset)))
            return
        r = result
        prinstr(out, mkstr('\tst %s, [%s%s%s]',(r, fp_offset, op, offset)))
    else:
        offset = str(offset)
        prinstr(out, mkstr('\tsethi %%hi(%s), %%g1', (offset)))
        prinstr(out, mkstr('\tor %%g1, %%lo(%s), %%g1', (offset)))
        prinstr(out, mkstr('\tst %s, [%s + %%g1]', (result, fp_offset)))
             


# A class to generate unique labels
class LabelManager:
    def __init__(self):
        self.labelno = 0

    def new_label(self):
        # Generate a new, unique label and return it
        l = '.L' + str(self.labelno)
        self.labelno += 1
        return l



# A general purpose Stack class
class Stack:
    def __init__(self):
        self.s_ = []
        self.st_ = -1

    def isempty(self):
        return self.s_ == []

    def push(self, item):
        self.s_.append(item)
        self.st_ += 1

    def top(self):
        return self.s_[self.st_]

    def pop(self):
        self.st_ -= 1
        return self.s_.pop()



# The Register class is used for register allocation
class Register:
    def __init__(self, offset, extras=()):
        self.regs = ('%l0', '%l1', '%l2', '%l3', '%l4', '%l5', '%l6', '%l7')
        if extras:
            self.regs = list(self.regs)
            self.regs += list(extras)
            self.regs = tuple(self.regs)
        self.nregs = len(self.regs)
        self.used = [0] * self.nregs
        self.sp = -1 
        self.count = 0
        self.ntemps = 0
        self.stack = list(self.regs)
        self.offset = offset
        
    def push(self, out):
        self.sp = (self.sp + 1) % self.nregs
        if self.count >= self.nregs:
            # Spill the oldest register to memory
            self.ntemps += 1
            oldest = self.regs[self.sp]
            location = self.offset - self.ntemps * 4
            prinstr(out, '', '! spill ' + str(oldest))
            do_store(out, oldest, '%fp', location)
            if self.stack[self.sp] == self.regs[self.sp]:
                self.stack[self.sp] = [location]
            else:
                self.stack[self.sp].append(location)
        else:
            self.count += 1

        # Set a flag that this register has a value in it
        # I.e., it can be returned on a pop()
        self.used[self.sp] = 1
        return self.regs[self.sp]
            
    def pop(self, out):
        if self.count > 0:
            self.count -= 1
        reg = self.regs[self.sp]

        # If this one is in use, it can be returned
        # on a pop().
        if self.used[self.sp]:
            self.used[self.sp] = 0

        # Otherwise, we have to restore the spilled
        # value from memory.
        else:
            location = self.stack[self.sp].pop()
            prinstr(out, '', '! load spilled value into ' + reg)
            do_load(out, reg, '%fp', location)
        self.sp = (self.sp - 1) % self.nregs
        return reg



# This is for break. It always holds the value of
# the current break label at the top.
breaklabels = Stack()


# Keep track of the current symbol table
symtables = Stack()


# Keep track of the current Register object
registers = Stack()


# Use a LabelManager to generate labels
lmanager = LabelManager()



# A function that pushes the value of DISPLAY[i] onto the stack
def save_fp(out, i):
    global registers
    current_registers = registers.top()

    # Load the address of DISPLAY into a register
    #r = current_registers.push(out)
    #r = '%i5'
    #prinstr(out, mkstr('\tset DISPLAY, %s', (r)))

    # Load DISPLAY[i] into a register
    #r = current_registers.pop(out)
    #r = '%i5'
    r = '%o5'
    s = current_registers.push(out)
    comment = '! push DISPLAY[' + str(i) + ']'
    prinstr(out, mkstr('\tld [%s + %d], %s', (r, i * 4, s)), comment)



# A function to write the current value of %fp to DISPLAY[i]
def set_fp(out, i):
    global registers
    current_registers = registers.top()

    # Load the address of DISPLAY into a register
    #r = current_registers.push(out)
    #prinstr(out, mkstr('\tset DISPLAY, %s', (r)))

    # Set the value of DISPLAY[i] to %fp
    #r = current_registers.pop(out)
    #r = '%i5'
    r = '%o5'
    comment = '! DISPLAY[' + str(i) + '] := %fp'
    prinstr(out, mkstr('\tst %%fp, [%s + %d]', (r, i * 4)), comment)



# A function to restore the old value of DISPLAY[i]
def restore_fp(out, i):
    global registers
    current_registers = registers.top()

    # Load the address of DISPLAY into a register
    #r = current_registers.push(out)
    #r = '%i5'
    #prinstr(out, mkstr('\tset DISPLAY, %s', (r)))

    # Set the value of DISPLAY[i] to the stack top
    #r = current_registers.pop(out)
    #r = '%i5'
    r = '%o5'
    s = current_registers.pop(out)
    comment = '! DISPLAY[' + str(i) + '] := pop'
    prinstr(out, mkstr('\tst %s, [%s + %d]', (s, r, i * 4)), comment)



# A function to align the stack frame size
def align_on_eight(n):
    # Return the next mulitple of eight that is
    # greater than  or equal to the argument n.
    return n + (8 - (n % 8))


# A function to get the frame offset of a symbol
def offsetof(name):
    global symtables

    # Look for the symbol in the current table
    found = symtab.lookup_in(name, symtables.top())

    if found:
        return found.frame_offset

    # Not found
    return None


# Find the symbol entry for a variable. This is needed because the lexer
# creates  empty  symbol  entries  in each table for symbols it does not
# recognize. This function goes  and  fetches  the symbol entry for when
# the  symbol  was  declared. This is used to get the nesting level of a
# symbol, no matter what function(s) it appears in.
def do_global_lookup(name):
    global symtables
    
    t = symtables.st_
    s = symtab.lookup_in(name, symtables.s_[t])
    while t >= 0:
        t -= 1
        s = symtab.lookup_in(name, symtables.s_[t])
        if s and hasattr(s, 'nestlev'):
            break
    return s


# A function to look up the mangled name of a function.
def get_mangled_name(name):
    # Look up the mangled name of a function by searching
    # outward in the enclosing functions' symbol tables
    global symtables

    # If no top symbol table, return the name
    if not symtables.top():
        return name

    # Start at the topmost symbol table
    t = symtables.st_

    # If there is no top, return the name
    if t < 0 or not symtables.s_[t]:
        return name

    # Otherwise, start searching in the symbol tables
    if t > 0:
        # Look at the top one
        nd = symtab.lookup_in('$namedict', symtables.top())
        if nd == None:
            return name
        mn = nd.get(name, None)

        # Search deeper until we find one or until we run out
        while mn == None:
            t -= 1
            if t < 0 or not symtables.s_[t]:
                return name
            nd = symtab.lookup_in('$namedict', symtables.s_[t])
            if nd == None:
                continue
            mn = nd.get(name, None)
        return mn
    else:
        # Catch-all
        return name


# A function to compute the space needed for locals or args
def allocate_space(locals_list, argflag, offst):
    global symtables

    # Initialization
    current_offset = offst
    total_bytes = 0
    nargs = 0

    # Iterate over each variable
    for local in locals_list:

        # This function handles both local declarations
        # and arguments,  so  we  have know which to do
        # (Their parse subtrees are slightly different)
        if argflag:
            a = local.children[0]
        else:
            a = local

        # Skip over local functions
        if a.type != 'local_variable' and a.type != 'argument':
            continue

        # Set up some convenient variables
        name = a.value
        type = a.datatype
        foundsym = symtab.lookup_in(name, symtables.top())

        # If the symbol exists, compute its size and offset
        if foundsym:

            # Initialize 'arg' attribute to None
            foundsym.arg = None

            # It might be an array
            dimension = type[1]

            if dimension:
                foundsym.dimension = dimension

            if a.type == 'argument':
                foundsym.arg = 1

            if dimension and a.type != 'argument':
                size = 4 * dimension
            elif dimension and a.type == 'argument':
                size = 0
            else:
                size = 4

            if a.type == 'argument':
                foundsym.frame_offset = 64 + (nargs * 4)
                nargs += 1
            else:
                if dimension:
                    foundsym.frame_offset = current_offset - size
                else:
                    # print current_offset
                    foundsym.frame_offset = current_offset - size

            current_offset -= size
            total_bytes += size
            # Debugging output
            # print '%s has offset %i' % (name, foundsym.frame_offset)

    return total_bytes



def generate(file, top):

    # This aligns the word data properly
    print >>worddata, '\n\t.section\t".data"'
    print >>worddata, '\t.align 4'

    # Add the DISPLAY array to the word data
    print >>worddata, '\nDISPLAY:\t.word\t%s' % \
          (''.join(['0, '] * (top.value)) + '0')

    print >>file, '! Created by flame.py'
    print >>file, '! Josh Joyce, CS326 (Spring 2001)'
    print >>file, '\n\t.section\t\".text\"'
    print >>stringdata, '\n\t.section\t\".rodata\"'

    emit_program(file, top)

    print >>file, worddata.getvalue()
    print >>file, stringdata.getvalue()



def emit_program(out, top):
    global symtables
    symtables.push(top.symtab)
    functions = top.children[0].children
    for f in functions:
        emit_function(out, f)



def emit_function(out, func, nest=0):
    global symtables
    global registers
    global lmanager

    # Add a name dictionary to this function's symbol table
    func.symtab['$namedict'] = {}

    # Helper function for finding local functions
    # in the parse tree
    def getfunction(n):
        if n.children[0].type == 'local_function':
            return n.children[0].children[0]
        return None

    # Get nested function list
    nestedfns = []
    locals = func.children[1]
    if locals.children:
        nestedfns = map(getfunction, func.children[1].children[0].children)
        nestedfns = filter(lambda n: n, nestedfns)

    # Add any local functions with the same name to the
    # name dictionary for this function
    for i in nestedfns:
        name = i.children[0].value
        l = lmanager.new_label()
        func.symtab['$namedict'][name] = l[1:] + name

    # Compute some information about this function
    fname = func.children[0].value
    mname = get_mangled_name(fname)
    symtables.push(func.symtab)
    locals_size = 0
    args_size = 0
    call_args = symtab.lookup_in('$maxargs', symtables.top())
    nest_levl = symtab.lookup_in('$nestlev', symtables.top())
    assert(call_args != None)
    cargs_size = 4 * call_args.size

    # If global function, print '.global'
    if not nest:
        prinstr(out, '\n\t.global ' + fname + '\n')
        
    # Create a buffer for the function body
    function_body = cStringIO.StringIO()
    
    # Determine space requirement for arguments
    if func.children[0].children[0].children:
        al = func.children[0].children[0].children[0].children
        args_size = allocate_space(al, 0, 0)

    # Determine space requirements for locals
    if func.children[1].children:
        ll = func.children[1].children[0].children
        locals_size = allocate_space(ll, 1, 0)

    # As long as we're passing function args on the stack,
    # these can be used as extra registers for expressions.
    iregs = map(lambda x,y: x+str(y), ['%i'] * 5, range(1, 6))
    oregs = map(lambda x,y: x+str(y), ['%o'] * 4, range(1, 5))
    gregs = ['%g2', '%g3']
    xregs = tuple(iregs + oregs + gregs)

    # Create a new Register object for this function
    regobj = Register(-locals_size, xregs)
    registers.push(regobj) 

    # Save the old value of DISPLAY[i]
    save_fp(function_body, nest_levl.size)

    # Write %fp to DISPLAY[i]
    set_fp(function_body, nest_levl.size)

    # Emit the body of the function
    statements = func.children[2]
    emit_statements(function_body, statements)

    # Compute temporary storage space needed
    spilled_size = regobj.ntemps * 4

    # Determine the total frame size needed
    frame_size = align_on_eight(64 + locals_size + spilled_size + cargs_size)

    # Output function entry code
    comment = '! function: ' + mname + ' (start)'
    prinstr(out, mname + ':', comment)

    if abs(frame_size) <= 4095:
        prinstr(out, mkstr('\tsave %%sp, %i, %%sp', (-frame_size)))
    else:
        prinstr(out, mkstr('\tsethi %%hi(%i), %%g1', (-frame_size)))
        prinstr(out, mkstr('\tor    %%g1, %%lo(%i), %%g1', (-frame_size)))
        prinstr(out, '\tsave  %sp, %g1, %sp')

    # Immediately store DISPLAY into %o5
    if fname == 'main':
        prinstr(out, '\tset DISPLAY, %o5', '! store DISPLAY in %o5')
    else:
        prinstr(out, '\tmov %i5, %o5', '! store DISPLAY in %o5')

    # Output the function body
    print >>out, function_body.getvalue()
    del function_body

    # Output 'done' label
    return_label = lmanager.new_label()
    prinstr(out, return_label + ':')

    # Restore the old value of DISPLAY[i]
    restore_fp(out, nest_levl.size)

    # Main function needs special cleanup instructions
    if fname == 'main':
        prinstr(out, '\tmov 0, %o0')
        prinstr(out, '\tcall _exit')
        prinstr(out, '\tnop')

    # Return code
    prinstr(out, '\tret')
    comment = '! function: ' + fname + ' (end)'
    prinstr(out, '\trestore', comment)

    # Restore the previous Registers object
    registers.pop()

    # Emit nested functions
    for i in nestedfns:
        emit_function(out, i, 1)         

    # Remove the symbol table for this function
    symtables.pop()



def emit_statements(out, statements):
    for s in statements.children:
        emit_statement(out, s)



def emit_statement(out, s):
    if s.type == 'while':
        emit_while(out, s)
    elif s.type == 'if':
        emit_if(out, s)
    elif s.type == 'assignment':
        emit_assignment(out, s)
    elif s.type == 'print':
        emit_print(out, s)
    elif s.type == 'write':
        emit_write(out, s)
    elif s.type == 'read':
        emit_read(out, s)
    elif s.type == 'return':
        emit_return(out, s)
    elif s.type == 'skip':
        emit_skip(out, s)
    elif s.type == 'break':
        emit_break(out, s)
    elif s.type == 'statement_block':
        emit_statement_block(out, s)
    elif s.type == 'function_call':
        emit_function_call(out, s)
    else:
        # We should never get here
        print 'There is a parse tree Node I do not recognize. Quitting.'
        import sys
        sys.exit()



def emit_statements_easy(out, s):
    if s.type == 'statement_block':
        emit_statement_block(out, s)
    else:
        emit_statement(out, s)



def emit_while(out, w):
    global registers
    global breaklabels
    global lmanager
    
    current_registers = registers.top()
    test_label = lmanager.new_label()
    done_label = lmanager.new_label()    

    breaklabels.push(done_label)
    comment = '! while (start)'
    prinstr(out, test_label + ':', comment)
    rel = w.children[0]
    eval_relation(out, rel)
    prinstr(out, '', '! relop := pop')
    relop = current_registers.pop(out)
    comment = '! if not relop: goto ' + done_label
    prinstr(out, '\tcmp ' + relop + ', %g0', comment)
    prinstr(out, mkstr('\tbe  %s', (done_label)))
    prinstr(out, '\tnop')
 
    statements = w.children[1]
    emit_statements_easy(out, statements)

    prinstr(out, '\tba ' + test_label, mkstr('! goto %s', (test_label)))
    prinstr(out, '\tnop')
    prinstr(out, mkstr('%s:', (done_label)), '! while (end)')
    breaklabels.pop()


def emit_if(out, w):
    global registers
    global breaklabels
    global lmanager
    current_registers = registers.top()

    prinstr(out, '', '! if (start)')
    else_label = lmanager.new_label()
    next_label = lmanager.new_label()
    rel = w.children[0]
    eval_relation(out, rel)
    prinstr(out, '', '! relop := pop')

    result = current_registers.pop(out)
    comment = '! if not relop: goto ' + else_label
    prinstr(out, mkstr('\tcmp %s, %%g0', result), comment)
    prinstr(out, mkstr('\tbe  %s', (else_label)))
    prinstr(out, '\tnop')

    emit_statements_easy(out, w.children[1])
    comment = '! goto ' + next_label
    prinstr(out, mkstr('\tba %s', next_label), comment)
    prinstr(out, '\tnop')
    prinstr(out, mkstr('%s:', (else_label)))

    if len(w.children) > 2:
        emit_statements_easy(out, w.children[2])
    prinstr(out, mkstr('%s:', (next_label)), '! if (end)')



def emit_assignment(out, w):
    global registers
    global symtables

    current_registers = registers.top()
    array = 0
    prinstr(out, '', '! assignment (start)')
    name = w.children[0].value
  
    # Get the symbol table entry for the variable
    symbol = symtab.lookup_in(name, symtables.top())
    assert(symbol != None)
    
    # Get the current nesting level
    current_nest_level = symtab.lookup_in('$nestlev', symtables.top())
    assert(current_nest_level != None)

    # Get the nesting level for the location
    if not hasattr(symbol, 'nestlev'):
        symbol = do_global_lookup(name)
            
    assert(hasattr(symbol, 'nestlev'))
    symbol_nest_level = symbol.nestlev
    offset = symbol.frame_offset

    # Evaluate the expression we're assigning
    expr = w.children[1]
    eval_expression(out, expr)

    if symbol_nest_level != current_nest_level.size:
        # We need to load DISPLAY[symbol_nest_level] as
        # our %fp and reference the symbol from that
        
        # Load DISPLAY
        #r = current_registers.push(out)
        #prinstr(out, mkstr('\tset DISPLAY, %s', (r)))
        
        # Load DISPLAY[i] into a register
        #r = current_registers.pop(out)
        #r = '%i5'
        r = '%o5'
        s = current_registers.push(out)
        i = symbol_nest_level * 4
        prinstr(out, mkstr('\tld [%s + %d], %s', (r, i, s)))
                
    # If we're assigning to an array element
    if w.children[0].children:
        array = 1
        
        # Compute index expr of a[expr]
        eval_expression(out, w.children[0].children[0])
        index = current_registers.pop(out)

        # Multiply the index by 4 to get no. bytes
        r = current_registers.push(out)
        prinstr(out, mkstr('\tsll %s, 2, %s', (index, r)), '! top *= 4')

        if symbol.arg:
            # lvalue was passed as a function parameter
            # Load the address
            if current_nest_level.size != symbol_nest_level:
                bytes = current_registers.pop(out)
                s = current_registers.pop(out)
                r = current_registers.push(out)
                do_load(out, r, s, offset)
            else:
                r = current_registers.push(out)
                do_load(out, r, '%fp', offset)  

            # Add bytes to its address
            addr = current_registers.pop(out)
            if current_nest_level.size == symbol_nest_level:
                bytes = current_registers.pop(out)
            comment = '! add bytes to base address'
            r = current_registers.push(out)
            prinstr(out, mkstr('\tadd %s, %s, %s', (addr, bytes, r)), comment)

        else:
            if current_nest_level.size != symbol_nest_level:
                bytes = current_registers.pop(out)
                s = current_registers.pop(out)
                r = current_registers.push(out)
                prinstr(out, mkstr('\tadd %s, %s, %s', (s, bytes, r)))
            else:
                bytes = current_registers.pop(out)
                # Add offset to %fp
                r = current_registers.push(out)
                comment = '! add offset to %fp'
                prinstr(out, mkstr('\tadd %%fp, %s, %s', (bytes, r)), comment)

    if array:
        fp_offset = current_registers.pop(out)
        result = current_registers.pop(out)
        if symbol.arg:
            do_store(out, result, fp_offset, offset, 1)
        else:
            do_store(out, result, fp_offset, offset)
    else:
        if current_nest_level.size != symbol_nest_level:
            s = current_registers.pop(out)
            result = current_registers.pop(out)
            do_store(out, result, s, offset)
        else:
            result = current_registers.pop(out)
            do_store(out, result, '%fp', offset)
    prinstr(out, '', '! assignment (end)')


def emit_print(out, w):
    global lmanager
    # Put the string literal into the data segment
    label = lmanager.new_label()
    literal = w.children[0].value

    s = '\n' + label + ':'
    t = '.asciz   \t' + literal

    print >>stringdata, '%-8s %s' % (s, t)
    prinstr(out, mkstr('\tsethi %%hi(%s), %%o0', (label)))
    prinstr(out, mkstr('\tor    %%o0, %%lo(%s), %%o0', (label)))
    prinstr(out, '\tcall  flprint')
    prinstr(out, '\tnop')
    

def emit_write(out, w):
    global registers
    current_registers = registers.top()
    prinstr(out, '', '! write (start)')
    expr = w.children[0]
    eval_expression(out, expr)
    prinstr(out, mkstr('\tmov %s, %%o0', (current_registers.pop(out))))

    if w.value == 'int':
        prinstr(out, '\tcall flwritei')
    elif w.value == 'float':
        prinstr(out, '\tcall flwritef')

    prinstr(out, '\tnop')
    prinstr(out, '', '! write (end)')



def emit_read(out, w):
    global registers
    current_registers = registers.top()
    prinstr(out, '', '! read (start)')
    expr = w.children[0]
    offset = offsetof(expr.value)

    # If we're reading into an array element
    if w.children[0].children:
        
        # Compute the index
        eval_expression(out, w.children[0].children[0])
        index = current_registers.pop(out)
        
        # Multiply the index by 4 to get the # of bytes
        s = mkstr('\tsll %s, 2, %s', (index, current_registers.push(out)))
        prinstr(out, s)
        bytes = current_registers.pop(out)

        # Add bytes to the offset
        t = mkstr('\tadd %%fp, %s, %s', (bytes, current_registers.push(out)))
        prinstr(out, t)

        fp_offset = current_registers.pop(out)

        # Call the appropriate routine
        if w.value == 'int':
            prinstr(out, '\tcall flreadi')
        elif w.value == 'float':
            prinstr(out, '\tcall flreadf')
        prinstr(out, '\tnop')
        do_store(out, '%o0', fp_offset, offset)
    else:
        # Not an array element
        # Call the appropriate routine
        if w.value == 'int':
            prinstr(out, '\tcall flreadi')
        elif w.value == 'float':
            prinstr(out, '\tcall flreadf')
        prinstr(out, '\tnop')
        do_store(out, '%o0', '%fp', offset)
    prinstr(out, '', '! read (end)')


#def emit_read(out, w):
#    global registers
#    global symtables
    
#    current_registers = registers.top()
#    array = 0
#    prinstr(out, '', '! read (start)')
#    expr = w.children[0]
#    name = expr.value

    # Get the symbol table entry for the variable
#    symbol = symtab.lookup_in(name, symtables.top())
#    assert(symbol != None)
    
    # Get the current nesting level
#    current_nest_level = symtab.lookup_in('$nestlev', symtables.top())
#    assert(current_nest_level != None)

    # Get the nesting level for the location
#    if not hasattr(symbol, 'nestlev'):
#        symbol = do_global_lookup(name)
            
#    assert(hasattr(symbol, 'nestlev'))
#    symbol_nest_level = symbol.nestlev
#    offset = symbol.frame_offset

#    if symbol_nest_level != current_nest_level.size:
        # We need to load DISPLAY[symbol_nest_level] as
        # our  %fp  and  reference the symbol from that
        
        # Load DISPLAY
#        r = current_registers.push(out)
#        prinstr(out, mkstr('\tset DISPLAY, %s', (r)))
        
        # Load DISPLAY[i] into a register
#        r = current_registers.pop(out)
        #r = '%i5'
#        s = current_registers.push(out)
#        i = symbol_nest_level * 4
#        prinstr(out, mkstr('\tld [%s + %d], %s', (r, i, s)))
 
    # If we're reading into an array element
#    if w.children[0].children:
#        array = 1

        # Compute the index
#        eval_expression(out, w.children[0].children[0])
#        index = current_registers.pop(out)
        
        # Multiply the index by 4 to get the no. bytes
#        r = current_registers.push(out)
#        prinstr(out, mkstr('\tsll %s, 2, %s', (index, r)))

#        if symbol.arg:
            # Load the address
#            if current_nest_level.size != symbol_nest_level:
#                bytes = current_registers.pop(out)
#                s = current_registers.pop(out)
#                r = current_registers.push(out)
#                do_load(out, r, s, offset)
#            else:
#                r = current_registers.push(out)
#                do_load(out, r, '%fp', offset)  

        # Add bytes to its address
#        addr = current_registers.pop(out)
#        if current_nest_level.size == symbol_nest_level:
#            bytes = current_registers.pop(out)
#        comment = '! add bytes to base address'
#        r = current_registers.push(out)
#        prinstr(out, mkstr('\tadd %s, %s, %s', (addr, bytes, r)), comment)

#    else:
#        # Not reading into array element
#        if current_nest_level.size != symbol_nest_level:
#            bytes = current_registers.pop(out)
#            s = current_registers.pop(out)
#            r = current_registers.push(out)
#            prinstr(out, mkstr('\tadd %s, %s, %s', (s, bytes, r)))
#        else:
#            bytes = current_registers.pop(out)
#            # Add offset to %fp
#            r = current_registers.push(out)
#            comment = '! add offset to %fp'
#            prinstr(out, mkstr('\tadd %%fp, %s, %s', (bytes, r)), comment)
#
#    if array:
#        fp_offset = current_registers.pop(out)
#        # Call the appropriate routine
#        if w.value == 'int':
#            prinstr(out, '\tcall flreadi')
#        elif w.value == 'float':
#            prinstr(out, '\tcall flreadf')
#        prinstr(out, '\tnop')
#        if symbol.arg:
#            do_store(out, '%o0', fp_offset, offset, 1)
#        else:
#            do_store(out, '%o0', fp_offset, offset)
#    else:
        # Not an array element
        # Call the appropriate routine
#        if w.value == 'int':
#            prinstr(out, '\tcall flreadi')
#        elif w.value == 'float':
#            prinstr(out, '\tcall flreadf')
#        prinstr(out, '\tnop')
#        if current_nest_level.size != symbol_nest_level:
#            s = current_registers.pop(out)
#            do_store(out, '%o0', s, offset)
#        else:
#            do_store(out, '%o0', '%fp', offset)
#    prinstr(out, '', '! read (end)')


def emit_return(out, w):
    global registers
    current_registers = registers.top()
    prinstr(out, '', '! return (start)')
    expr = w.children[0]
    eval_expression(out, expr)
    prinstr(out, '', '! expr := pop')
    result = current_registers.pop(out)
    prinstr(out, mkstr('\tmov %s, %%i0', (result)))
    prinstr(out, '', '! return(expr)')
    prinstr(out, '', '! return (end)')
    

def emit_skip(out, w):
    # Skip does nothing
    pass


def emit_break(out, w):
    current_registers = registers.top()
    prinstr(out, mkstr('\tba %s', breaklabels.top()), '! break')
    prinstr(out, '\tnop')


def emit_statement_block(out, w):
    emit_statements(out, w.children[0])


def eval_funargs(out, fun):
    global registers
    current_registers = registers.top()

    # Evaluate each argument and return the arg string
    arglist = []
    if fun.children[0].children:
        arglist = fun.children[0].children[0].children
    
    l = len(arglist)
    for arg in arglist:
        eval_expression(out, arg)
    # Return the no. of arguments
    return l


def emit_function_call(out, w):
    global registers
    current_registers = registers.top()
    prinstr(out, '', '! function call: ' + w.value + ' (start)')
    nargs = eval_funargs(out, w)

    # Adjust where we start storing args because
    # they come off of the stack in backwards order
    offset = 64 + (nargs - 1) * 4

    # Evaluate each argument
    for i in range(nargs):
        reg = current_registers.pop(out)
        do_store(out, reg, '%sp', offset)
        offset -= 4

    # Call the function
    mname = get_mangled_name(w.value)
    prinstr(out, mkstr('\tcall %s', (mname)))
    prinstr(out, '\tnop')
    prinstr(out, '', '! function call (end)')


def eval_expression(out, expr):
    global registers
    current_registers = registers.top()

    type = expr.type
    value = expr.value
    

    # This function returns k if n = 2^k. Otherwise returns None.
    def get_power_of_two(n):

        # Don't deal with negative values (or zero) for now
        if n <= 0:
            return None

        # Return log2(n) if n is a power of 2
        parts = math.modf(math.log(n) / math.log(2))

        # If the fractional parts is zero,
        # return the integer part
        if parts[0] == 0:
            return parts[1]

        # Otherwise, return None
        return None


    if type == 'function_call':
        emit_function_call(out, expr)

        # push the return value of the function call
        prinstr(out, '\tmov %%o0, %s' % (current_registers.push(out)))

    elif type == 'int':
        comment = '! push ' + str(value)

        # Small constant
        if abs(value) <= 4095:
            r = current_registers.push(out)
            prinstr(out, mkstr('\tmov %i, %s', (value, r)), comment)

        # Large constant
        else:
            r = current_registers.push(out)
            label = lmanager.new_label()
            prinstr(out, mkstr('\tsethi %%hi(%s), %%g1', (label)), comment)
            prinstr(out, mkstr('\tor    %%g1, %%lo(%s), %%g1', (label)))
            prinstr(out, mkstr('\tld    [%%g1], %s', (r)))
            prinstr(worddata, mkstr('%s:    .word    %i', (label, value)))

    elif type == 'float':
        comment = '! push ' + str(value)

        # Small constant
        if abs(value) <= 4095:
            r = current_registers.push(out)
            prinstr(out, mkstr('\tmov %f, %s', (value, r)), comment)

        # Large constant
        else:
            r = current_registers.push(out)
            label = lmanager.new_label()
            prinstr(out, mkstr('\tsethi %%hi(%s), %%g1', (label)), comment)
            prinstr(out, mkstr('\tor    %%g1, %%lo(%s), %%g1', (label)))
            prinstr(out, mkstr('\tld    [%%g1], %s', (r)))
            prinstr(worddata, mkstr('\n%s:    .float   %i', (label, value)))

    elif type == 'location':
        # Get the symbol table entry for the variable
        symbol = symtab.lookup_in(value, symtables.top())
        assert(symbol != None)

        # Get the current nesting level
        current_nest_level = symtab.lookup_in('$nestlev', symtables.top())
        assert(current_nest_level != None)

        # Get the nesting level for the location
        if not hasattr(symbol, 'nestlev'):
            symbol = do_global_lookup(value)
            
        assert(hasattr(symbol, 'nestlev'))
        symbol_nest_level = symbol.nestlev
        offset = symbol.frame_offset

        if symbol_nest_level != current_nest_level.size:
            # We need to load DISPLAY[symbol_nest_level] as
            # our %fp and reference the symbol from that

            # Load DISPLAY
            # r = current_registers.push(out)
            # prinstr(out, mkstr('\tset DISPLAY, %s', (r)))

            # Load DISPLAY[i] into a register
            # r = current_registers.pop(out)
            # r = '%i5'
            r = '%o5'
            s = current_registers.push(out)
            i = symbol_nest_level * 4
            prinstr(out, mkstr('\tld [%s + %d], %s',(r, i, s)))
            
        if expr.children:
            # This is an array element. We need to load its value
            # and how we handle that depends on whether it is a parameter
            # or not.
            
            # Evaluate the index expression
            eval_expression(out, expr.children[0])
            index = current_registers.pop(out)

            # Multiply the index by four to get the
            # no. of bytes from the base address
            r = current_registers.push(out)
            prinstr(out, mkstr('\tsll %s, 2, %s', (index, r)))

            if symbol.dimension and symbol.arg:
                # The array is a parameter. We need to load its address,
                # which is stored at [%fp + offset].

                # Load the address
                if current_nest_level.size != symbol_nest_level:
                    bytes = current_registers.pop(out)
                    s = current_registers.pop(out)
                    r = current_registers.push(out)
                    do_load(out, r, s, offset)
                else:
                    r = current_registers.push(out)
                    do_load(out, r, '%fp', offset)  


                # If the array is an argument, there is
                # an extra level of indirection. We have
                # to first get the address we computed
                # above.

                # Pop the address that we got
                # above from the do_load()
                addr = current_registers.pop(out)
                if current_nest_level.size == symbol_nest_level:
                    bytes = current_registers.pop(out)
                
                r = current_registers.push(out)

                # Add the no. bytes to that address
                # and load the value at that address
                prinstr(out, mkstr('\tld [%s + %s], %s', (addr, bytes, r)))
                return

            else:
                if current_nest_level.size != symbol_nest_level:
                    bytes = current_registers.pop(out)
                    s = current_registers.pop(out)
                    r = current_registers.push(out)
                    prinstr(out, mkstr('\tadd %s, %s, %s', (s, bytes, r)))
                else:
                    bytes = current_registers.pop(out)
                    r = current_registers.push(out)
                    # This is just a non-parameter array.
                    # Add the no. bytes to the frame pointer
                    prinstr(out, mkstr('\tadd %%fp, %s, %s', (bytes, r)))

            # Pop the result of the computation above
            fp_offset = current_registers.pop(out)

            # Load the value from memory
            do_load(out, current_registers.push(out), fp_offset, offset)

        elif hasattr(symbol, 'dimension') and symbol.dimension:
            # In this case the variable is an array that we are passing
            # as an argument. Again, we need to distinguish between the array
            # being local to this function or not. Either way, we need to
            # push its address, rather than its value, onto the call stack.
            comment = '! push address of ' + str(value)

            # Put the address on the call stack
            if -4096 <= offset and offset <= 4095:

                if symbol.arg:
                    # If it's an argument, we already
                    # have its address, so just load it
                    r = current_registers.push(out)
                    prinstr(out, mkstr('\tld [%%fp + %i], %s', (offset, r)))
                    # do_load(out, r, '%fp', offset)
                    prinstr(out, '', comment)

                else:
                    # Push the symbol offset
                    prinstr(out, mkstr('\tmov %i, %%g1', (abs(offset))))

                    if current_nest_level.size != symbol_nest_level:
                        # Get DISPLAY[i] off of the stack
                        s = current_registers.pop(out)

                    r = current_registers.push(out)
                    # Push the address onto the stack
                    if offset < 0:
                        if current_nest_level.size != symbol_nest_level:
                            prinstr(out, mkstr('\tsub %s, %%g1, %s', (s, r)))
                        else:
                            prinstr(out, mkstr('\tsub %%fp, %%g1, %s',(r)))
                    else:
                        if current_nest_level.size != symbol_nest_level:
                            prinstr(out, mkstr('\tadd %s, %%g1, %s', (s, r)))
                        else:
                            prinstr(out, mkstr('\tadd %%fp, %%g1, %s',(r)))
                    prinstr(out, '', comment)

            else:
                if current_nest_level.size != symbol_nest_level:
                    # Get DISPLAY[i] off of the stack
                    s = current_registers.pop(out)

                prinstr(out, mkstr('\tsethi %%hi(%i), %%g1', (offset)))
                prinstr(out, mkstr('\tor %%g1, %%lo(%i), %%g1', (offset)))

                if symbol.arg:
                    r = current_registers.push(out)
                    if current_nest_level.size != symbol_nest_level:
                        prinstr(out, mkstr('\tld [%s + %%g1], %s',(s, r)),
                                comment)
                    else:
                        # Load the address at [%fp + offset]
                        prinstr(out, mkstr('\tld [%%fp + %%g1], %s',(r)),
                                comment)
                else:
                    r = current_registers.push(out)
                    if current_nest_level.size != symbol_nest_level:
                        prinstr(out, mkstr('\tadd %s, %%g1, %s',(s, r)))
                    else:
                        prinstr(out, mkstr('\tadd %%fp, %%g1, %s',(r)))
                    prinstr(out, '', comment)
 
        else:
            # Non-array variable
            comment = '! push ' + str(value)
            if current_nest_level.size != symbol_nest_level:
                s = current_registers.pop(out)
                r = current_registers.push(out)
                do_load(out, r, s, offset)
            else:
                do_load(out, current_registers.push(out), '%fp', offset)
            prinstr(out, '', comment)

    elif type == 'plus':
        left = expr.children[0]
        right = expr.children[1]
        eval_expression(out,left)
        eval_expression(out,right)
        r = current_registers.pop(out)
        l = current_registers.pop(out)
        prinstr(out, mkstr('\tadd %s, %s, %s',
                             (l, r, current_registers.push(out))), '! add')

    elif type == 'minus':
        left = expr.children[0]
        right = expr.children[1]
        eval_expression(out,left)
        eval_expression(out,right)
        r = current_registers.pop(out)
        l = current_registers.pop(out)        
        prinstr(out, mkstr('\tsub %s, %s, %s',
                             (l, r, current_registers.push(out))), '! sub')


    elif type == 'times':

        left = expr.children[0]
        right = expr.children[1]

        # If either the left or the right operand is a power of
        # two, use bit shifting to do the multiply
        lpower = None
        rpower = None

        if left.type == 'int':
            lpower = get_power_of_two(abs(left.value))

        if right.type == 'int':
            rpower = get_power_of_two(abs(right.value))

        if lpower:
            eval_expression(out, right)
            a = current_registers.pop(out)
            b = current_registers.push(out)
            comment = '! fast mul by ' + str(abs(left.value))
            prinstr(out, mkstr('\tsll %s, %d, %s', (a, lpower, b)), comment)
            if left.value < 0:
                comment = '! negate result'
                prinstr(out, mkstr('\tneg %s, %s', (b, b)), comment)
        elif rpower:
            eval_expression(out, left)
            a = current_registers.pop(out)
            b = current_registers.push(out)
            comment = '! fast mul by ' + str(abs(right.value))
            prinstr(out, mkstr('\tsll %s, %d, %s', (a, rpower, b)), comment)
            if right.value < 0:
                comment = '! negate result'
                prinstr(out, mkstr('\tneg %s, %s', (b, b)), comment)
        else:
            eval_expression(out, left)
            eval_expression(out, right)
            r = current_registers.pop(out)
            l = current_registers.pop(out)
            prinstr(out, mkstr('\tmov %s, %%o0', (l)), '! mul')
            prinstr(out, '\tcall .mul')
            prinstr(out, mkstr('\tmov %s, %%o1', (r)))
            rg = current_registers.push(out)
            prinstr(out, mkstr('\tmov %%o0, %s', (rg)))
            
    elif type == 'divide':
        left = expr.children[0]
        right = expr.children[1]

        # If either the left or the right operand is a power of
        # two, use bit shifting to do the divide
        lpower = None
        rpower = None

        if left.type == 'int':
            lpower = get_power_of_two(left.value)

        if right.type == 'int':
            rpower = get_power_of_two(right.value)

        if lpower:
            eval_expression(out, right)
            a = current_registers.pop(out)
            b = current_registers.push(out)
            prinstr(out,
                    mkstr('\tsra %s, %d, %s', (a, lpower, b)),
                    '! fast div by ' + str(left.value))
        elif rpower:
            eval_expression(out, left)
            a = current_registers.pop(out)
            b = current_registers.push(out)
            prinstr(out,
                    mkstr('\tsra %s, %d, %s', (a, rpower, b)),
                    '! fast div by ' + str(right.value))
        else:
            eval_expression(out,left)
            eval_expression(out,right)
            r = current_registers.pop(out)
            l = current_registers.pop(out)
            prinstr(out, mkstr('\tmov %s, %%o0', (l)), '! div')
            prinstr(out, '\tcall .div')
            prinstr(out, mkstr('\tmov %s, %%o1', (r)))
            prinstr(out,
                    mkstr('\tmov %%o0, %s', (current_registers.push(out))))
                    
    elif type == 'uminus':
        eval_expression(out, expr.children[0])
        operand = current_registers.pop(out)
        prinstr(out,
                mkstr('\tneg %s, %s',
                        (operand, current_registers.push(out))),
                '! unary minus')

    elif type == 'int_cast':
        eval_expression(out, expr.children[0])
        prinstr(out, '', '! int()')

    elif type == 'float_cast':
        eval_expression(out, expr.children[0])
        prinstr(out, '', '! float()')
        

def eval_relation(out, rel):
    global registers
    global lmanager
    current_registers = registers.top()
    t = rel.type

    if t == 'not':
        eval_relation(out, rel.children[0])
        operand = current_registers.pop(out)
        prinstr(out,
                mkstr('\txor %s, 1, %s',
                        (operand, current_registers.push(out))), '! not')

    elif t == 'and':
        left = rel.children[0]
        right = rel.children[1]
        eval_relation(out,left)
        eval_relation(out,right)
        r = current_registers.pop(out)
        l = current_registers.pop(out)
        prinstr(out, mkstr('\tand %s, %s, %s',
                             (l, r, current_registers.push(out))), '! and')

    elif t == 'or':
        left = rel.children[0]
        right = rel.children[1]
        eval_relation(out,left)
        eval_relation(out,right)
        r = current_registers.pop(out)
        l = current_registers.pop(out)
        prinstr(out, mkstr('\tor %s, %s, %s',
                             (l, r, current_registers.push(out))), '! or')

    elif t == '==':
        left = rel.children[0]
        right = rel.children[1]
        eval_expression(out,left)
        eval_expression(out,right)
        r = current_registers.pop(out)
        l = current_registers.pop(out)
        label = lmanager.new_label()
        prinstr(out, mkstr('\tcmp %s, %s', (l, r)), '! == test')
        prinstr(out, mkstr('\tbe %s', (label)))
        r = current_registers.push(out)
        prinstr(out, mkstr('\tmov 1, %s', (r)))
        prinstr(out, mkstr('\tmov 0, %s', (r)))
        prinstr(out, mkstr('%s:', (label)))

    elif t == '!=':
        left = rel.children[0]
        right = rel.children[1]
        eval_expression(out,left)
        eval_expression(out,right)
        r = current_registers.pop(out)
        l = current_registers.pop(out)
        label = lmanager.new_label()
        prinstr(out, mkstr('\tcmp %s, %s', (l, r)), '! != test')
        prinstr(out, mkstr('\tbne %s', label))
        r = current_registers.push(out)
        prinstr(out, mkstr('\tmov 1, %s', (r)))
        prinstr(out, mkstr('\tmov 0, %s', (r)))
        prinstr(out, mkstr('%s:', (label)))

    elif t == '<':
        left = rel.children[0]
        right = rel.children[1]
        eval_expression(out,left)
        eval_expression(out,right)
        r = current_registers.pop(out)
        l = current_registers.pop(out)
        label = lmanager.new_label()
        prinstr(out, mkstr('\tcmp %s, %s', (l, r)), '! < test')
        prinstr(out, mkstr('\tbl %s', (label)))
        r = current_registers.push(out)
        prinstr(out, mkstr('\tmov 1, %s', (r)))
        prinstr(out, mkstr('\tmov 0, %s', (r)))
        prinstr(out, mkstr('%s:', (label)))

    elif t == '>':
        left = rel.children[0]
        right = rel.children[1]
        eval_expression(out,left)
        eval_expression(out,right)
        r = current_registers.pop(out)
        l = current_registers.pop(out)
        label = lmanager.new_label()
        prinstr(out, mkstr('\tcmp %s, %s', (l, r)), '! > test')
        prinstr(out, mkstr('\tbg %s', (label)))
        r = current_registers.push(out)
        prinstr(out, mkstr('\tmov 1, %s', (r)))
        prinstr(out, mkstr('\tmov 0, %s', (r)))
        prinstr(out, mkstr('%s:', (label)))

    elif t == '<=':
        left = rel.children[0]
        right = rel.children[1]
        eval_expression(out,left)
        eval_expression(out,right)
        r = current_registers.pop(out)
        l = current_registers.pop(out)
        label = lmanager.new_label()
        prinstr(out, mkstr('\tcmp %s, %s', (l, r)), '! <= test')
        prinstr(out, mkstr('\tble %s', (label)))
        r = current_registers.push(out)
        prinstr(out, mkstr('\tmov 1, %s', (r)))
        prinstr(out, mkstr('\tmov 0, %s', (r)))
        prinstr(out, mkstr('%s:', (label)))

    elif t == '>=':
        left = rel.children[0]
        right = rel.children[1]
        eval_expression(out,left)
        eval_expression(out,right)
        r = current_registers.pop(out)
        l = current_registers.pop(out)
        label = lmanager.new_label()
        prinstr(out, mkstr('\tcmp %s, %s', (l, r)), '! >= test')
        prinstr(out, mkstr('\tbge %s', label))
        r = current_registers.push(out)
        prinstr(out, mkstr('\tmov 1, %s', (r)))
        prinstr(out, mkstr('\tmov 0, %s', (r)))
        prinstr(out, mkstr('%s:', (label)))
