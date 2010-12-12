#!/usr/bin/env python

ops = [ '||', '&&', '==', '!='
      , '<' , '<=', '>' , '>='
      , '+' , '-' , '*' , '/' ]

eqs  = ['==', '!=']

ords = ['<', '<=', '>', '>=']

def prec(op):
  return \
    { '||' : 0
    , '&&' : 1
    , '==' : 2
    , '!=' : 2
    , '<'  : 3
    , '<=' : 3
    , '>'  : 3
    , '>=' : 3
    , '+'  : 4
    , '-'  : 4
    , '*'  : 5
    , '/'  : 5
    }[op]

def nonassoc(op1, op2): 
  return \
    (op1 in eqs  and op2 in eqs) or \
    (op1 in ords and op2 in ords)

top = []
bot = []

for o1 in ops:
  for o2 in ops:
    t = 'a = e %2s e %2s e;' % (o1, o2)

    if prec(o1) < prec(o2):
      b = 'a = e %2s (e %2s e);' % (o1, o2)
    else:
      b = 'a = (e %2s e) %2s e;' % (o1, o2)

    if nonassoc(o1, o2):
      t = b

    top.append(t)
    bot.append(b)

print '''
orig a
expr e

---

%s

+++

%s

''' % ('\n'.join(top), '\n'.join(bot))

