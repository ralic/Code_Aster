#@ MODIF funct_root Utilitai  DATE 14/09/2004   AUTEUR MCOURTOI M.COURTOIS 
# -*- coding: iso-8859-1 -*-
################################################################################
#       Mathematical utility routines
#       Copyright (C) 1999, Wesley Phoa
#
#       Reference: Numerical Recipes in C
#       [[[[extraits]]]]

class BracketingException(Exception):
        pass

class RootFindingException(Exception):
        pass

class MinimizationException(Exception):
        pass

GOLDEN = (1+5**.5)/2

# 
# MISCELLANEOUS
#

def sgn(x):
        if x==0:
                return 0
        else:
                return x/abs(x)

#
# UNIVARIATE ROOT FINDING
#

def bracket_root(f, interval, max_iterations=50):
        """\
Given a univariate function f and a tuple interval=(x1,x2),
return a new tuple (bracket, fnvals) where bracket=(x1,x2)
brackets a root of f and fnvals=(f(x1),f(x2)).
        """
        x1, x2 = interval
        if x1==x2:
                 raise BracketingException("initial interval has zero width")
        elif x2<x1:
                x1, x2 = x2, x1
        f1, f2 = f(x1), f(x2)
        for j in range(max_iterations):
                while f1*f2 >= 0:  # not currently bracketed
                        if abs(f1)<abs(f2):
                                x1 = x1 + GOLDEN*(x1-x2)
                        else:
                                x2 = x2 + GOLDEN*(x2-x1)
                        f1, f2 = f(x1), f(x2)
                return (x1, x2), (f1, f2)
        raise BracketingException("too many iterations")

def ridder_root(f, bracket, fnvals=None, accuracy=1e-6, max_iterations=50):
        """\
Given a univariate function f and a tuple bracket=(x1,x2) bracketing a root,
find a root x of f using Ridder s method. Parameter fnvals=(f(x1),f(x2)) is optional.
        """
        x1, x2 = bracket
        if fnvals==None:
                f1, f2 = f(x1), f(x2)
        else:
                f1, f2 = fnvals
        if f1==0:
                return x1
        elif f2==0:
                return x2
        elif f1*f2>=0:
                raise BracketingException("initial interval does not bracket a root")
        x4 = 123456789.
        for j in range(max_iterations):
                x3 = (x1+x2)/2
                f3 = f(x3)
                temp = f3*f3 - f1*f2
                x4, x4old = x3 + (x3-x1)*sgn(f1-f2)*f3/temp**.5, x4
                f4 = f(x4)
                if f1*f4<0:  # x1 and x4 bracket root
                        x2, f2 = x4, f4
                else:  # x4 and x2 bracket root
                        x1, f1 = x4, f4
                if min(abs(x1-x2),abs(x4-x4old))<accuracy or temp==0:
                        return x4
        raise RootFindingException("too many iterations")

def root(f, interval=(0.,1.), accuracy=1e-4, max_iterations=50):
        """\
Given a univariate function f and an optional interval (x1,x2),
find a root of f using bracket_root and ridder_root.
        """
        bracket, fnvals = bracket_root(f, interval, max_iterations)
        return ridder_root(f, bracket, fnvals, accuracy, max_iterations)

