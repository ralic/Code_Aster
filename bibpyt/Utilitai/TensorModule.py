# coding=utf-8

# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
# (AT YOUR OPTION) ANY LATER VERSION.
#
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.
#
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
# ======================================================================

# =====================================================================
# Based on tensor.py written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# =====================================================================

import unittest
import numpy as NP

try:
    import sympy
    X, Y, Z = sympy.symbols('X Y Z')
    HAVE_SYMPY = True
except ImportError:
    HAVE_SYMPY = False

def flatten(x):
    """flatten(sequence) -> list

    Returns a single, flat list which contains all elements retrieved
    from the sequence and all recursively contained sub-sequences
    (iterables).

    Examples:
    >>> [1, 2, [3,4], (5,6)]
    [1, 2, [3, 4], (5, 6)]
    >>> flatten([[[1,2,3], (42,None)], [4,5], [6], 7, MyVector(8,9,10)])
    [1, 2, 3, 42, None, 4, 5, 6, 7, 8, 9, 10]"""

    result = []
    for el in x:
        if hasattr(el, "__iter__") and not isinstance(el, basestring):
            result.extend(flatten(el))
        else:
            result.append(el)
    return result

class Tensor:

    is_tensor = 1

    def __init__(self, elements, nocheck = None):

        self.array = NP.array(elements)
        if nocheck is None:
            if not NP.logical_and.reduce(NP.equal(NP.array(self.array.shape), 3)):
                raise ValueError('Tensor must have length 3 along any axis')
        self.rank = len(self.array.shape)

    def __repr__(self):
        return str(self)

    def __str__(self):
        s = 'TensorModule.Tensor(numpy.array(' + str(self.array) + ')  )'
        s = s.replace('\n', ',')
        return s

    def __add__(self, other):
        return Tensor(self.array+other.array, 1)
    __radd__ = __add__

    def __neg__(self):
        return Tensor(-self.array, 1)

    def __sub__(self, other):
        return Tensor(self.array-other.array, 1)

    def __rsub__(self, other):
        return Tensor(other.array-self.array, 1)

    def __rmul__(self, other):
        if isTensor(other):
            return Tensor(self.array*other.array, 1)
        else:
            return Tensor(self.array*other, 1)

    def __div__(self, other):
        if isTensor(other):
            if other.rank == 0:
                return Tensor(self.array/other.array, 1)
            else:
                raise TypeError("Can't divide by a tensor")
        else:
            return Tensor(self.array/(1.*other), 1)

    def __rdiv__(self, other):
        raise TypeError("Can't divide by a tensor")

    def __cmp__(self, other):
        if not isTensor(other):
            return NotImplemented
        if self.rank != other.rank:
            return 1
        else:
            return not NP.logical_and.reduce(
                NP.equal(self.array, other.array).flat)

    def __len__(self):
        return 3

    def __getitem__(self, index):
        elements = self.array[index]
        if type(elements) == type(self.array):
            return Tensor(elements)
        else:
            return elements

    def dot(self, other):
        if isTensor(other):
            a = self.array
            b =  NP.transpose(other.array, range(1, other.rank)+[0])
            return Tensor(NP.inner(a, b), 1)
        else:
            return Tensor(self.array*other, 1)

    def sqrt(self):
        return Tensor(self.array**0.5, 1)

    def diagonal(self, axis1=0, axis2=1):
        if self.rank == 2:
            return Tensor([self.array[0, 0], self.array[1, 1], self.array[2, 2]])
        else:
            if axis2 < axis1:
                axis1, axis2 = axis2, axis1
            raise ValueError('Not yet implemented')

    def trace(self, axis1=0, axis2=1):
        if self.rank == 2:
            return self.array[0, 0]+self.array[1, 1]+self.array[2, 2]
        else:
            raise ValueError('Not yet implemented')

    def transpose(self):
        return Tensor(NP.transpose(self.array))

    def determinant(self):
        if NP.shape(self.array) == (3, 3) :
            M = self
            a = M[0, 0]
            b = M[0, 1]
            c = M[0, 2]
            d = M[1, 0]
            e = M[1, 1]
            f = M[1, 2]
            g = M[2, 0]
            h = M[2, 1]
            i = M[2, 2]
            Determinant = a*(e*i - f*h) - b*(d*i - f*g) +  c*(d*h - e*g)
            return Determinant
        else :
            raise ValueError ('Tenseur must of rank 2 and shape 3 by 3')

    def inverse(self):
        if NP.shape(self.array) == (3, 3) :
            M = self
            a = M[0, 0]
            b = M[0, 1]
            c = M[0, 2]
            d = M[1, 0]
            e = M[1, 1]
            f = M[1, 2]
            g = M[2, 0]
            h = M[2, 1]
            i = M[2, 2]
            Mprime =  NP.array([[  e*i - f*h,   c*h - b*i,  b*f - c*e  ], [  f*g - d*i,   a*i - c*g,  c*d - a*f  ], [  d*h - e*g,   b*g - a*h,  a*e - b*d  ]])
            det = M.determinant()
            Inverse = Mprime*(1/det)
            return Tensor(Inverse)
        else :
            raise ValueError ('Tenseur must of rank 2 and shape 3 by 3')

    def symmetricalPart(self):
        if self.rank == 2:
            return Tensor(0.5*(self.array + \
                               NP.transpose(self.array,
                                           NP.array([1, 0]))),
                          1)
        else:
            raise ValueError('Not yet implemented')

    def matrixmultiply(self, other):
        if self.rank == 2 and other.rank == 2:
            return Tensor(NP.matrixmultiply(NP.transpose(self.array), other.array))
        else:
            raise ValueError('Tenseur must of rank 2')

    def asymmetricalPart(self):
        if self.rank == 2:
            return Tensor(0.5*(self.array - \
                               NP.transpose(self.array,
                                           NP.array([1, 0]))),
                          1)
        else:
            raise ValueError('Not yet implemented')

    def eigenvalues(self):
        if self.rank == 2:
            return eigenvals(self.array)
        else:
            raise ValueError('Undefined operation')

    def diagonalization(self):
        if self.rank == 2:
            ev, vectors = eigenvects(self.array)
            return ev, Tensor(vectors)
        else:
            raise ValueError, 'Undefined operation'

    def sympyVariables(self):
        variablesList = []
        for exp in NP.ravel(self.array):
            try:
                variablesList.append(exp.atoms(sympy.Symbol))
            except:
                pass
        if len(variablesList) > 3:
            raise ValueError('sympy Tensor must have less than 3 sympy variables')
        variablesSet = set(flatten(variablesList))
        return list(sorted(variablesSet))

    def produitDoubleContracte(self, other):
        if self.rank >= 2 and other.rank >= 2:
            resultat = NP.resize(0, [3]*(self.rank+other.rank-4))
            for j in range(3):
                resultat = resultat+NP.inner(NP.transpose(self.array[j]), other.array[j])
        else :
            raise ValueError('range of each Tensor must be at least 2')
        return  Tensor(resultat)

    def produitSimpleContracte(self, other):
        rank = self.rank + other.rank - 2
        # Ruse pour produire un objet Sympy nul
        out_array = NP.array([X]*(3**rank))
        out_array.shape = [3, ]*rank
        if not (self.rank >= 1 and other.rank >= 1):
            raise ValueError('range of each Tensor must be at least 1')
        if self.rank == 1 and other.rank == 1:
            out_array = NP.dot(self.array, other.array)
        elif self.rank == 2 and other.rank == 1:
            for i in xrange(3):
                out_array[i] = NP.dot(self.array[i,:], other.array)
        elif self.rank == 2 and other.rank == 2:
            for i in xrange(3):
                for j in xrange(3):
                    out_array[i][j] = NP.dot(self.array[i,:], other.array[:, j])
        elif self.rank == 4 and other.rank == 2:
            for i in xrange(3):
                for j in xrange(3):
                    for k in xrange(3):
                        for l in xrange(3):
                            out_array[i][j][k][l] = NP.dot(self.array[i, j, k,:], other.array[:, l])
        else :
            raise NotImplemented
        return  Tensor(out_array)

def isTensor(x):
    return isinstance(x, Tensor)

def grad(F):
    if not isTensor(F):
        raise ValueError('Argument must be a Tensor')
    varList = [X, Y, Z] #F.sympyVariables()
    gradF = NP.resize(F.array, flatten((F.array.shape, 3)))
    # see http://code.google.com/p/sympy/issues/detail?id=2622
    def _diff(elt, symb):
        """apply sympy.diff on 'elt' or each element of 'elt' if iterable"""
        diff = lambda x: sympy.diff(x, symb)
        try:
            res = map(diff, elt)
        except TypeError:
            res = diff(elt)
        return res
    for i in range(3):
        gradF[i] = [_diff(elt, varList[i]) for elt in gradF[i].tolist()]
    return Tensor(NP.transpose(NP.array(gradF)))

def div(F):
    if not isTensor(F):
        raise ValueError('Argument must be a Tensor')
    varList = [X, Y, Z]#F.sympyVariables()
    if F.rank == 0:
        raise ValueError('Divergence just applies on Tensor with rank>0')
    elif F.rank == 1:
        return Tensor(sympy.diff(F[0], varList[0]) + sympy.diff(F[1], varList[1]) + sympy.diff(F[2], varList[2]))
    elif F.rank == 2:
        return Tensor([sympy.diff(F[0][0], varList[0]) + sympy.diff(F[0][1], varList[1]) + sympy.diff(F[0][2], varList[2]),
                       sympy.diff(F[1][0], varList[0]) + sympy.diff(F[1][1], varList[1]) + sympy.diff(F[1][2], varList[2]),
                       sympy.diff(F[2][0], varList[0]) + sympy.diff(F[2][1], varList[1]) + sympy.diff(F[2][2], varList[2])])
    elif F.rank == 3:
        return Tensor([[sympy.diff(F[0][0][0], varList[0]) + sympy.diff(F[0][0][1], varList[1]) + sympy.diff(F[0][0][2], varList[2]),
                        sympy.diff(F[0][1][0], varList[0]) + sympy.diff(F[0][1][1], varList[1]) + sympy.diff(F[0][1][2], varList[2]),
                        sympy.diff(F[0][2][0], varList[0]) + sympy.diff(F[0][2][1], varList[1]) + sympy.diff(F[0][2][2], varList[2])],
                       [sympy.diff(F[1][0][0], varList[0]) + sympy.diff(F[1][0][1], varList[1]) + sympy.diff(F[1][0][2], varList[2]),
                        sympy.diff(F[1][1][0], varList[0]) + sympy.diff(F[1][1][1], varList[1]) + sympy.diff(F[1][1][2], varList[2]),
                        sympy.diff(F[1][2][0], varList[0]) + sympy.diff(F[1][2][1], varList[1]) + sympy.diff(F[1][2][2], varList[2])],
                       [sympy.diff(F[2][0][0], varList[0]) + sympy.diff(F[2][0][1], varList[1]) + sympy.diff(F[2][0][2], varList[2]),
                        sympy.diff(F[2][1][0], varList[0]) + sympy.diff(F[2][1][1], varList[1]) + sympy.diff(F[2][1][2], varList[2]),
                        sympy.diff(F[2][2][0], varList[0]) + sympy.diff(F[2][2][1], varList[1]) + sympy.diff(F[2][2][2], varList[2])]]
                       )
    else:
        raise ValueError('Not implemented for Tensor of rank > 3')

def laplacien(F):
    if not isTensor(F):
        raise ValueError('Argument must be a Tensor')
    LapF = div(grad(F))
    return LapF

def gradsym(F):
    if not isTensor(F):
        raise ValueError('Argument must be a Tensor')
    gradsymF = 0.5 * (grad(F) + grad(F).transpose())
    return gradsymF


# 
class TensorUnitTest(unittest.TestCase):

    def setUp(self):
        if not HAVE_SYMPY:
            return
        self.U = Tensor(NP.array(([X**3, sympy.sin(X), sympy.exp(X)],
                                  [Y**3, sympy.sin(Y), sympy.exp(Y)],
                                  [Z**3, sympy.sin(Z), sympy.exp(Z)])))

    def testType(self):
        if not HAVE_SYMPY:
            return
        self.assertEqual(isTensor(self.U), 1)

    def testRank(self):
        if not HAVE_SYMPY:
            return
        self.assertEqual(self.U.rank, 2)
        self.assertEqual(grad(self.U).rank, 3)

    def testGrad(self):
        if not HAVE_SYMPY:
            return
        self.assertEqual(grad(self.U), Tensor(NP.array([[[3*X**2, 0, 0 ], [0, 3*Y**2, 0 ], [0, 0, 3*Z**2 ]], [[sympy.cos(X), 0, 0 ], [0, sympy.cos(Y), 0 ], [0, 0, sympy.cos(Z) ]], [[sympy.exp(X), 0, 0 ], [0, sympy.exp(Y), 0 ], [0, 0, sympy.exp(Z) ]]])))

    def testGradSym(self):
        if not HAVE_SYMPY:
            return
        # attention: sensible 3.0*X**2 != 3*X**2
        self.assertEqual(gradsym(self.U), Tensor(NP.array(
            [[[3.0*X**2, 0.5*sympy.cos(X), 0.5*sympy.exp(X)], [0, 1.5*Y**2, 0], [0, 0, 1.5*Z**2]],
             [[0.5*sympy.cos(X), 0, 0], [1.5*Y**2, 1.*sympy.cos(Y), 0.5*sympy.exp(Y)], [0, 0, 0.5*sympy.cos(Z)]],
             [[0.5*sympy.exp(X), 0, 0], [0, 0.5*sympy.exp(Y), 0], [1.5*Z**2, 0.5*sympy.cos(Z), 1.*sympy.exp(Z)]]])))

    def testLaplacien(self):
        if not HAVE_SYMPY:
            return
        self.assertEqual(laplacien(self.U), Tensor(NP.array([[6*X, 6*Y, 6*Z ], [-sympy.sin(X), -sympy.sin(Y), -sympy.sin(Z) ], [sympy.exp(X), sympy.exp(Y), sympy.exp(Z) ]])  ))

    def testDivergence(self):
        if not HAVE_SYMPY:
            return
        self.assertEqual(div(grad(self.U)),
                         Tensor(NP.array([[6*X, 6*Y, 6*Z ], [-sympy.sin(X), -sympy.sin(Y), -sympy.sin(Z) ], [sympy.exp(X), sympy.exp(Y), sympy.exp(Z) ]])  ))

    def testProduitDoubleContracte(self):
        if not HAVE_SYMPY:
            return
        TensO4Sym = Tensor(NP.array([[[[ 400.,           0.,           0.,       ],
                                    [   0.,         200.,           0.,       ],
                                    [   0.,           0.,         200.,       ]],
                                   [[   0.,          66.66666667,   0.,       ],
                                    [  66.66666667,   0.,           0.,       ],
                                    [   0.,           0.,           0.,       ]],
                                   [[   0.,           0.,         133.33333333],
                                    [   0.,           0.,           0.,       ],
                                    [ 133.33333333,   0.,           0.,       ]]],
                                  [[[   0.,          66.66666667,   0.,       ],
                                    [  66.66666667,   0.,           0.,       ],
                                    [   0.,           0.,           0.,       ]],
                                   [[ 200.,           0.,           0.,       ],
                                    [   0.,         233.33333333,   0.,       ],
                                    [   0.,           0.,         166.66666667]],
                                   [[   0.,           0.,           0.,       ],
                                    [   0.,           0.,          66.66666667],
                                    [   0.,          66.66666667,   0.,       ]]],
                                  [[[   0.,           0.,         133.33333333],
                                    [   0.,           0.,           0.,       ],
                                    [ 133.33333333,   0.,           0.,       ]],
                                   [[   0.,           0.,           0.,       ],
                                    [   0.,           0.,          66.66666667],
                                    [   0.,          66.66666667,   0.,       ]],
                                   [[ 200.,           0.,           0.,       ],
                                    [   0.,         166.66666667,   0.,       ],
                                    [   0.,           0.,         233.33333333]]]])  )

        self.assertEqual(TensO4Sym.produitDoubleContracte(self.U),
                         Tensor(NP.array([[200.000000000000*sympy.sin(Y) + 400.000000000000*X**3 + 200.000000000000*sympy.exp(Z), 66.6666666700000*sympy.sin(X) + 66.6666666700000*Y**3, 133.333333330000*Z**3 + 133.333333330000*sympy.exp(X) ],
                                          [66.6666666700000*sympy.sin(X) + 66.6666666700000*Y**3, 233.333333330000*sympy.sin(Y) + 200.000000000000*X**3 + 166.666666670000*sympy.exp(Z),       66.6666666700000*sympy.sin(Z) + 66.6666666700000*sympy.exp(Y) ],
                                          [133.333333330000*Z**3 + 133.333333330000*sympy.exp(X),       66.6666666700000*sympy.sin(Z) + 66.6666666700000*sympy.exp(Y),       166.666666670000*sympy.sin(Y) + 200.000000000000*X**3 + 233.333333330000*sympy.exp(Z) ]])))

    def testproduitSimpleContracte(self):
        if not HAVE_SYMPY:
            return
        self.assertEqual(self.U.produitSimpleContracte(Tensor(NP.array([-1, 0, 0]))), Tensor(NP.array([-X**3, -Y**3, -Z**3])))


if __name__ == '__main__':
    unittest.main()
