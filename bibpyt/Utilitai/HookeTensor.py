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

import unittest
import numpy as N

try:
    import sympy
    import TensorModule
    X, Y, Z = sympy.symbols('X Y Z')
    HAVE_SYMPY = True
except ImportError:
    HAVE_SYMPY = False


def kron(i, j):
    if i == j:
        a = 1
    else:
        a = 0
    return a


def antikron(i, j):
    if i == j:
        a = 0.
    else:
        a = 1.
    return a


def dif(i, j, n, m):
    if i == n and j == m:
        a = -1.
    elif i == m and j == n:
        a = 1.
    else:
        a = 0.
    return a


def HookeIsotropic(E, NU):
    lamda = E * NU / ((1 + NU) * (1 - 2 * NU))
    mu = E / (2 * (1 + NU))
    C = N.resize(None, (3, 3, 3, 3))
    for i in range(3):
        for j in range(3):
            for k in range(3):
                for l in range(3):
                    C[i][j][k][l] = lamda * kron(i, j) * kron(k, l) + mu * (
                        kron(i, k) * kron(j, l) + kron(i, l) * kron(j, k))
    return TensorModule.Tensor(C)


# definition des matrices des angles rotation nautique
def Rotation(alpha, beta, gamma):
    alpha = float(alpha)
    beta = float(beta)
    gamma = float(gamma)
    Ralpha = N.resize(0., (3, 3))
    Rbeta = N.resize(0., (3, 3))
    Rgamma = N.resize(0., (3, 3))
    for i in range(3):
        for j in range(3):
            Ralpha[i, j] = sympy.cos(alpha * antikron(i, 2)) * kron(
                i, j) + sympy.sin(alpha * dif(i, j, 0, 1))
           # Rbeta[i,j]=sympy.cos(-beta*antikron(i,1))*kron(i,j)+dif(i,j,2,1)*sympy.sin(-beta)*antikron(i,j)
           # Rgamma[i,j]=sympy.cos(gamma*antikron(i,0))*kron(i,j)+dif(i,j,1,0)*sympy.sin(gamma)*antikron(i,j)
    Rotation = Ralpha  # sympy.Matrix(Ralpha.tolist())*sympy.Matrix(Rbeta.tolist())*sympy.Matrix(Rgamma.tolist())
    return TensorModule.Tensor(N.array(Rotation.tolist()))


def HookeIsotropicP(E, NU):
    lamda = E * NU / ((1. + NU) * (1. - 2. * NU))
    mu = E / (2. * (1. + NU))
    A = ((lamda * ones(3) + 2 * mu * eye(3)).row_join(zeros(3)))
    B = zeros(3).row_join(mu * eye(3))
    C = A.col_join(B)
    return TensorModule.Tensor(N.array(C))


def HookeOrthotropic(E_L, E_T, E_N, NU_LT, NU_LN, NU_TN, G_LT, G_LN, G_TN):
    E_L = float(E_L)
    E_T = float(E_T)
    E_N = float(E_N)
    NU_LT = float(NU_LT)
    NU_TN = float(NU_TN)
    NU_LN = float(NU_LN)
    G_LT = float(G_LT)
    G_TN = float(G_TN)
    G_LN = float(G_LN)
    NU_TL = (E_T / E_L) * NU_LT
    NU_NT = (E_N / E_T) * NU_TN
    NU_NL = (E_N / E_L) * NU_LN
    DELTA = (1 - NU_TN * NU_NT - NU_NL * NU_LN -
             NU_LT * NU_TL - 2 * NU_TN * NU_NL * NU_LT) / (E_L * E_T * E_N)
    C_reduit = N.resize(0., (6, 6))
    C_reduit[0, 0] = (1 - NU_TN * NU_NT) / (E_T * E_N)
    C_reduit[0, 1] = (
        NU_TL + NU_NL * NU_TN) / (E_T * E_N)
    C_reduit[0, 2] = (NU_NL + NU_TL * NU_NT) / (E_T * E_N)
    C_reduit[1, 0] = (NU_LT + NU_LN * NU_NT) / (E_L * E_N)
    C_reduit[1, 1] = (
        1 - NU_NL * NU_LN) / (E_L * E_N)
    C_reduit[1, 2] = (NU_NT + NU_LT * NU_NL) / (E_L * E_N)
    C_reduit[2, 0] = (NU_LN + NU_LT * NU_TN) / (E_L * E_T)
    C_reduit[2, 1] = (
        NU_TN + NU_TL * NU_LN) / (E_L * E_T)
    C_reduit[2, 2] = (1 - NU_LT * NU_TL) / (E_L * E_T)
    C_reduit[3, 3] = G_LT * DELTA
    C_reduit[4, 4] = G_LN * DELTA
    C_reduit[5, 5] = G_TN * DELTA
    C_reduit = C_reduit / DELTA
    C = N.resize(0., (3, 3, 3, 3))
    I = N.array([[0, 3, 4], [3, 1, 5], [4, 5, 2]])
    K = I
    for i in range(3):
        for j in range(3):
            for k in range(3):
                for l in range(3):
                    C[i][j][k][l] = C_reduit[I[i][j], K[k][l]]
    return TensorModule.Tensor(C)


def HookeOrthotropicOrienteQuelconque(E_L, E_T, E_N, NU_LT, NU_LN, NU_TN, G_LT, G_LN, G_TN, alpha, beta, gamma):
    R = Rotation(alpha, beta, gamma)
    H_Ortho = HookeOrthotropic(
        E_L, E_T, E_N, NU_LT, NU_LN, NU_TN, G_LT, G_LN, G_TN)
    Tens = N.resize(0., (3, 3, 3, 3))
    for i in range(3):
        for j in range(3):
            for k in range(3):
                for l in range(3):
                    for I in range(3):
                        for J in range(3):
                            for K in range(3):
                                for L in range(3):
                                    Tens[i][j][k][l] = Tens[i][j][k][l] + R[i, I] * R[
                                        j, J] * R[k, K] * R[l, L] * H_Ortho[I][J][K][L]
    return TensorModule.Tensor(Tens)


class TensorUnitTest(unittest.TestCase):

    def setUp(self):
        if not HAVE_SYMPY:
            return
        self.U = TensorModule.Tensor(N.array(([X ** 3, Y ** 3, Z ** 3])))

    def testType(self):
        if not HAVE_SYMPY:
            return
        self.assertEqual(TensorModule.isTensor(self.U), 1)

    def testRank(self):
        if not HAVE_SYMPY:
            return
        self.assertEqual(self.U.rank, 1)
        self.assertEqual(TensorModule.grad(self.U).rank, 2)

    def testProduitDoubleContracte(self):
        if not HAVE_SYMPY:
            return
        tensDiff = (HookeOrthotropic(200., 100., 150., 0.4, 0.2, 0.3, 100., 100., 200.).produitDoubleContracte(TensorModule.Tensor(N.ones((3, 3))))
                    - TensorModule.Tensor(N.array([[375.52155772, 200., 200.], [200., 273.99165508, 400.], [200., 400., 329.62447844]])))
        diff = max(N.fabs(TensorModule.flatten(tensDiff.array.tolist())))
        self.assertAlmostEqual(diff, 0., 8)


if __name__ == '__main__':
    unittest.main()
