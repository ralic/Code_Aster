# coding=utf-8

# ======================================================================
# COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
# extract from pystat module
#
from math import pi,sqrt,log,exp,pow
import numpy
# CDF
#
# -------------------------------------------------------------------
def normcdf(X):
    # Cumulative normal distribution

    (a1,a2,a3,a4,a5) = (0.31938153, -0.356563782, 1.781477937, -1.821255978, 1.330274429)
    L = numpy.absolute(X)
    K = 1.0 / (1.0 + 0.2316419 * L)
    w = 1.0 - 1.0 / sqrt(2*pi)*exp(-L*L/2.) * (a1*K + a2*K*K + a3*pow(K,3) +
    a4*pow(K,4) + a5*pow(K,5))
    if X<0:
        w = 1.0-w
    return w

# Inverse CDF
def normicdf(v):
   if v > 0.5:
      r = -1.
   else:
      r = 1.
   xp = 0.
   lim = 1.e-20
   p = [-0.322232431088, -1.0, -0.342242088547, -0.0204231210245, -0.453642210148e-4]
   q = [0.0993484626060, 0.588581570495, 0.531103462366, 0.103537752850, 0.38560700634e-2]

   if v < lim or v == 1:
      return -1./lim
   elif v == 0.5:
      return 0
   elif v > 0.5:
      v = 1.-v
   y = sqrt(log(1./v**2.))
   xp = y+((((y*p[4]+p[3])*y+p[2])*y+p[1])*y+p[0])/((((y*q[4]+q[3])*y+q[2])*y+q[1])*y+q[0])
   if v < 0.5:
      xp *= -1.
   return xp*r
#--
