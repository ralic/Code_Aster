#@ MODIF stats Utilitai  DATE 25/02/2008   AUTEUR ZENTNER I.ZENTNER 

#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
# Author: Michael Bommarito 
#
from math import pi,sqrt,log,exp
# CDF
# Based on Chokri Dridi's algorithm
# http://econwpa.wustl.edu:8089/eps/comp/papers/0212/0212001.pdf
# normcdf(x, m, s)
# x: float
# m: float, mean
# float, standard deviation
def normcdf(x, m, s):
   if x >= 0.:
      return (1.+glquad(0, x/sqrt(2.)))/2.
   else:
      return (1.-glquad(0, -x/sqrt(2.)))/2.

# Composite fifth-order Gauss-Legendre quadrature estimation
def glquad(a, b):
   y = [0, 0, 0, 0, 0]
   x = [-sqrt(245.+14.*sqrt(70.))/21, -sqrt(245.-14.*sqrt(70.))/21, 0, sqrt(245.-14.*sqrt(70.))/21., sqrt(245.+14.*sqrt(70.))/21]
   w = [(322.-13.*sqrt(70.))/900., (322.+13.*sqrt(70.))/900., 128./225., (322.+13.*sqrt(70.))/900, (322.-13.*sqrt(70.))/900.]
   n = 4800
   s = 0
   h = (b-a)/n

   for i in range(0, n, 1):
      y[0] = h * x[0]/2.+(h+2.*(a+i*h))/2.
      y[1] = h * x[1]/2.+(h+2.*(a+i*h))/2.
      y[2] = h * x[2]/2.+(h+2.*(a+i*h))/2.
      y[3] = h * x[3]/2.+(h+2.*(a+i*h))/2.
      y[4] = h * x[4]/2.+(h+2.*(a+i*h))/2.
      f = lambda r: (2./sqrt(pi))*exp(-r**2.)
      s = s+h*(w[0]*f(y[0])+w[1]*f(y[1])+w[2]*f(y[2])+w[3]*f(y[3])+w[4]*f(y[4]))/2.
   return s

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
