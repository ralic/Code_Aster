#@ MODIF reca_algo Macro  DATE 24/09/2002   AUTEUR PABHHHH N.TARDIEU 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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

import Numeric
import copy
import LinearAlgebra 



def calcul_gradient(A,erreur):
   grad = Numeric.dot(Numeric.transpose(A),erreur)
   return grad


#-------------------------------------------
#classe gérant l'adimensionnement et le dimensionnemnt
class Dimension:
   #le constructeur calcul la matrice D et son inverse
   def __init__(self,val_initiales,para):
      self.val_init = val_initiales
      dim =len(self.val_init)
      self.D = Numeric.zeros((dim,dim),Numeric.Float)
      for i in range(dim):
         self.D[i][i] = self.val_init[i]
      self.inv_D=LinearAlgebra.inverse(self.D)
   

   def adim_sensi(self,A):
      for i in range(A.shape[0]):
         for j in range(A.shape[1]):
            A[i,j] = A[i,j] * self.val_init[j]
      return A



   def redim_sensi(self,A):
      for i in range(A.shape[0]):
         for j in range(A.shape[1]):
            A[i,j] = A[i,j] / self.val_init[j]
      return A


   def adim(self,tab):
      tab_adim = Numeric.dot(self.inv_D,copy.copy(tab))
      return tab_adim


   def redim(self,tab_adim):
      tab = Numeric.dot(self.D,tab_adim)
      return tab
   
#------------------------------------------
def cond(matrix):
    e=LinearAlgebra.eigenvalues(matrix)
    size=len(e)
    e=Numeric.sort(e)
    try:
      condi=e[size-1]/e[0]
    except ZeroDivisionError:
      condi=0.0
    return condi

#-----------------------------------------
def norm(matrix):
    e=LinearAlgebra.eigenvalues(matrix)
    size=len(e)
    e=Numeric.sort(e)
    norm=e[size-1]
    return norm

#-----------------------------------------
def lambda_init(matrix):
# Routine qui calcule la valeur initial du parametre
# de regularisation l. On le choisit tel que 
# l = alpha*[plus grande valeur propre de AtA]
# ou alpha = 1.e-3 si AtA est singuliere et 1.e-16 sinon
     condi=cond(matrix)
     id=Numeric.identity(matrix.shape[0])
     if (condi==0.0):
         l=1.e-3*norm(matrix)
     else:
         l=1.e-16*norm(matrix)
     return l

#-----------------------------------------

def Levenberg(val,A,erreur,l):  
   #on resoud un systeme lineaire afin que l'on puisse réactualiser les valeurs des parametres
   dim_val = len(val)
   id = Numeric.identity(dim_val)
   delta = LinearAlgebra.solve_linear_equations(Numeric.matrixmultiply(Numeric.transpose(A),A) +l*id ,-1*Numeric.matrixmultiply(Numeric.transpose(A),erreur))
   return delta

def test_bornes(l,val,d,borne_inf,borne_sup):
   test=1
   for i in range(len(val)):
      if((val[i] + d[i] < borne_inf[i]) or (val[i] + d[i] > borne_sup[i])):
         test = 0
   if (test == 0):
      l= l*2  
   return test,l
   
def actualise_lambda(l,new_J,J):
   if (new_J<J):
      l = l/10.
   else:
      l = l*10.
   return l


def test_convergence(gradient_init,erreur,A):
   gradient = calcul_gradient(A,erreur)
   epsilon = Numeric.dot(gradient,gradient)/Numeric.dot(gradient_init,gradient_init)
   epsilon = epsilon**0.5
   return epsilon


#fonction appellée quand la convergence est atteinte
#on calcule le Hessien et les valeurs propres et vecteurs propre associés à l'inverse du Hessien
# a est la sensibilite
#le hessien=At*A
def calcul_etat_final(A,iter,max_iter,prec,residu,Messg,ul_out):
   if ((iter < max_iter) or (residu < prec)):
      Hessien = Numeric.matrixmultiply(Numeric.transpose(A),A)
      valeurs_propres,vecteurs_propres = LinearAlgebra.eigenvectors(Hessien) 
      Messg.affiche_calcul_etat_final(Hessien,valeurs_propres,vecteurs_propres,ul_out)
