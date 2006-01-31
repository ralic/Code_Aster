#@ MODIF reca_algo Macro  DATE 31/01/2006   AUTEUR MCOURTOI M.COURTOIS 
# -*- coding: iso-8859-1 -*-
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
from Numeric import take, size
import copy,os
import LinearAlgebra 
from Cata.cata import INFO_EXEC_ASTER
from Cata.cata import DETRUIRE
from Accas import _F
from Utilitai.Utmess     import UTMESS


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
    e1=LinearAlgebra.eigenvalues(matrix)
    e=map(abs,e1)
    size=len(e)
    e=Numeric.sort(e)
    try:
      condi=e[size-1]/e[0]
    except ZeroDivisionError:
      condi=0.0
    return condi,e[size-1],e[0]

#-----------------------------------------
def norm(matrix):
    e=LinearAlgebra.Heigenvalues(matrix)
    size=len(e)
    e=Numeric.sort(e)
    norm=e[size-1]
    return norm

#-----------------------------------------
def lambda_init(matrix):
# Routine qui calcule la valeur initial du parametre
# de regularisation l.
     condi,emax,emin=cond(matrix)
     id=Numeric.identity(matrix.shape[0])
     if (condi==0.0):
         l=1.e-3*norm(matrix)
     elif (condi<=10000):
         l=1.e-16*norm(matrix)
     elif (condi>10000):
         l=abs(10000.*emin-emax)/10001.
     return l

#-----------------------------------------


def temps_CPU(self,restant_old,temps_iter_old):
   # Fonction controlant le temps CPU restant
   CPU=INFO_EXEC_ASTER(LISTE_INFO = ("CPU_RESTANT",))
   TEMPS=CPU['CPU_RESTANT',1]
   DETRUIRE(CONCEPT=_F(NOM='CPU'),INFO=1)
   err=0
   # Indique une execution interactive
   if (TEMPS>1.E+9):
     return 0.,0.,0
   # Indique une execution en batch
   else:
      restant=TEMPS
      # Initialisation
      if (restant_old==0.):
         temps_iter=-1.
      else:
         # Première mesure
         if (temps_iter_old==-1.):
            temps_iter=(restant_old-restant)
         # Mesure courante
         else:
            temps_iter=(temps_iter_old + (restant_old-restant))/2.
         if ((temps_iter>0.96*restant)or(restant<0.)):
            err=1
            UTMESS('F', "MACR_RECAL", 'Arret de MACR_RECAL par manque de temps CPU')
   return restant,temps_iter,err




def Levenberg_bornes(self,val,Dim,val_init,borne_inf,borne_sup,A,erreur,l,ul_out):  
   # on resoud le système par contraintes actives:
   #    Q.dval + s + d =0
   #    soumis à :
   #    borne_inf < dval < borne_sup 
   #            0 <  s
   #            s.(borne_inf - dval)=0
   #            s.(borne_sup - dval)=0
   dim = len(val)
   id = Numeric.identity(dim)
   # Matrice du système
   Q=Numeric.matrixmultiply(Numeric.transpose(A),A) +l*id
   # Second membre du système
   d=Numeric.matrixmultiply(Numeric.transpose(A),erreur)
   # Ens. de liaisons actives
   Act=Numeric.array([])
   k=0
   done=0
   # Increment des parametres 
   dval=Numeric.zeros(dim,Numeric.Float)
   while done <1 :
      k=k+1
      I=Numeric.ones(dim)
      for i in Act:
         I[i]=0
      I=Numeric.nonzero(Numeric.greater(I,0))
      s=Numeric.zeros(dim,Numeric.Float)
      for i in Act:
         # test sur les bornes (on stocke si on est en butée haute ou basse)
         if (val[i]+dval[i]>=borne_sup[i]):
            dval[i]=borne_sup[i]-val[i]
            s[i]=1.
         if (val[i]+dval[i]<=borne_inf[i]):
            dval[i]=borne_inf[i]-val[i]
            s[i]=-1.
      if (len(I)!=0):
          # xi=-Q(I)-1.(d(I)+Q(I,Act).dval(Act))
          t_QI = take(Q, I)
          t_tQI_Act = take(t_QI, Act, 1)
          t_adim_Act = take(Dim.adim(dval), Act)
          if size(t_tQI_Act) > 0 and size(t_adim_Act) > 0:
             smemb = take(d, I) + Numeric.dot(t_tQI_Act, t_adim_Act)
          else:
             smemb = take(d, I)
          xi=-LinearAlgebra.solve_linear_equations(take(t_QI, I, 1), smemb)
          for i in Numeric.arange(len(I)):
             dval[I[i]]=xi[i]*val_init[I[i]]
      if (len(Act)!=0):
         # s(Av)=-d(Act)-Q(Act,:).dval
         sa=-take(d,Act)-Numeric.dot(take(Q,Act),Dim.adim(dval))
         for i in range(len(Act)):
            if (s[Act[i]]==-1.):
               s[Act[i]]=-sa[i]
            else:
               s[Act[i]]=sa[i]
      # Nouvel ens. de liaisons actives
      Act=Numeric.concatenate((Numeric.nonzero(Numeric.greater(dval,borne_sup-val)),Numeric.nonzero(Numeric.less(dval,borne_inf-val)),Numeric.nonzero(Numeric.greater(s,0.))))
      done=(max(val+dval-borne_sup)<=0)&(min(val+dval-borne_inf)>=0)&(min(s)>=0.0)
      # Pour éviter le cyclage
      if (k>50):
         try:
            l=l*2
            Q=Numeric.matrixmultiply(Numeric.transpose(A),A) +l*id
            k=0
         except:
             res=open(os.getcwd()+'/fort.'+str(ul_out),'a')
             res.write('\n\nQ = \n'+Numeric.array2string(Q-l*id,array_output=1,separator=','))
             res.write('\n\nd = '+Numeric.array2string(d,array_output=1,separator=','))
             res.write('\n\nval = '+Numeric.array2string(val,array_output=1,separator=','))
             res.write('\n\nval_ini= '+Numeric.array2string(val_init,array_output=1,separator=','))
             res.write('\n\nborne_inf= '+Numeric.array2string(borne_inf,array_output=1,separator=','))
             res.write('\n\nborne_sup= '+Numeric.array2string(borne_sup,array_output=1,separator=','))
             UTMESS('F', "MACR_RECAL", "Erreur dans l'algorithme de bornes de MACR_RECAL")
             return 
   newval=copy.copy(val+dval)
   return newval,s,l,Act


def actualise_lambda(l,val,new_val,A,erreur,new_J,old_J):
   dim = len(val)
   id = Numeric.identity(dim)
   # Matrice du système
   Q=Numeric.matrixmultiply(Numeric.transpose(A),A) +l*id
   # Second membre du système
   d=Numeric.matrixmultiply(Numeric.transpose(A),erreur)
   old_Q=old_J
   new_Q=old_J+0.5*Numeric.dot(Numeric.transpose(new_val-val),Numeric.dot(Q,new_val-val))+Numeric.dot(Numeric.transpose(new_val-val),d)
   # Ratio de la décroissance réelle et de l'approx. quad.
   try:
      R=(old_J-new_J)/(old_Q-new_Q)
      if (R<0.25):
         l = l*10.
      elif (R>0.75):
         l = l/15.
   except ZeroDivisionError:
      if (old_J>new_J):
         l = l*10.
      else:
         l = l/10.
   return l


def test_convergence(gradient_init,erreur,A,s):
   gradient = calcul_gradient(A,erreur)+s
   epsilon = Numeric.dot(gradient,gradient)/Numeric.dot(gradient_init,gradient_init)
   epsilon = epsilon**0.5
   return epsilon


# fonction appellée quand la convergence est atteinte
# on calcule le Hessien et les valeurs propres et vecteurs 
# propre associés au Hessien
#  A    = sensibilite
#  At*A = hessien
def calcul_etat_final(para,A,iter,max_iter,prec,residu,Messg,ul_out):
   if ((iter < max_iter) or (residu < prec)):
      Hessien = Numeric.matrixmultiply(Numeric.transpose(A),A)
      valeurs_propres,vecteurs_propres = LinearAlgebra.eigenvectors(Hessien) 
      sensible=Numeric.nonzero(Numeric.greater(abs(valeurs_propres/max(abs(valeurs_propres))),1.E-1))
      insensible=Numeric.nonzero(Numeric.less(abs(valeurs_propres/max(abs(valeurs_propres))),1.E-2))
      Messg.affiche_calcul_etat_final(para,Hessien,valeurs_propres,vecteurs_propres,sensible,insensible,ul_out)





