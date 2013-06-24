# coding=utf-8
# person_in_charge: aimery.assire at edf.fr
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

import os
import numpy as NP

from Utilitai.Utmess import UTMESS


#===========================================================================================

# INTERPOLATION, ETC....

#--------------------------------------
class Sim_exp :

   def __init__ (self,result_exp,poids) :
      self.resu_exp = result_exp
      self.poids = poids

   # ------------------------------------------------------------------------------
   def InterpolationLineaire (self, x0, points) :
      """
          Interpolation Lineaire de x0 sur la fonction discrétisée yi=points(xi) i=1,..,n
      """
      # x0     = Une abscisse        (1 colonne, 1 ligne)
      # points = Tableau de n points (2 colonnes, n lignes)
      # on suppose qu'il existe au moins 2 points, 
      # et que les points sont classés selon les abscisses croissantes

      n = len(points)
      if ( x0 < points[0][0] ) or ( x0 > points[n-1][0] ) :
        UTMESS('F','RECAL0_48', valk=(str(x0), str(points[0][0]), str(points[n-1][0])))

      i = 1
      while x0 > points[i][0]:
         i = i+1

      y0 = (x0-points[i-1][0]) * (points[i][1]-points[i-1][1]) / (points[i][0]-points[i-1][0]) + points[i-1][1]

      return y0




   # ------------------------------------------------------------------------------
   def DistVertAdimPointLigneBrisee (self, M, points) :
      """
          Distance verticale d'un point M à une ligne brisée composée de n points
      """
      # M      = Point               (2 colonnes, 1 ligne)
      # points = Tableau de n points (2 colonnes, n lignes)
      # on suppose qu'il existe au moins 2 points, 
      # et que les points sont classés selon les abscisses croissantes
      n = len(points)
      if ( M[0] < points[0][0] ) or ( M[0] > points[n-1][0] ):
        return 0.
      i = 1
      while M[0] > points[i][0]:
         i = i+1
      y_proj_vert = (M[0]-points[i-1][0]) * (points[i][1]-points[i-1][1]) / (points[i][0]-points[i-1][0]) + points[i-1][1]  
      d = (M[1] - y_proj_vert)
           # Attention: la distance n'est pas normalisée
           # Attention: problème si points[0][0] = points[1][0] = M[0]
           # Attention: problème si M[1] = 0
      return d


   # ------------------------------------------------------------------------------
   def _Interpole(self, F_calc,experience,poids) :   #ici on passe en argument "une" experience
      """
         La Fonction Interpole interpole une et une seule F_calc sur F_exp et renvoie l'erreur seulement
      """
      n = 0
      resu_num = F_calc
      n_exp = len(experience)    # nombre de points sur la courbe expérimentale num.i    
      stockage = NP.ones(n_exp)     # matrice de stockage des erreurs en chaque point
      for j in xrange(n_exp) :
         d = self.DistVertAdimPointLigneBrisee(experience[j], resu_num)
         if NP.all(experience[j][1] != 0.):
            stockage[n] = d/experience[j][1]
         else:
            stockage[n] = d

         n = n + 1         # on totalise le nombre de points valables
      err = NP.ones(n, dtype=float) 

      for i in xrange(n) :
          err[i] = poids*stockage[i]
      return  err


   # ------------------------------------------------------------------------------
   def multi_interpole(self, L_F, reponses):
      """
         Cette fonction appelle la fonction interpole et retourne les sous-fonctionnelles J et l'erreur.
         On interpole toutes les reponses une à une en appelant la methode interpole.
      """

      L_erreur=[]
      for i in range(len(reponses)):
         err = self._Interpole(L_F[i],self.resu_exp[i],self.poids[i])
         L_erreur.append(err)

      # On transforme L_erreur en tab num
      dim=[]
      J=[]
      for i in range(len(L_erreur)):
         dim.append(len(L_erreur[i]))
      dim_totale = NP.sum(dim)
      L_J = self.calcul_J(L_erreur)
      a=0
      erreur = NP.zeros((dim_totale))
      for n in range(len(L_erreur)):
         for i in range(dim[n]):
            erreur[i+a] = L_erreur[n][i]
         a = dim[n]
      del(L_erreur) #on vide la liste puisqu'on n'en a plus besoin

      return L_J,erreur


   # ------------------------------------------------------------------------------
   def multi_interpole_sensib(self, L_F, reponses):    
      """
         Cette fonction retourne seulement l'erreur, elle est appelée dans la methode sensibilité.
         On interpole toutes les reponses une à une en appelant la methode interpole.
      """

      L_erreur=[]
      for i in range(len(reponses)):   
         err = self._Interpole(L_F[i], self.resu_exp[i], self.poids[i])
         L_erreur.append(err)
      # On transforme L_erreur en tab num
      return L_erreur
       

   # ------------------------------------------------------------------------------
   def calcul_J(self, L_erreur):
      L_J = []
      for i in range(len(L_erreur)):
         total = 0
         for j in range(len(L_erreur[i])):
            total = total + L_erreur[i][j]**2
         L_J.append(total)
      return L_J


   # ------------------------------------------------------------------------------
   def norme_J(self, L_J_init, L_J, unite_resu=None):
      """
         Cette fonction calcul une valeur normée de J
      """
      for i in range(len(L_J)):
         if NP.all(L_J_init[i] != 0.):
            L_J[i] = L_J[i]/L_J_init[i]
         else:
            if unite_resu:
               fic=open(os.getcwd()+'/fort.'+str(unite_resu),'a')
               fic.write(message)
               fic.close()
            UTMESS('F', "RECAL0_44", valr=L_J_init)
            return

      J = NP.sum(L_J)
      J = J/len(L_J)
      return J
