#@ MODIF reca_interp Macro  DATE 16/10/2007   AUTEUR REZETTE C.REZETTE 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE ASSIRE A.ASSIRE
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

import os, sys, pprint
import Numeric
from externe_mess import UTMESS

try: import Macro
except: pass


#===========================================================================================


# INTERPOLATION, CALCUL DE SENSIBILITE, ETC....

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
        txt  = "Problème lors de l'interpolation du calcul dérivé sur les données expérimentale!"
        txt += "\nValeur à interpoler              :  " + str(x0)
        txt += "\nDomaine couvert par l'experience : [" + str(points[0][0]) + ":" + str(points[n-1][0]) + "]"
        UTMESS('F','MACR_RECAL', txt)

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
      stockage = Numeric.ones(n_exp, Numeric.Float)     # matrice de stockage des erreurs en chaque point
      for j in xrange(n_exp) :
         d = self.DistVertAdimPointLigneBrisee(experience[j], resu_num)
         try:
            stockage[n] = d/experience[j][1]
         except ZeroDivisionError:
            stockage[n] = d

         n = n + 1         # on totalise le nombre de points valables
      err = Numeric.ones(n, Numeric.Float) 

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

#      print "L_erreur=", L_erreur

      # On transforme L_erreur en tab num
      dim=[]
      J=[]
      for i in range(len(L_erreur)):
         dim.append(len(L_erreur[i]))
      dim_totale = Numeric.sum(dim)
      L_J = self.calcul_J(L_erreur)
      a=0
      erreur = Numeric.zeros((dim_totale),Numeric.Float)
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

   def calcul_J(self,L_erreur):
      L_J = []
      for i in range(len(L_erreur)):
         total = 0
         for j in range(len(L_erreur[i])):
            total = total + L_erreur[i][j]**2
         L_J.append(total)
      return L_J


# ------------------------------------------------------------------------------

   def norme_J(self,L_J_init,L_J,unite_resu=None):
      """
         Cette fonction calcul une valeur normée de J
      """
      for i in range(len(L_J)):
         try:
            L_J[i] = L_J[i]/L_J_init[i]
         except ZeroDivisionError:
            message=        'Problème de division par zéro dans la normalisation de la fonctionnelle.\n'
            message=message+'Une des valeurs de la fonctionnelle initiale est nulle ou inférieure à la précision machine : %.2f \n'%L_J_init
            if unite_resu:
               fic=open(os.getcwd()+'/fort.'+str(unite_resu),'a')
               fic.write(message)
               fic.close()
            UTMESS('F', "MACR_RECAL", message)
            return

      J = Numeric.sum(L_J)
      J = J/len(L_J)
      return J


# ------------------------------------------------------------------------------

#   def sensibilite(self,objet,UL,F,L_deriv_sensible,val,para,reponses,pas,unite_resu,LIST_SENSI=[],LIST_DERIV=[],INFO=1):

   def sensibilite(self, CALCUL_ASTER, F, L_deriv_sensible, val, pas):

      # CALCUL_ASTER est l'objet regroupant le calcul de F et des derivées, ainsi que les options
      UL         = CALCUL_ASTER.UL
      para       = CALCUL_ASTER.para
      reponses   = CALCUL_ASTER.reponses
      unite_resu = CALCUL_ASTER.UNITE_RESU
      LIST_SENSI = CALCUL_ASTER.LIST_SENSI
      LIST_DERIV = CALCUL_ASTER.LIST_DERIV
      INFO       = CALCUL_ASTER.INFO



      # Erreur de l'interpolation de F_interp : valeur de F interpolée sur les valeurs experimentales
      F_interp = self.multi_interpole_sensib(F, reponses)  #F_interp est une liste contenant des tab num des reponses interpolés

      # Creation de la liste des matrices de sensibilités
      L_A=[]
      for i in range(len(reponses)):     
         L_A.append(Numeric.zeros((len(self.resu_exp[i]),len(val)),Numeric.Float) )

      for k in range(len(val)): # pour une colone de A (dim = nb parametres)

         # On utilise les differences finies pour calculer la sensibilité
         # --------------------------------------------------------------
         # Dans ce cas, un premier calcul_Aster pour val[k] a deja ete effectué, on effectue un autre calcul_Aster pour val[k]+h

         if para[k] not in LIST_SENSI:

             # Message
             if INFO>=2: UTMESS('I','MACR_RECAL','On utilise les differences finies pour calculer la sensibilite de : %s ' % para[k])

             fic=open(os.getcwd()+'/fort.'+str(unite_resu),'a')
             fic.write('\nCalcul de la sensibilité par differences finies pour : '+para[k])
             fic.close() 

             # Perturbation
             h = val[k]*pas
             val[k] = val[k] + h

             # Calcul_Aster pour la valeur perturbée
             F_perturbe, L_deriv = CALCUL_ASTER.calcul_Aster(val)

             # Erreur de l'interpolation de F_perturb : valeur de F (perturbée) interpolée sur les valeurs experimentales
             F_perturbe_interp =self.multi_interpole_sensib(F_perturbe, reponses)

             # On replace les parametres a leurs valeurs initiales
             val[k] = val[k] - h

             # Calcul de L_A (matrice sensibilité des erreurs sur F interpolée)
             for j in range(len(reponses)):
                for i in range(len(self.resu_exp[j])):
                   try:
                      L_A[j][i,k] = -1*(F_interp[j][i] - F_perturbe_interp[j][i])/h
                   except ZeroDivisionError:
                      fic=open(os.getcwd()+'/fort.'+str(unite_resu),'a')
                      fic.write('\n Probleme de division par zéro dans le calcul de la matrice de sensiblité')
                      fic.write('\n Le parametre '+para[k]+'est nul ou plus petit que la précision machine')
                      fic.close() 
                      UTMESS('F','MACR_RECAL',"Probleme de division par zéro dans le calcul de la matrice de sensiblité.\n Le parametre "+para[k]+"est nul ou plus petit que la précision machine")
                      return


         # On utilise le calcul de SENSIBILITE
         # --------------------------------------------------------------
         # Dans ce cas, L_deriv_sensible a deja ete calculé pour le premier calcul pour val[k], aucun autre calcul_F n'est a lancer
         else:
             if INFO>=2: UTMESS('I','MACR_RECAL','On utilise le calcul de SENSIBILITE pour : %s ' % para[k])

             # Message
             fic=open(os.getcwd()+'/fort.'+str(unite_resu),'a')
             fic.write('\nCalcul de la sensibilité par la SENSIBILITE pour : '+para[k])
             fic.close() 

             L_deriv_sensible_interp = L_deriv_sensible

             # Calcul de L_A (matrice sensibilité des erreurs sur F interpolée)
             for j in range(len(reponses)):
                for i in range(len(self.resu_exp[j])):

                   # On interpole la fonction derivée aux points experimentaux
                   val_derivee_interpolee = self.InterpolationLineaire( self.resu_exp[j][i][0], L_deriv_sensible_interp[ para[k] ][:][j] )

                   # Application du poids de la reponse courante j
                   val_derivee_interpolee = val_derivee_interpolee*self.poids[j]

                   try:
                     L_A[j][i,k] =  -1.* ( val_derivee_interpolee ) / self.resu_exp[j][i][1]
                   except ZeroDivisionError:
                     L_A[j][i,k] =  -1.* ( val_derivee_interpolee )

         # fin
         # --------------------------------------------------------------

      # On construit la matrice de sensiblité sous forme d'un tab num
      dim =[]
      for i in range(len(L_A)):
         dim.append(len(L_A[i]))
      dim_totale = Numeric.sum(dim)
      a=0
      A = Numeric.zeros((dim_totale,len(val)),Numeric.Float)
      for n in range(len(L_A)):
         for k in range(len(val)):
            for i in range(dim[n]):
               A[i+a][k] = L_A[n][i,k]
         a=dim[n]

      del(L_A) # On ecrase tout ce qu'il y a dans L_A puisqu'on n'en a plus besoin   

      return A
