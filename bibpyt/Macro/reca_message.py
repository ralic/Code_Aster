#@ MODIF reca_message Macro  DATE 06/09/2004   AUTEUR MCOURTOI M.COURTOIS 
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

import os,Numeric

#===========================================================================================

# -*- coding: iso-8859-1 -*-

# AFFICHAGE DES MESSAGES

class Message :
   """classe gérant l'affichage des messages concernant le déroulement de l'optmisation """
   #Constructeur de la classe
   def __init__(self,para,val_init,resu_exp,ul_out):
      self.nom_para = para
      self.res_exp = resu_exp
      res=open(os.getcwd()+'/fort.'+str(ul_out),'a')
      res.write(' <INFO>  MACR_RECAL V1.1 \n\n\n')
      res.close()
      
   
   def affiche_result_iter(self,iter,J,val,residu,Act,ul_out):
      res=open(os.getcwd()+'/fort.'+str(ul_out),'a')
      res.write('\n=======================================================\n')
      res.write('Iteration '+str(iter)+' :\n')
      res.write('\n=> Fonctionnelle = '+str(J))
      res.write('\n=> Résidu        = '+str(residu))
      res.write('\n=> Paramètres    = ')
      for i in range(len(val)):
         res.write('\n         '+ self.nom_para[i]+' = '+str(val[i]) )
      if (len(Act)!=0):
         if (len(Act)==1):
            res.write('\n\n Le paramètre ')
         else:
            res.write('\n\n Les paramètres ')
         for i in Act:
            res.write(self.nom_para[i]+' ')
         if (len(Act)==1):
            res.write('\n est en butée sur un bord de leur domaine admissible.')
         else:
            res.write('\n sont en butée sur un bord de leur domaine admissible.')
      res.write('\n=======================================================\n\n')
      res.close()
   
   def affiche_etat_final_convergence(self,iter,max_iter,prec,residu,Act,ul_out):
      res=open(os.getcwd()+'/fort.'+str(ul_out),'a')
      if ((iter < max_iter) or (residu < prec)):
        res.write('\n=======================================================\n') 
        res.write('                   CONVERGENCE ATTEINTE                ')
        if (len(Act)!=0):
           res.write("\n\n         ATTENTION : L'OPTIMUM EST ATTEINT AVEC      ")
           res.write("\n           DES PARAMETRES EN BUTEE SUR LE BORD     ")
           res.write("\n               DU DOMAINE ADMISSIBLE                 ")
        res.write('\n=======================================================\n') 
        res.close()
      else:
        res.write("\n=======================================================\n")
        res.write('               CONVERGENCE  NON ATTEINTE              ')
        res.write("\n  Le nombre maximal  d'itération ("+str(max_iter)+") a été dépassé")                    
        res.write('\n=======================================================\n')
        res.close()

   def affiche_calcul_etat_final(self,para,Hessien,valeurs_propres,vecteurs_propres,sensible,insensible,ul_out):
        res=open(os.getcwd()+'/fort.'+str(ul_out),'a')
        res.write('\n\nValeurs propres du Hessien:\n')
        res.write(str( valeurs_propres))
        res.write('\n\nVecteurs propres associés:\n')
        res.write(str( vecteurs_propres))
        res.write('\n\n              --------')
        res.write('\n\nOn peut en déduire que :')
        # Paramètres sensibles
        if (len(sensible)!=0):
           res.write('\n\nLes combinaisons suivantes de paramètres sont prépondérantes pour votre calcul :\n')
           k=0
           for i in sensible:
              k=k+1
              colonne=vecteurs_propres[:,i]
              numero=Numeric.nonzero(Numeric.greater(abs(colonne/max(abs(colonne))),1.E-1))
              res.write('\n   '+str(k)+') ')
              for j in numero:
                 res.write('%+3.1E ' %colonne[j]+'* '+para[j]+' ')
              res.write('\n      associée à la valeur propre %3.1E \n' %valeurs_propres[i])
        # Paramètres insensibles
        if (len(insensible)!=0):
           res.write('\n\nLes combinaisons suivantes de paramètres sont insensibles pour votre calcul :\n')
           k=0
           for i in insensible:
              k=k+1
              colonne=vecteurs_propres[:,i]
              numero=Numeric.nonzero(Numeric.greater(abs(colonne/max(abs(colonne))),1.E-1))
              res.write('\n   '+str(k)+') ')
              for j in numero:
                 res.write('%+3.1E ' %colonne[j]+'* '+para[j]+' ')
              res.write('\n      associée à la valeur propre %3.1E \n' %valeurs_propres[i])
        res.close()
      
   

