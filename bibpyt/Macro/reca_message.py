# -*- coding: iso-8859-1 -*-
# person_in_charge: aimery.assire at edf.fr
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

import os
import numpy as NP
from Utilitai.Utmess import UTMESS, MessageLog
from recal import Affiche_Param
#===========================================================================================


# AFFICHAGE DES MESSAGES

class Message :
   """
       classe gérant l'affichage des messages concernant le déroulement de l'optmisation
   """

   # ------------------------------------------------------------------------------
   def __init__(self,para,val_init,resu_exp,ul_out):
      self.nom_para = para
      self.resu_exp = resu_exp
      self.val_init = val_init
      self.resu_exp = resu_exp
      self.ul_out   = ul_out

      
   # ------------------------------------------------------------------------------
   def get_filename(self):
      return os.getcwd()+'/fort.'+str(self.ul_out)
   
   
   # ------------------------------------------------------------------------------
   def initialise(self):
      """ Initialisation du fichier """
      UTMESS('I','RECAL0_1', cc=self.get_filename())


   # ------------------------------------------------------------------------------
   def affiche_valeurs(self,val):
      """ Affichage de la valeur des parametres """
      txt = Affiche_Param(self.nom_para, val)
      UTMESS('I','RECAL0_32', valk=txt, cc=self.get_filename())


   # ------------------------------------------------------------------------------
   def affiche_fonctionnelle(self,J):
      """ Affichage de la fonctionnelle  """
      UTMESS('I','RECAL0_33', valr=J, cc=self.get_filename())


   # ------------------------------------------------------------------------------
   def affiche_result_iter(self, iter, J, val, residu, Act=[], ecart_para=None, ecart_fonc=None):
      """ Affichage du message recapitulatif de l'iteration
      """
      UTMESS('I','RECAL0_30')
      UTMESS('I','RECAL0_79', cc=self.get_filename())
      UTMESS('I','RECAL0_31', vali=iter, cc=self.get_filename())
      self.affiche_fonctionnelle(J)
      UTMESS('I','RECAL0_34', valr=residu, cc=self.get_filename())
      if ecart_para: UTMESS('I','RECAL0_37', valr=ecart_para, cc=self.get_filename())
      if ecart_fonc: UTMESS('I','RECAL0_38', valr=ecart_fonc, cc=self.get_filename())

      # Affichage des parametres
      self.affiche_valeurs(val)

      # Si les parametres sont en butee
      if (len(Act)!=0):
         lpara = ' '.join([self.nom_para[i] for i in Act])
         if (len(Act)==1):
            UTMESS('I','RECAL0_46', valk=lpara, cc=self.get_filename())
         else:
            UTMESS('I','RECAL0_47', valk=lpara, cc=self.get_filename())

      UTMESS('I','RECAL0_80', cc=self.get_filename())


   # ------------------------------------------------------------------------------
   def affiche_etat_final_convergence(self,iter,max_iter,iter_fonc,max_iter_fonc,prec,residu,Act=[]):
      """ Affichage du message recapitulatif a la fin du processus d'optimisation
      """
      if ((iter < max_iter) and (residu <= prec) and (iter_fonc < max_iter_fonc) ):
         UTMESS('I','RECAL0_56', cc=self.get_filename())
         if (len(Act)!=0):                UTMESS('I','RECAL0_58', cc=self.get_filename())
      else:
         UTMESS('I','RECAL0_57', cc=self.get_filename())
         if (iter >= max_iter):           UTMESS('I','RECAL0_55', cc=self.get_filename())
         if (iter_fonc >= max_iter_fonc): UTMESS('I','RECAL0_54', cc=self.get_filename())

      UTMESS('I','RECAL0_80', cc=self.get_filename())


   # ------------------------------------------------------------------------------
   def affiche_calcul_etat_final(self,para,Hessien,valeurs_propres,vecteurs_propres,sensible,insensible):
      """ Affichage des informations de l'optimisation (valeurs propres, vecteurs propres, etc.)
      """
      UTMESS('I','RECAL0_60', valk=str(valeurs_propres), cc=self.get_filename())
      UTMESS('I','RECAL0_61', valk=str(vecteurs_propres), cc=self.get_filename())
      UTMESS('I','RECAL0_62', cc=self.get_filename())

      if (len(sensible)!=0 or len(insensible)!=0):
         UTMESS('I','RECAL0_63', cc=self.get_filename())

      # Parametres sensibles
      if (len(sensible)!=0):
         UTMESS('I','RECAL0_64', cc=self.get_filename())
         k=0
         for i in sensible:
            k=k+1
            colonne=vecteurs_propres[:,i]
            numero=NP.nonzero(NP.greater(abs(colonne/max(abs(colonne))),1.E-1))[0]
            txt = '\n   '+str(k)+') '
            for j in numero:
               txt += '%+3.1E ' %colonne[j]+'* '+para[j]+' '
            UTMESS('I','RECAL0_65', valk=(txt, str(valeurs_propres[i])), cc=self.get_filename())

      # Parametres insensibles
      if (len(insensible)!=0):
         UTMESS('I','RECAL0_66', cc=self.get_filename())
         k=0
         for i in insensible:
            k=k+1
            colonne=vecteurs_propres[:,i]
            numero=NP.nonzero(NP.greater(abs(colonne/max(abs(colonne))),1.E-1))[0]
            txt = '\n   '+str(k)+') '
            for j in numero:
               txt += '%+3.1E ' %colonne[j]+'* '+para[j]+' '
            UTMESS('I','RECAL0_65', valk=(txt, str(valeurs_propres[i])), cc=self.get_filename())

      if (len(sensible)!=0 or len(insensible)!=0):
         UTMESS('I','RECAL0_62', cc=self.get_filename())
