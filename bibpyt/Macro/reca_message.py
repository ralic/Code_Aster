#@ MODIF reca_message Macro  DATE 24/09/2002   AUTEUR PABHHHH N.TARDIEU 
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

#===========================================================================================
# AFFICHAGE DES MESSAGES

class Message :
   """classe gérant l'affichage des messages concernant le déroulement de l'optmisation """
   #Constructeur de la classe
   def __init__(self,para,val_init,resu_exp,ul_out):
      self.nom_para = para
      self.res_exp = resu_exp
      res=open(os.getcwd()+'/fort.'+str(ul_out),'a')
      res.write(' <INFO>  MACR_RECAL V1.0 \012\012\012')
      res.close()
      
   
   def affiche_result_iter(self,iter,J,val,residu,ul_out):
      res=open(os.getcwd()+'/fort.'+str(ul_out),'a')
      res.write('\012=======================================================\012')
      res.write('Iteration '+str(iter)+' :\012')
      res.write('\012=> Fonctionnelle = '+str(J))
      res.write('\012=> Résidu        = '+str(residu))
      res.write('\012=> Paramètres    = ')
      for i in range(len(val)):
         res.write('\012         '+ self.nom_para[i]+' = '+str(val[i]) )
      res.write('\012=======================================================\012\012')
      res.close()
   
   def affiche_etat_final_convergence(self,iter,max_iter,prec,residu,ul_out):
      res=open(os.getcwd()+'/fort.'+str(ul_out),'a')
      if ((iter < max_iter) or (residu < prec)):
        res.write('\012=======================================================\012') 
        res.write('                   CONVERGENCE ATTEINTE                ')
        res.write('\012=======================================================\012') 
        res.close()
      else:
        res.write("\012=======================================================\012")
        res.write('               CONVERGENCE  NON ATTEINTE              ')
        res.write("\012  Le nombre maximal  d'itération ("+str(max_iter)+") a été dépassé")                    
        res.write('\012=======================================================\012')
        res.close()
   def affiche_calcul_etat_final(self,Hessien,valeurs_propres,vecteurs_propres,ul_out):
        res=open(os.getcwd()+'/fort.'+str(ul_out),'a')
        res.write('\012Hessien adimensionnel de la fonctionnelle cout: \012')
        res.write(str(Hessien))
        res.write('\012\012Valeurs propres du Hessien:\012')
        res.write(str( valeurs_propres))
        res.write('\012\012Vecteurs propres associés:\012')
        res.write(str(vecteurs_propres))
      
   

