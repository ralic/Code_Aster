#@ MODIF extract Utilitai  DATE 16/06/2004   AUTEUR DURAND C.DURAND 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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

#==================================================
# fonction "EXTRACT"
# usage : renvoie sous forme de NumArray le 
# résultat d'une extraction dans une table ASTER
# méthode utilisée en particulier par MACR_RECAL
#==================================================

import Numeric

def EXTRACT(Table,Para,Champ):
# Definition des variables
  __Result = [[None]*2]
  __nb_temp = 0
# Boucle de lecture sur les temps
  while 1: 
#   Si on n a pas lu tous les temps
    try:
#     alors on lit les 2 champs abscisse et ordonnee
        __Result[__nb_temp][0] = Table[Para,__nb_temp+1]
        __Result[__nb_temp][1] = Table[Champ,__nb_temp+1]
        __nb_temp = __nb_temp + 1  
#     on ajoute une ligne supplementaire a __Result
        __Result.append([None]*2)
#   Si on a lu tous les temps alors on sort de la boucle
    except KeyError:
      break
# on renvoie le resultat en fin
  __Rep = __Result[0:__nb_temp]
  F=Numeric.zeros((len(__Rep),2),Numeric.Float)   #on transforme __Rep en array Numeric
  for i in range(len(__Rep)):
   for j in range(2) :
      F[i][j] = __Rep[i][j]
  del(__Rep)
  del(__Result)
  return F
