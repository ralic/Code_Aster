#@ MODIF reca_controles Macro  DATE 31/10/2006   AUTEUR ASSIRE A.ASSIRE 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE ASSIRE A.ASSIRE
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

import string, copy, Numeric, types, os, sys, pprint

try:
   from Utilitai.Utmess import UTMESS
except ImportError:
   def UTMESS(code,sprg,texte):
      fmt='\n <%s> <%s> %s\n\n'
      print fmt % (code,sprg,texte)
      if code=='F': sys.exit()


# Nom de la routine
nompro = 'MACR_RECAL'



#_____________________________________________
#
# CONTROLE DES ENTREES UTILISATEUR
#_____________________________________________

# ------------------------------------------------------------------------------

def erreur_de_type(code_erreur,X):
   #code_erreur ==0 --> X est une liste
   #code erreur ==1 --> X est un char
   #code erreur ==2 --> X est un float
   #test est un boolean (test = 0 défaut et 1 si un test if est verifier
   txt=""
   if(code_erreur == 0 ):
      if type(X) is not types.ListType:
         txt="\nCette entrée: " +str(X)+" n'est pas une liste valide"
   if(code_erreur == 1 ):
      if type(X) is not types.StringType:
         txt="\nCette entrée: " +str(X)+" n'est pas une chaine de caractère valide ; Veuillez la ressaisir en lui appliquant le type char de python"
   if(code_erreur == 2 ):
      if type(X) is not types.FloatType:
         txt="\nCette entrée:  " +str(X)+" n'est pas une valeur float valide ; Veuillez la ressaisir en lui appliquant le type float de python"
   return txt
   

# ------------------------------------------------------------------------------
   
def erreur_dimension(PARAMETRES,REPONSES):
#On verifie que la dimension de chaque sous_liste de parametre est 4
#et que la dimension de chaque sous_liste de REPONSES est 3
   txt=""
   for i in range(len(PARAMETRES)):
      if (len(PARAMETRES[i]) != 4):
         txt=txt + "\nLa sous-liste de la variable paramètre numéro " + str(i+1)+" n'est pas de longueur 4"
   for i in range(len(REPONSES)):
      if (len(REPONSES[i]) != 3):
         txt=txt + "\nLa sous-liste de la variable réponse numéro " + str(i+1)+" n'est pas de longueur 3"
   return txt


# ------------------------------------------------------------------------------

def compare__dim_rep__dim_RESU_EXP(REPONSES,RESU_EXP):
   # X et Y sont deux arguments qui doivent avoir la meme dimension
   # pour éviter l'arret du programme
   txt=""
   if( len(REPONSES) != len(RESU_EXP)):
      txt="\nVous avez entré " +str(len(REPONSES))+ " réponses et "+str(len(RESU_EXP))+ " expériences ; On doit avoir autant de réponses que de résultats expérimentaux"
   return txt


# ------------------------------------------------------------------------------

def compare__dim_poids__dim_RESU_EXP(POIDS,RESU_EXP):
   # POIDS et Y sont deux arguments qui doivent avoir la meme dimension
   # pour éviter l'arret du programme
   txt=""
   if( len(POIDS) != len(RESU_EXP)):
      txt="\nVous avez entré " +str(len(POIDS))+ " poids et "+str(len(RESU_EXP))+ " expériences ; On doit avoir autant de poids que de résultats expérimentaux"
   return txt


# ------------------------------------------------------------------------------

def verif_fichier(UL,PARAMETRES,REPONSES):
#On verifie les occurences des noms des PARAMETRES et REPONSES 
#dans le fichier de commande ASTER
   txt=""

   try:
      fichier = open('fort.'+str(UL),'r')
      fic=fichier.read()
   except:
      txt += "\nImpossible d'ouvrir le fichier esclave declare avec l'unite logique " + str(UL)
      return txt
   for i in range(len(PARAMETRES)):
      if((string.find(fic,PARAMETRES[i][0])==-1) or ((string.find(fic,PARAMETRES[i][0]+'=')==-1) and (string.find(fic,PARAMETRES[i][0]+' ')==-1))):
         txt += "\nLe paramètre "+PARAMETRES[i][0]+" que vous avez entré pour la phase d'optimisation n'a pas été trouvé dans votre fichier de commandes ASTER"
   for i in range(len(REPONSES)):
      if((string.find(fic,REPONSES[i][0])==-1) or ((string.find(fic,REPONSES[i][0]+'=')==-1) and (string.find(fic,REPONSES[i][0]+' ')==-1))):
         txt += "\nLa réponse  "+REPONSES[i][0]+" que vous avez entrée pour la phase d'optimisation n'a pas été trouvée dans votre fichier de commandes ASTER"
   return txt


# ------------------------------------------------------------------------------

def verif_valeurs_des_PARAMETRES(PARAMETRES):
#On verifie que pour chaque PARAMETRES de l'optimisation
# les valeurs entrées par l'utilisateur sont telles que :
#              val_inf<val_sup
#              val_init appartient à [borne_inf, borne_sup] 
#              val_init!=0         
#              borne_sup!=0         
#              borne_inf!=0         
   txt=""
   #verification des bornes
   for i in range(len(PARAMETRES)):
      if( PARAMETRES[i][2] >PARAMETRES[i][3]):
         txt=txt + "\nLa borne inférieure "+str(PARAMETRES[i][2])+" de  "+PARAMETRES[i][0]+ "est plus grande que sa borne supérieure"+str(PARAMETRES[i][3])
   #verification de l'encadrement de val_init 
   for i in range(len(PARAMETRES)):
      if( (PARAMETRES[i][1] < PARAMETRES[i][2]) or (PARAMETRES[i][1] > PARAMETRES[i][3])):
         txt=txt + "\nLa valeur initiale "+str(PARAMETRES[i][1])+" de "+PARAMETRES[i][0]+ " n'est pas dans l'intervalle [borne_inf,born_inf]=["+str(PARAMETRES[i][2])+" , "+str(PARAMETRES[i][3])+"]"
   #verification que val_init !=0
   for  i in range(len(PARAMETRES)):
      if (PARAMETRES[i][1] == 0. ):
         txt=txt + "\nProblème de valeurs initiales pour le paramètre "+PARAMETRES[i][0]+" : ne pas donner de valeur initiale nulle mais un ordre de grandeur."
   #verification que borne_sup !=0
   for  i in range(len(PARAMETRES)):
      if (PARAMETRES[i][3] == 0. ):
         txt=txt + "\nProblème de borne supérieure pour le paramètre "+PARAMETRES[i][0]+" : ne pas donner de valeur strictement nulle."
   #verification que borne_inf !=0
   for  i in range(len(PARAMETRES)):
      if (PARAMETRES[i][2] == 0. ):
         txt=txt + "\nProblème de borne inférieure pour le paramètre "+PARAMETRES[i][0]+" : ne pas donner de valeur strictement nulle."
   return txt


# ------------------------------------------------------------------------------

def verif_UNITE(GRAPHIQUE,UNITE_RESU):
   # On vérifie que les unités de résultat et 
   # de graphique sont différentes
   txt=""
   if GRAPHIQUE:
      GRAPHE_UL_OUT=GRAPHIQUE['UNITE']
      if (GRAPHE_UL_OUT==UNITE_RESU):
          txt=txt + "\nLes unités logiques des fichiers de résultats graphiques et de résultats d'optimisation sont les memes."
   return txt


# ------------------------------------------------------------------------------

def gestion(UL,PARAMETRES,REPONSES,RESU_EXP,POIDS,GRAPHIQUE,UNITE_RESU,METHODE):
   #Cette methode va utiliser les methodes de cette classe declarée ci-dessus
   #test  est un boolean: test=0 -> pas d'erreur
   #                      test=1 -> erreur détectée

   texte=""
   #On vérifie d'abord si PARAMETRES, REPONSES, RESU_EXP sont bien des listes au sens python
   #test de PARAMETRES
   texte = texte + erreur_de_type(0,PARAMETRES)
   #test de REPONSES
   texte = texte + erreur_de_type(0,REPONSES)
   #test de RESU_EXP
   texte = texte + erreur_de_type(0,RESU_EXP) 
   
   #On vérifie si chaque sous liste de PARAMETRES, REPONSES,  possède le type adéquat
   #test des sous_listes de PARAMETRES
   for i in range(len(PARAMETRES)):
      texte = texte +  erreur_de_type(0,PARAMETRES[i]) 
   #test des sous_listes de REPONSES
   for i in range(len(REPONSES)):
      texte = texte + erreur_de_type(0,REPONSES[i])

   #On verifie si la dimension de chaque sous-liste de : PARAMETRES, REPONSES
   #il faut que:la dimension d'une sous-liste de PARAMETRES = 4
   #et   que    la dimension d'une sous liste de REPONSES   = 3
   texte = texte + erreur_dimension(PARAMETRES,REPONSES)

   #on verifie que l'on a autant de réponses que de résultats expérimentaux
   texte = texte + compare__dim_rep__dim_RESU_EXP(REPONSES,RESU_EXP)
   #on verifie que l'on a autant de poids que de résultats expérimentaux
   texte = texte + compare__dim_poids__dim_RESU_EXP(POIDS,RESU_EXP)

   #on verifie les types des arguments de chaque sous liste de PARAMETRES et REPONSES
      #verification du type stringet type float des arguments de PARAMETRES
   for i in range(len(PARAMETRES)):
      texte = texte + erreur_de_type(1,PARAMETRES[i][0])
      for k in [1,2,3]:
         texte = texte + erreur_de_type(2,PARAMETRES[i][k])

   #verification du type string pour les arguments  de REPONSES
   for i in range(len(REPONSES)):
      for j in range(len(REPONSES[i])):
         texte = texte + erreur_de_type(1,REPONSES[i][j])
   
   #verification du fichier de commndes ASTER
   if METHODE != 'EXTERNE': # pour celui-ci le fort.UL n'est pas l'esclave... voir comment faire
      texte = texte + verif_fichier(UL,PARAMETRES,REPONSES)

   #verification des valeurs des PARAMETRES entrées par l'utilisteur 
   if METHODE == 'LEVENBERG':
      texte = texte + verif_valeurs_des_PARAMETRES(PARAMETRES)

   #verification des unités logiques renseignées par l'utilisateur
   if METHODE != 'EXTERNE':
      texte = texte + verif_UNITE(GRAPHIQUE,UNITE_RESU)

   return texte
   
