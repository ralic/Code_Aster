#@ MODIF B_SENSIBILITE_COMMANDES_SENSIBLES Build  DATE 23/01/2006   AUTEUR NICOLAS O.NICOLAS 
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


# RESPONSABLE GNICOLAS G.NICOLAS
"""
Classe des commandes particulières pour la sensibilité
"""
#
class SENSIBILITE_COMMANDES_SENSIBLES :
   """
   Attributs :
   . Un dictionnaire contenant les listes de commandes suivantes :
     l_commandes_defi_para_sensi = liste des commandes de définition des paramètres sensibles
     l_commandes_sensibles = liste des commandes qui pilotent la sensibilité
                             ce sont les commandes principales de calcul thermique ou mécanique
     l_commandes_a_deriver = liste des commandes à dériver quand un des arguments est dérivé
     l_commandes_a_deriver_ensemble = liste des commandes à dériver dès que l'une est concernée
                                      par un paramètre sensible
     l_commandes_poursuite = liste des commandes activant une reprise de calcul
     Une commande n'est présente qu'une seule fois dans chaque liste.
   . Le mot clé pilotant le calcul de sensibilité. C'est sa présence dans les commandes
     "sensibles" qui caractérisera un calcul avec sensibilité.
   . La commande mémorisant les noms pour la sensibilité.
   . La liste des classe de paramètres sensibles définis dans le jdc.

   """
   defi_para_sensi = "defi_para_sensi"
   sensible = "sensible"
   a_deriver = "a_deriver"
   a_deriver_ensemble = "a_deriver_ensemble"    
   sensibles_speciales="sensibles_speciales"
   poursuite = "poursuite"    
#
# ---------- Début du constructeur ----------
#
   def __init__(self,
                l_commandes_defi_para_sensi=None,
                l_commandes_sensibles=None,
                l_commandes_a_deriver=None,
                l_commandes_a_deriver_ensemble=None,
                l_commandes_poursuite=None,
                d_commandes_sensibles_speciales=None,
                mot_cle = 'SENSIBILITE',
                memo_nom_sensi = 'MEMO_NOM_SENSI',
                DEBUG=None
                ) :
       l_commandes_defi_para_sensi_defaut=['DEFI_PARA_SENSI']
       l_commandes_sensibles_defaut=['THER_LINEAIRE',
                                     'THER_NON_LINE',
                                     'MECA_STATIQUE',
                                     'STAT_NON_LINE',
                                     'DYNA_LINE_HARM',
                                     'DYNA_LINE_TRAN',
                                     'MODE_ITER_SIMULT',
                                     'MODE_ITER_INV',
                                     'NORM_MODE',
                                     'EXTR_RESU',
                                     'PROJ_CHAMP',
                                     'CALC_G_THETA_T',
                                     'RECU_FONCTION',
                                     'CREA_TABLE',
                                     'POST_RELEVE_T']
       l_commandes_a_deriver_defaut=['AFFE_MATERIAU',
                                     'AFFE_CHAR_MECA_F',
                                     'AFFE_CHAR_THER_F',
                                     'CREA_CHAMP',
                                     'CALC_MATR_ELEM' ]
       l_commandes_a_deriver_ensemble_defaut=['DEFI_MATERIAU']
       l_commandes_poursuite_defaut=['POURSUITE']
       d_commandes_sensibles_speciales_defaut={}
       d={}
       d["OPERATION"] = ["AFFE"]
       d_commandes_sensibles_speciales_defaut["CREA_CHAMP"]=d
       DEBUG_defaut = 0
       self.dict_commandes = {}
       self.dict_commandes[self.defi_para_sensi]     = l_commandes_defi_para_sensi or l_commandes_defi_para_sensi_defaut
       self.dict_commandes[self.sensible]            = l_commandes_sensibles or l_commandes_sensibles_defaut
       self.dict_commandes[self.a_deriver]           = l_commandes_a_deriver or l_commandes_a_deriver_defaut
       self.dict_commandes[self.a_deriver_ensemble]  = l_commandes_a_deriver_ensemble or l_commandes_a_deriver_ensemble_defaut
       self.dict_commandes[self.poursuite]           = l_commandes_poursuite or l_commandes_poursuite_defaut
       self.dict_commandes[self.sensibles_speciales] = d_commandes_sensibles_speciales or d_commandes_sensibles_speciales_defaut
       self.DEBUG = DEBUG or DEBUG_defaut
       self.mot_cle = mot_cle
       self.memo_nom_sensi = memo_nom_sensi
       self.classe_para_sensi = []
# ---------- Fin du constructeur ----------
#
   def add_classe_para_sensi(self,classe) :
       """
       Ajoute une classe de paramètre sensible à la liste
       Code de retour :  0, tout va bien
                         1, la classe est déjà dans la liste
       """
       if classe in self.classe_para_sensi :
         codret = 1
       else :
         codret = 0
         self.classe_para_sensi.append(classe)
       return codret
#
   def get_classe_para_sensi(self) :
       """
       Récupère les classes des paramètres sensibles
       """
       return self.classe_para_sensi
#
   def add_commande(self,commande,type_commande) :
       """
       Ajoute une commande à la liste du type type_commande
       Code de retour :  0, tout va bien
                         1, la commande est déjà dans la liste
       """
       liste = self.dict_commandes[type_commande]
       if commande in liste :
         codret = 1
       else :
         codret = 0
         liste.append(commande)
       self.dict_commandes[type_commande] = liste
       return codret
#
#
   def add_commande_defi_para_sensi(self,commande) :
       """
       Ajoute une commande de définition des paramètres sensibles à la liste
       Code de retour :  0, tout va bien
                         1, la commande est déjà dans la liste
       """
       return self.add_commande(commande,self.defi_para_sensi)
#
   def get_l_commandes_defi_para_sensi(self) :
       """
       Récupère la liste des commandes de définition des paramètres sensibles
       """
       return self.dict_commandes[self.defi_para_sensi]
#
   def add_commande_sensible(self,commande) :
       """
       Ajoute une commande sensible à la liste
       Code de retour :  0, tout va bien
                         1, la commande est déjà dans la liste
       """
       return self.add_commande(commande,self.sensible)
#
   def get_l_commandes_sensibles(self) :
       """
       Récupère la liste des commandes sensibles
       """
       return self.dict_commandes[self.sensible]
#
   def add_commande_a_deriver(self,commande) :
       """
       Ajoute une commande a dériver à la liste
       Code de retour :  0, tout va bien
                         1, la commande à dériver est déjà dans la liste
       """
       return self.add_commande(commande,self.a_deriver)
# 
   def get_l_commandes_a_deriver(self) :
       """
       Récupère la liste des commandes à dériver
       """
       return self.dict_commandes[self.a_deriver]
#
   def add_commande_a_deriver_ensemble(self,commande) :
       """
       Ajoute une commande a dériver à la liste
       Code de retour :  0, tout va bien
                         1, la commande à dériver est déjà dans la liste
       """
       return self.add_commande(commande,self.a_deriver_ensemble)
# 
   def get_l_commandes_a_deriver_ensemble(self) :
       """
       Récupère la liste des commandes à dériver ensemble
       """
       return self.dict_commandes[self.a_deriver_ensemble]
#
   def add_commande_poursuite(self,commande) :
       """
       Ajoute une commande a dériver à la liste
       Code de retour :  0, tout va bien
                         1, la commande de poursuite est déjà dans la liste
       """
       return self.add_commande(commande,self.poursuite)
# 
   def get_l_commandes_poursuite(self) :
       """
       Récupère la liste des commandes de poursuite
       """
       return self.dict_commandes[self.poursuite]
# 
   def get_d_commandes_sensibles_speciales(self) :
       """
       Récupère le dictionnaire des commandes sensibles speciales
       """
       return self.dict_commandes[self.sensibles_speciales]
#
#
if __name__ == "__main__" :
#
#
  commandes_sensibles = SENSIBILITE_COMMANDES_SENSIBLES()
  print "\n",commandes_sensibles
  print "Commande de mémorisation : ",commandes_sensibles.memo_nom_sensi
  print "Mot clé pilotant le calcul de sensibilité : ",commandes_sensibles.mot_cle
  print "Classe des paramètres sensibles           : ",commandes_sensibles.get_classe_para_sensi()
  print "Liste des commandes de déf des paras. sensibles : ",commandes_sensibles.get_l_commandes_defi_para_sensi()
  print "Liste des commandes sensibles          : ",commandes_sensibles.get_l_commandes_sensibles()
  print "Liste des commandes à dériver          : ",commandes_sensibles.get_l_commandes_a_deriver()
  print "Liste des commandes à dériver ensemble : ",commandes_sensibles.get_l_commandes_a_deriver_ensemble()
  print "Liste des commandes de poursuite       : ",commandes_sensibles.get_l_commandes_poursuite()
  print "Ajout de la commande à dériver 'GABUZOME' : ", commandes_sensibles.add_commande_a_deriver('GABUZOME')
  print "Ajout de la commande à dériver 'GABUZOME' : ", commandes_sensibles.add_commande_a_deriver('GABUZOME')
  print "Liste des commandes à dériver  : ", commandes_sensibles.get_l_commandes_a_deriver()
  print "\n"
  
  
  
