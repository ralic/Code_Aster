#@ MODIF B_SENSIBILITE_MEMO_NOM_SENSI Build  DATE 06/09/2004   AUTEUR MCOURTOI M.COURTOIS 
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

# -*- coding: iso-8859-1 -*-

# RESPONSABLE GNICOLAS G.NICOLAS
"""
Classe de mémorisation des noms liés à la sensibilité
"""

class SENSIBILITE_MEMO_NOM_SENSI :
   """
   Attributs :
   l_param_sensi = liste des paramètres sensibles concernés.
     Un paramètre n'est présent qu'une seule fois dans la liste.
   d_noms_composes = dictionnaire des noms composés.
     La clé est le tuple (nom_simple,paramètre sensible).
     La valeur est le nom composé correspondant.
     On ne peut enregistrer un nom composé que si le paramètre sensible est connu.
   d_param_commande = dictionnaire des commandes à dériver ensemble.
     La clé est un paramètre sensibles.
     La valeur est la liste des commandes à dériver meme si le paramètre sensible ne
     fait pas partie des données de la commande.
   """
#
# ---------- Début du constructeur ----------
#
   def __init__(self,l_param_sensi=[]) :
       self.l_param_sensi=l_param_sensi
       self.d_noms_composes={}
       self.d_param_commande={}
#
# ---------- Fin du constructeur ----------
#
#  1. Les paramètres sensibles
#
   def add_param_sensi(self,param_sensi) :
       """
       Ajoute un paramètre sensible à la liste
       Code de retour : 0, tout va bien
                        1, le paramètre sensible est déjà dans la liste
       """
       if param_sensi in self.l_param_sensi :
         print "Le paramètre sensible ", param_sensi, " est déjà dans la liste."
         codret = 1
       else :
         codret = 0
         self.l_param_sensi.append(param_sensi)
       return codret
#
   def get_l_param_sensi(self) :
       """
       Récupère la liste des paramètres sensibles
       """
       return self.l_param_sensi
#
#  2. Les noms composés
#
   def get_d_noms_composes(self) :
       """
       Récupère le dictionnaire des noms composes
       """
       return self.d_noms_composes
# 
   def add_nom_compose(self,nom_simple,param_sensi,nom_compose) :
       """
       Ajoute un nom composé dans la structure de mémorisation.
       'nom_compose' est le nom associé à la dérivation de 'nom_simple' par rapport
       à 'param_sensi'.
       Code de retour : 0, tout va bien
                        1, le paramètre sensible est inconnu dans la liste
                        2, un nom composé existe déjà
       """
###       print ">>>> dans add_nom_compose, nom_simple  = ", nom_simple
###       print ">>>> dans add_nom_compose, param_sensi = ", param_sensi
###       print ">>>> dans add_nom_compose, l_param_sensi = ", self.l_param_sensi
       if self.d_noms_composes.has_key((nom_simple,param_sensi)) :
         print "Un nom composé existe déjà pour ",nom_simple," et ",param_sensi,"."
         codret = 2
       elif param_sensi in self.l_param_sensi :
         codret = 0
         self.d_noms_composes[(nom_simple,param_sensi)] = nom_compose
       else :
         print "Le paramètre sensible ", param_sensi, " est inconnu dans la liste."
         codret = 1
       return codret
#
   def get_nom_compose(self,nom_simple,param_sensi,message=None) :
       """
       Pour un un nom simple et un paramètre sensible donnés :
        1. Code de retour : 0, tout va bien
                            1, le paramètre sensible est inconnu dans la liste
                            2, aucun nom composé n'a été défini
        2. Le nom composé associé
       """
###       print ">>>> dans get_nom_compose, nom_simple  = ", nom_simple
###       print ">>>> dans get_nom_compose, param_sensi = ", param_sensi
       nom_compose= None
       if self.d_noms_composes.has_key((nom_simple,param_sensi)) :
         codret = 0
         nom_compose = self.d_noms_composes[(nom_simple,param_sensi)]
       elif param_sensi not in self.l_param_sensi :
         if ( message ) :
           print "Le paramètre sensible ", param_sensi, " est inconnu dans la liste."
         codret = 1
       else :
         if ( message ) :
           print "Aucun nom composé n'a été défini pour ",nom_simple," et ",param_sensi,"."
         codret = 2
       return codret, nom_compose
#
   def get_d_nom_s_c(self,param_sensi) :
       """
       Pour un paramètre sensible donné :
        1. Code de retour : 0, tout va bien
                            1, le paramètre sensible est inconnu dans la liste
        2. Le dictionnaire des couples (nom simple,nom composé) existant
       """
       if param_sensi in self.l_param_sensi :
         d_nom_s_c = {}
         codret = 0
         for cle,valeur in self.d_noms_composes.items() :
           le_nom_simple,le_param_sensi = cle
           if param_sensi == le_param_sensi :
             d_nom_s_c[le_nom_simple] = valeur
       else :
         d_nom_s_c = None
         print "Le paramètre sensible ", param_sensi, " est inconnu dans la liste."
         codret = 1
       return codret, d_nom_s_c
#
#  3. Les noms simples
#
   def get_l_noms_simples(self,param_sensi) :
       """
       Pour un paramètre sensible donné :
        1. Code de retour : 0, tout va bien
                            1, le paramètre sensible est inconnu dans la liste
        2. La liste des noms simples pour lesquels un nom composé existe
       """
       if param_sensi in self.l_param_sensi :
         l_noms_simples = []
         codret = 0
         for cle in self.d_noms_composes.keys() :
           le_nom_simple,le_param_sensi = cle
           if param_sensi == le_param_sensi :
             l_noms_simples.append(le_nom_simple)
       else :
         l_noms_simples = None
         print "Le paramètre sensible ", param_sensi, " est inconnu dans la liste."
         codret = 1
       return codret, l_noms_simples
#
#  4. Les commandes particulières
#
   def add_commande(self,param_sensi,commande) :
       """
       Ajoute une commande pour un paramètre sensible.
       Code de retour : 0, tout va bien
                        1, le paramètre sensible est inconnu dans la liste
       """
#       print ">>>> dans add_commande, param_sensi = ", param_sensi
#       print ">>>> dans add_commande, commande  = ", commande
       if param_sensi in self.l_param_sensi :
         if self.d_param_commande.has_key(param_sensi) :
           liste = self.d_param_commande[param_sensi]
         else :
           liste = []
         if not commande in liste :
           liste.append(commande)
           self.d_param_commande[param_sensi] = liste
         codret = 0
       else :
         print "Le paramètre sensible ", param_sensi, " est inconnu dans la liste."
         codret = 1
#       print self.d_param_commande
       return codret
#
   def get_l_commandes(self,param_sensi) :
       """
       Pour un paramètre sensible donné :
        1. Code de retour : 0, tout va bien
                            1, le paramètre sensible est inconnu dans la liste
        2. La liste des commandes à dériver
       """
       if param_sensi in self.l_param_sensi :
         if self.d_param_commande.has_key(param_sensi) :
           l_commandes = self.d_param_commande[param_sensi]
         else :
           l_commandes = []
         codret = 0
       else :
         print "Le paramètre sensible ", param_sensi, " est inconnu dans la liste."
         l_commandes = None
         codret = 1
       return codret, l_commandes
#
#
if __name__ == "__main__" :
#
#
  t1 = SENSIBILITE_MEMO_NOM_SENSI()
  print "\n",t1
  print "Liste des paramètres sensibles : ",t1.get_l_param_sensi()
  print "Dictionnaire des noms composés : ", t1.get_d_noms_composes()
  print "Ajout de 'PS3' : ", t1.add_param_sensi('PS3')
  print "Liste des paramètres sensibles : ",t1.get_l_param_sensi()
  print "Ajout de 'PS3' : ", t1.add_param_sensi('PS3')
#
  memo_nom_sensi = SENSIBILITE_MEMO_NOM_SENSI(['PS1','PS2'])
  print "\n",memo_nom_sensi
  print "Liste des paramètres sensibles : ",memo_nom_sensi.get_l_param_sensi()
  print "Dictionnaire des noms composés : ",memo_nom_sensi.get_d_noms_composes()
  print memo_nom_sensi.add_nom_compose('CH0','PS2','CH0_PS2')
  print "Dictionnaire des noms composés : ",memo_nom_sensi.get_d_noms_composes()
  l_param = ['PS1','PS2','PS2','PS3']
  for param in l_param :
    print "Ajout de la composition de 'CH1' par ",param," : ",memo_nom_sensi.add_nom_compose('CH1',param,'CH1_'+param)
  print "Dictionnaire des noms composés : ",memo_nom_sensi.get_d_noms_composes()
  print memo_nom_sensi.get_nom_compose('CH1','PS2')
  print memo_nom_sensi.get_nom_compose('CH1','PS3')
  print memo_nom_sensi.get_nom_compose('CH2','PS2','0')
  print memo_nom_sensi.get_nom_compose('CH2','PS2')
  l_param = ['PS1','PS2','PS3']
  for param in l_param :
    print ". Noms simples associés à ",param," :" , memo_nom_sensi.get_l_noms_simples(param)
    print ". Noms s/c associés à ",param,"     :" , memo_nom_sensi.get_d_nom_s_c(param)

