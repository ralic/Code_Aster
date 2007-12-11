#@ MODIF V_A_CLASSER Validation  DATE 14/09/2004   AUTEUR MCOURTOI M.COURTOIS 
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
#                                                                       
#                                                                       
# ======================================================================



import types

class A_CLASSER:
   """
      La règle A_CLASSER vérifie que ...

   """
   def __init__(self,*args):
      if len(args) > 2 :
        print "Erreur à la création de la règle A_CLASSER(",args,")"
        return
      self.args=args
      if type(args[0]) == types.TupleType:
        self.args0 = args[0]
      elif type(args[0]) == types.StringType:
        self.args0 = (args[0],)
      else :
        print "Le premier argument de :",args," doit etre un tuple ou une string"
      if type(args[1]) == types.TupleType:
        self.args1 = args[1]
      elif type(args[1]) == types.StringType:
        self.args1 = (args[1],)
      else :
        print "Le deuxième argument de :",args," doit etre un tuple ou une string"
      # création de la liste des mcs
      liste = []
      for arg0 in self.args0:
        liste.append(arg0)
      for arg1 in self.args1:
        liste.append(arg1)
      self.mcs = liste
      self.init_couples_permis()

   def init_couples_permis(self):
      """ Crée la liste des couples permis parmi les self.args, càd pour chaque élément
          de self.args0 crée tous les couples possibles avec un élément de self.args1"""
      liste = []
      for arg0 in self.args0:
        for arg1 in self.args1:
          liste.append((arg0,arg1))
      self.liste_couples = liste

   def verif(self,args):
      """

          args peut etre un dictionnaire ou une liste. Les éléments de args
          sont soit les éléments de la liste soit les clés du dictionnaire.
      """
      # création de la liste des couples présents dans le fichier de commandes
      l_couples = []
      couple = []
      text = ''
      test = 1
      for nom in args:
        if nom in self.mcs :
          couple.append(nom)
          if len(couple) == 2 :
            l_couples.append(tuple(couple))
            couple=[]
            if nom not in self.args1:
              couple.append(nom)
      if len(couple) > 0 :
        l_couples.append(tuple(couple))
      # l_couples peut etre vide si l'on n'a pas réussi à trouver au moins un
      # élément de self.mcs
      if len(l_couples) == 0 :
        message = "- Il faut qu'au moins un objet de la liste : "+`self.args0`+\
                  " soit suivi d'au moins un objet de la liste : "+`self.args1`
        return message,0
      # A ce stade, on a trouvé des couples : il faut vérifier qu'ils sont
      # tous licites
      num = 0
      for couple in l_couples :
        num = num+1
        if len(couple) == 1 :
          # on a un 'faux' couple
          if couple[0] not in self.args1:
            text = text+"- L'objet : "+couple[0]+" doit etre suivi d'un objet de la liste : "+\
                   `self.args1`+'\n'
            test = 0
          else :
            if num > 1 :
              # ce n'est pas le seul couple --> licite
              break
            else :
              text = text+"- L'objet : "+couple[0]+" doit etre précédé d'un objet de la liste : "+\
                   `self.args0`+'\n'
              test = 0
        elif couple not in self.liste_couples :
          text = text+"- L'objet : "+couple[0]+" ne peut etre suivi de : "+couple[1]+'\n'
          test = 0
      return text,test

