#@ MODIF N_MCLIST Noyau  DATE 16/05/2007   AUTEUR COURTOIS M.COURTOIS 
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


""" 
    Ce module contient la classe MCList qui sert à controler la valeur
    d'une liste de mots-clés facteur par rapport à sa définition portée par un objet
    de type ENTITE
"""

from copy import copy
import UserList
import types

class MCList(UserList.UserList):
   """ Liste semblable a la liste Python
       mais avec quelques methodes en plus
       = liste de MCFACT
   """
   nature = 'MCList'
   def init(self,nom,parent):
      self.definition = None
      self.nom = nom
      self.parent=parent
      if parent :
         self.jdc = self.parent.jdc
         self.niveau = self.parent.niveau
         self.etape = self.parent.etape
      else:
         # Le mot cle a été créé sans parent
         self.jdc = None
         self.niveau = None
         self.etape = None

   def get_valeur(self):
      """
         Retourne la "valeur" d'un objet MCList. Sert à construire
         un contexte d'évaluation pour une expression Python.
         On retourne l'objet lui-meme.
      """
      return self

   def get_val(self):
      """
          Une autre méthode qui retourne une "autre" valeur d'une MCList
          Elle est utilisée par la méthode get_mocle
      """
      return self

   def supprime(self):
      """ 
         Méthode qui supprime toutes les références arrières afin que l'objet puisse
         etre correctement détruit par le garbage collector 
      """
      self.parent = None
      self.etape = None
      self.jdc = None
      self.niveau = None
      for child in self.data :
         child.supprime()

   def get_child(self,name):
      """ 
          Retourne le fils de nom name s'il est contenu dans self
          Par défaut retourne le fils du premier de la liste 
      """
      obj = self.data[0]
      # Phase 1 : on cherche dans les fils directs de obj
      for child in obj.mc_liste :
        if child.nom == name: return child
      # Phase 2 : on cherche dans les blocs de self
      for child in obj.mc_liste:
        if child.isBLOC() :
          resu = child.get_child(name)
          if resu != None : return resu
      # Phase 3 : on cherche dans les entites possibles pour les défauts
      for k,v in obj.definition.entites.items():
        #if k == name: return v.defaut
        if k == name:
          if v.defaut != None : return v(None,k,None)
      # si on passe ici, c'est que l'on demande un fils qui n'est pas possible --> erreur
      #print "Erreur : %s ne peut etre un descendant de %s" %(name,self.nom)
      return None

   def isBLOC(self):
      """
           Indique si l'objet est de type BLOC
      """
      return 0

   def accept(self,visitor):
      """
         Cette methode permet de parcourir l'arborescence des objets
         en utilisant le pattern VISITEUR
      """
      visitor.visitMCList(self)

   def get_sd_utilisees(self):
      """ 
        Retourne la liste des concepts qui sont utilisés à l'intérieur de self
        ( comme valorisation d'un MCS) 
      """
      l=[]
      for child in self.data:
        l.extend(child.get_sd_utilisees())
      return l

   def get_sd_mcs_utilisees(self):
      """ 
          Retourne la ou les SD utilisée par self sous forme d'un dictionnaire :
            - Si aucune sd n'est utilisée, le dictionnaire est vide.
            - Sinon, les clés du dictionnaire sont les mots-clés derrière lesquels on
              trouve des sd ; la valeur est la liste des sd attenante.

              Exemple ::
              
                { 'VALE_F': [ <Cata.cata.para_sensi instance at 0x9419854>,
                              <Cata.cata.para_sensi instance at 0x941a204> ],
                  'MODELE': [<Cata.cata.modele instance at 0x941550c>] }
     """
      dico = {}
      for child in self.data:
        daux = child.get_sd_mcs_utilisees()
        for cle in daux.keys():
          dico[cle] = daux[cle]
      return dico

   def get_mcs_with_co(self,co):
      """
         Cette methode retourne l'objet MCSIMP fils de self
         qui a le concept co comme valeur.
         En principe, elle ne doit etre utilisee que pour les concepts
         instances de la classe CO 
      """
      l=[]
      for child in self.data:
        l.extend(child.get_mcs_with_co(co))
      return l

   def get_all_co(self):
      """
         Cette methode retourne tous les concepts instances de CO
      """
      l=[]
      for child in self.data:
        l.extend(child.get_all_co())
      return l

   def copy(self):
      """
        Réalise la copie d'une MCList
      """
      liste = self.data[0].definition.list_instance()
      # FR -->Il faut spécifier un parent pour la méthode init qui attend 2 arguments ...
      liste.init(self.nom,self.parent)
      for objet in self:
        new_obj = objet.copy()
        # Pour etre coherent avec le constructeur de mots cles facteurs N_FACT.__call__
        # dans lequel le parent de l'element d'une MCList est le parent de la MCList
        new_obj.reparent(self.parent)
        liste.append(new_obj)
      return liste

   def reparent(self,parent):
      """
         Cette methode sert a reinitialiser la parente de l'objet
      """
      self.parent=parent
      self.jdc=parent.jdc
      self.etape=parent.etape
      for mcfact in self.data:
        mcfact.reparent(parent)

   def get_etape(self):
      """
         Retourne l'étape à laquelle appartient self
         Un objet de la catégorie etape doit retourner self pour indiquer que
         l'étape a été trouvée
         XXX double emploi avec self.etape ???
      """
      if self.parent == None: return None
      return self.parent.get_etape()

   def __getitem__(self,key):
      """
         Dans le cas d un mot cle facteur de longueur 1 on simule un scalaire
      """
      if type(key) != types.IntType and len(self) ==1:
         return self.data[0].get_mocle(key)
      else:
         return self.data[key]
   
   def List_F(self):
      """
         Retourne une liste de dictionnaires (eventuellement singleton) qui peut etre
         passe directement derriere un mot-cle facteur (pour les macros).
      """
      dresu = []
      for mcf in self:
         dico = mcf.cree_dict_valeurs(mcf.mc_liste)
         for i in dico.keys():
            if dico[i] == None:
               del dico[i]
         dresu.append(dico)
      return dresu
