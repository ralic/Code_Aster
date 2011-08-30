#@ MODIF N_MCCOMPO Noyau  DATE 30/08/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE COURTOIS M.COURTOIS
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    Ce module contient la classe MCCOMPO qui sert à factoriser les comportements
    des OBJECT composites
"""

from copy import copy
import N_OBJECT

class MCCOMPO(N_OBJECT.OBJECT):
   """
      Classe support d'un OBJECT composite

   """

   def build_mc(self):
      """
          Construit la liste des sous-entites du MCCOMPO
          à partir du dictionnaire des arguments (valeur)
      """
      if CONTEXT.debug : print "MCCOMPO.build_mc ",self.nom
      # Dans la phase de reconstruction args peut contenir des mots-clés
      # qui ne sont pas dans le dictionnaire des entites de definition (self.definition.entites)
      # de l'objet courant (self)
      #  mais qui sont malgré tout des descendants de l'objet courant (petits-fils, ...)
      args = self.valeur
      if args == None : args ={}
      mc_liste=[]

      # On recopie le dictionnaire des arguments pour protéger l'original des delete (del args[k])
      args = args.copy()

      # Phase 1:
      # On construit les sous entites presentes ou obligatoires
      # 1- les entites présentes dans les arguments et dans la définition
      # 2- les entités non présentes dans les arguments, présentes dans la définition avec un défaut
      # Phase 1.1 : on traite d'abord les SIMP pour enregistrer les mots cles globaux
      for k,v in self.definition.entites.items():
        if v.label != 'SIMP':continue
        if args.has_key(k) or v.statut=='o' :
          #
          # Creation par appel de la methode __call__ de la definition de la sous entite k de self
          # si une valeur existe dans args ou est obligatoire (generique si toutes les
          # entites ont l attribut statut )
          #
          objet=v(val=args.get(k,None),nom=k,parent=self)
          mc_liste.append(objet)
          # Si l'objet a une position globale on l'ajoute aux listes correspondantes
          if hasattr(objet.definition,'position'):
            if objet.definition.position == 'global' :
              self.append_mc_global(objet)
            elif objet.definition.position == 'global_jdc' :
              self.append_mc_global_jdc(objet)
        if args.has_key(k):
           del args[k]

      # Phase 1.2 : on traite les autres entites que SIMP
      # (FACT en fait car un BLOC ne peut avoir le meme nom qu'un mot-clef)
      for k,v in self.definition.entites.items():
        if v.label == 'SIMP':continue
        if args.has_key(k) or v.statut=='o' :
          #
          # Creation par appel de la methode __call__ de la definition de la sous entite k de self
          # si une valeur existe dans args ou est obligatoire (generique si toutes les
          # entites ont l attribut statut )
          #
          objet=v(val=args.get(k,None),nom=k,parent=self)
          mc_liste.append(objet)
        if args.has_key(k):
           del args[k]

      # Phase 2:
      # On construit les objets (en général, blocs) conditionnés par les mots-clés précédemment créés.
      # A ce stade, mc_liste ne contient que les fils de l'objet courant
      # args ne contient plus que des mots-clés qui n'ont pas été attribués car ils sont
      #      à attribuer à des blocs du niveau inférieur ou bien sont des mots-clés erronés
      dico_valeurs = self.cree_dict_condition(mc_liste,condition=1)
      for k,v in self.definition.entites.items():
         if v.label != 'BLOC':continue
         # condition and a or b  : Equivalent de l'expression :  condition ? a : b du langage C
         globs= self.jdc and self.jdc.condition_context or {}
         if v.verif_presence(dico_valeurs,globs):
            # Si le bloc existe :
            #        1- on le construit
            #        2- on l'ajoute à mc_liste
            #        3- on récupère les arguments restant
            #        4- on reconstruit le dictionnaire équivalent à mc_liste
            bloc = v(nom=k,val=args,parent=self)
            mc_liste.append(bloc)
            args=bloc.reste_val
            # On ne recalcule pas le contexte car on ne tient pas compte des blocs
            # pour évaluer les conditions de présence des blocs
            #dico_valeurs = self.cree_dict_valeurs(mc_liste)

      # On conserve les arguments superflus dans l'attribut reste_val
      self.reste_val=args
      # On ordonne la liste ainsi créée suivant l'ordre du catalogue
      # (utile seulement pour IHM graphique)
      mc_liste = self.ordonne_liste(mc_liste)
      # on retourne la liste ainsi construite
      return mc_liste

   def ordonne_liste(self,mc_liste):
      """
         Ordonne la liste suivant l'ordre du catalogue.
         Seulement pour IHM graphique
      """
      if self.jdc and self.jdc.cata_ordonne_dico != None :
         liste_noms_mc_ordonnee = self.get_liste_mc_ordonnee_brute(
                       self.get_genealogie(),self.jdc.cata_ordonne_dico)
         return self.ordonne_liste_mc(mc_liste,liste_noms_mc_ordonnee)
      else:
         return mc_liste

   def cree_dict_valeurs(self,liste=[],condition=0):
      """
        Cette méthode crée un contexte (sous la forme d'un dictionnaire)
        à partir des valeurs des mots clés contenus dans l'argument liste.
        L'opération consiste à parcourir la liste (d'OBJECT) et à la
        transformer en un dictionnaire dont les clés sont les noms des
        mots clés et les valeurs dépendent du type d'OBJECT.
        Ce dictionnaire servira de liste d'arguments d'appel pour les
        fonctions sd_prod de commandes et ops de macros ou de contexte
        d'évaluation des conditions de présence de BLOC.

        Si l'argument condition de la méthode vaut 1, on ne
        remonte pas les valeurs des mots clés contenus dans des blocs
        pour eviter les bouclages.

        Cette méthode réalise les opérations suivantes en plus de transformer
        la liste en dictionnaire :

           - ajouter tous les mots-clés non présents avec la valeur None
           - ajouter tous les mots-clés globaux (attribut position = 'global'
             et 'global_jdc')

        L'argument liste est, en général, une mc_liste en cours de
        construction, contenant les mots-clés locaux et les blocs déjà créés.

      """
      dico={}
      for v in liste:
        if v.isBLOC():
           # Si v est un BLOC, on inclut ses items dans le dictionnaire
           # représentatif du contexte. Les blocs sont retournés par get_valeur
           # sous la forme d'un dictionnaire : les mots-clés fils de blocs sont
           # donc remontés au niveau du contexte.
           if not condition:
               dadd = v.get_valeur()
               assert intersection_vide(dico, dadd)
               dico.update(dadd)
        else:
           assert not dico.has_key(v.nom), "deja vu : %s" % v.nom
           dico[v.nom]=v.get_valeur()

      # On rajoute tous les autres mots-clés locaux possibles avec la valeur
      # par défaut ou None
      # Pour les mots-clés facteurs, on ne traite que ceux avec statut défaut ('d')
      # et caché ('c')
      # On n'ajoute aucune information sur les blocs. Ils n'ont pas de défaut seulement
      # une condition.
      #XXX remplacer le not has_key par un dico différent et faire dico2.update(dico)
      #    ce n'est qu'un pb de perf
      for k,v in self.definition.entites.items():
        if not dico.has_key(k):
           if v.label == 'SIMP':
              # Mot clé simple
              dico[k]=v.defaut
           elif v.label == 'FACT' :
              if v.statut in ('c','d') :
                 # Mot clé facteur avec défaut ou caché provisoire
                 dico[k]=v(val=None,nom=k,parent=self)
                 # On demande la suppression des pointeurs arrieres
                 # pour briser les eventuels cycles
                 dico[k].supprime()
              else:
                 dico[k]=None
      # A ce stade on a rajouté tous les mots-clés locaux possibles (fils directs) avec leur
      # valeur par défaut ou la valeur None

      # On rajoute les mots-clés globaux sans écraser les clés existantes
      dico_mc = self.recherche_mc_globaux()
      dico_mc.update(dico)
      dico=dico_mc

      return dico

   def cree_dict_condition(self,liste=[],condition=0):
      """
          Methode pour construire un contexte qui servira dans l'évaluation
          des conditions de présence de blocs. Si une commande a un concept
          produit réutilisé, on ajoute la clé 'reuse'
      """
      dico=self.cree_dict_valeurs(liste,condition=1)
      # On ajoute la cle "reuse" pour les MCCOMPO qui ont un attribut reuse. A destination
      # uniquement des commandes. Ne devrait pas etre dans cette classe mais dans une classe dérivée
      if not dico.has_key('reuse') and hasattr(self,'reuse'):
         dico['reuse']=self.reuse
      return dico

   def recherche_mc_globaux(self):
      """
          Retourne la liste des mots-clés globaux de l'étape à laquelle appartient self
          et des mots-clés globaux du jdc
      """
      etape = self.get_etape()
      if etape :
        dict_mc_globaux_fac = self.recherche_mc_globaux_facultatifs()
        for k,v in etape.mc_globaux.items():
           dict_mc_globaux_fac[k]=v.get_valeur()
        if self.jdc :
           for k,v in self.jdc.mc_globaux.items():
              dict_mc_globaux_fac[k]=v.get_valeur()
        return dict_mc_globaux_fac
      else :
        return {}

   def recherche_mc_globaux_facultatifs(self):
      """
          Cette méthode interroge la définition de self et retourne la liste des mots-clés fils
          directs de self de type 'global'.
          position='global' n'est donc possible (et n'a de sens) qu'au plus haut niveau.
      """
      dico={}
      etape = self.get_etape()
      if not etape : return {}
      for k,v in etape.definition.entites.items():
         if v.label != 'SIMP' : continue
         if v.position != 'global' : continue
         if v.statut == 'o':continue
         obj = v(val=None,nom=k,parent=etape)
         dico[k]=obj.get_valeur()
      return dico

   def supprime(self):
      """
         Méthode qui supprime toutes les références arrières afin que l'objet puisse
         etre correctement détruit par le garbage collector
      """
      N_OBJECT.OBJECT.supprime(self)
      for child in self.mc_liste :
         child.supprime()

   def __getitem__(self,key):
      """
         Cette méthode retourne la valeur d'un sous mot-clé (key)
      """
      return self.get_mocle(key)

   def get_mocle(self,key):
      """
          Retourne la valeur du sous mot-clé key
          Ce sous mot-clé peut exister, avoir une valeur par defaut ou etre
          dans un BLOC fils de self
      """
      # on cherche dans les mots cles presents, le mot cle de nom key
      # s'il est là on retourne sa valeur (méthode get_val)
      for child in self.mc_liste:
        if child.nom == key : return child.get_val()
      #  Si on n a pas trouve de mot cle present on retourne le defaut
      #  eventuel pour les mots cles accessibles dans la definition
      #  a ce niveau
      try:
        d=self.definition.entites[key]
        if d.label == 'SIMP':
          return d.defaut
        elif d.label == 'FACT':
          # il faut construire les objets necessaires pour
          # evaluer les conditions des blocs eventuels (a faire)
          if d.statut == 'o' :return None
          if d.statut != 'c' and d.statut != 'd' :
             return None
          else :
             return d(val=None,nom=key,parent=self)
      except KeyError:
        # le mot cle n est pas defini a ce niveau
        pass
      #  Si on a toujours rien trouve, on cherche dans les blocs presents
      #  On suppose que tous les blocs possibles ont ete crees meme ceux
      #  induits par un mot cle simple absent avec defaut (???)
      for mc in self.mc_liste :
        if not mc.isBLOC() : continue
        try:
          return mc.get_mocle(key)
        except:
          # On n a rien trouve dans ce bloc, on passe au suivant
          pass
      #  On a rien trouve, le mot cle est absent.
      #  On leve une exception
      raise IndexError,"Le mot cle %s n existe pas dans %s" % (key,self)

   def get_child(self,name,restreint = 'non'):
      """
          Retourne le fils de self de nom name ou None s'il n'existe pas
          Si restreint vaut oui : ne regarde que dans la mc_liste
          Si restreint vaut non : regarde aussi dans les entites possibles
          avec defaut (Ce dernier cas n'est utilisé que dans le catalogue)
      """
      for v in self.mc_liste:
        if v.nom == name : return v
      if restreint == 'non' :
        try:
           entite=self.definition.entites[name]
           if entite.label == 'SIMP' or (entite.label == 'FACT' and entite.statut in ( 'c', 'd')):
              return entite(None,name,None)
        except:
           pass

      return None

   def append_mc_global(self,mc):
      """
         Ajoute le mot-clé mc à la liste des mots-clés globaux de l'étape
      """
      etape = self.get_etape()
      if etape :
        nom = mc.nom
        etape.mc_globaux[nom]=mc

   def append_mc_global_jdc(self,mc):
      """
          Ajoute le mot-clé mc à la liste des mots-clés globaux du jdc
      """
      nom = mc.nom
      self.jdc.mc_globaux[nom]=mc

   def copy(self):
    """ Retourne une copie de self """
    objet = self.makeobjet()
    # FR : attention !!! avec makeobjet, objet a le meme parent que self
    # ce qui n'est pas du tout bon dans le cas d'une copie !!!!!!!
    # FR : peut-on passer par là autrement que dans le cas d'une copie ???
    # FR --> je suppose que non
    # XXX CCAR : le pb c'est qu'on vérifie ensuite quel parent avait l'objet
    # Il me semble preferable de changer le parent a la fin quand la copie est acceptee
    objet.valeur = copy(self.valeur)
    objet.val = copy(self.val)
    objet.mc_liste=[]
    for obj in self.mc_liste:
      new_obj = obj.copy()
      new_obj.reparent(objet)
      objet.mc_liste.append(new_obj)
    return objet

   def reparent(self,parent):
     """
         Cette methode sert a reinitialiser la parente de l'objet
     """
     self.parent=parent
     self.jdc=parent.get_jdc_root()
     self.etape=parent.etape
     for mocle in self.mc_liste:
        mocle.reparent(self)

   def get_sd_utilisees(self):
      """
        Retourne la liste des concepts qui sont utilisés à l'intérieur de self
        ( comme valorisation d'un MCS)
      """
      l=[]
      for child in self.mc_liste:
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
      for child in self.mc_liste:
         daux = child.get_sd_mcs_utilisees()
         for cle in daux.keys():
            dico[cle] = dico.get(cle, [])
            dico[cle].extend(daux[cle])
      return dico

   def get_mcs_with_co(self,co):
      """
         Cette methode retourne l'objet MCSIMP fils de self
         qui a le concept co comme valeur.
         En principe, elle ne doit etre utilisee que pour les concepts
         instances de la classe CO
      """
      l=[]
      for child in self.mc_liste:
        l.extend(child.get_mcs_with_co(co))
      return l

   def get_all_co(self):
      """
         Cette methode retourne tous les concepts instances de CO
      """
      l=[]
      for child in self.mc_liste:
        l.extend(child.get_all_co())
      return l


def intersection_vide(dict1, dict2):
    """Verification qu'il n'y a pas de clé commune entre 'dict1' et 'dict2'."""
    sk1 = set(dict1.keys())
    sk2 = set(dict2.keys())
    inter = sk1.intersection(sk2)
    ok = len(inter) == 0
    if not ok:
        print 'ERREUR: Mot(s)-clef(s) vu(s) plusieurs fois :', tuple(inter)
    return ok
