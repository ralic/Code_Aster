#@ MODIF N_FACT Noyau  DATE 27/03/2002   AUTEUR DURAND C.DURAND 
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
""" Ce module contient la classe de definition FACT
    qui permet de spécifier les caractéristiques des mots clés facteurs
"""

import types

import N_ENTITE
import N_MCFACT
import N_MCLIST

class FACT(N_ENTITE.ENTITE):
   """
    Classe pour definir un mot cle facteur

    Cette classe a trois attributs de classe 

    - class_instance qui indique la classe qui devra etre utilisée 
            pour créer l'objet qui servira à controler la conformité d'un 
            mot-clé facteur avec sa définition

    - list_instance

    - label qui indique la nature de l'objet de définition (ici, FACT)
   """
   class_instance = N_MCFACT.MCFACT
   list_instance = N_MCLIST.MCList
   label = 'FACT'

   def __init__(self,fr="",ang="",docu="",regles=(),statut='f',defaut=None,
                     min=0,max=1,**args):
     
      """
          Un mot-clé facteur est caractérisé par les attributs suivants :

          - fr   :

          - ang :

          - statut :

          - defaut :

          - regles

          - min

          - max

          - position

          - docu
      """
      # Initialisation des attributs
      self.fr=fr
      self.ang=ang
      self.docu = docu
      if type(regles)== types.TupleType:
          self.regles=regles
      else:
          self.regles=(regles,)
      self.statut=statut
      self.defaut=defaut
      self.min=min
      self.max=max
      self.entites=args
      self.position=None
      self.affecter_parente()

   def __call__(self,val,nom,parent):
      """
          Construit la structure de donnee pour un mot cle facteur a partir 
          de sa definition (self) de sa valeur (val), de son nom (nom) et de 
          son parent dans l arboresence (parent)
          
          Suivant le type de la valeur on retournera soit un objet de type 
          MCFACT soit une liste de type MCLIST.

          La creation d un mot cle facteur depend de son statut
          Si statut ='o'   il est obligatoire
          Si statut == 'd' il est facultatif mais ses sous mots cles avec 
                           defaut sont visibles
          Si statut == 'f' il est facultatif et ses sous mots avec defaut ne 
                           sont pas visibles
          Si statut == 'c' il est cache ???
          Si defaut != None, on utilise cette valeur pour calculer la valeur 
                             par defaut du mot cle facteur
      """
      if val == None:
        if self.defaut == None:
          val={}
        elif type(self.defaut) == types.TupleType:
          val=self.defaut
              # Est ce utile ? Le défaut pourrait etre uniquement un dict
        elif type(self.defaut) == types.DictType or isinstance(self.defaut,N_MCFACT._F):
          val=self.defaut
        else:
          # On ne devrait jamais passer par la
          print "On ne devrait jamais passer par la"
          return None

      if type(val) == types.TupleType or type(val) == types.ListType :
        # on est en présence d'un MCFACT multiple !
        l=self.list_instance()
        l.init(nom = nom,parent=parent)
        for v in val:
          objet=self.class_instance(nom=nom,definition=self,val=v,parent=parent)
          l.append(objet)
        return l
      else:
        return self.class_instance(nom=nom,definition=self,val=val,parent=parent)


   def verif_cata(self):
      if type(self.min) != types.IntType :
         if self.min != '**':
            self.cr.fatal("L'attribut 'min' doit etre un entier : %s" %`self.min`)
      if type(self.max) != types.IntType :
         if self.max != '**':
            self.cr.fatal("L'attribut 'max' doit etre un entier : %s" %`self.max`)
      if self.min > self.max :
         self.cr.fatal("Nombres d'occurrence min et max invalides : %s %s" %(`self.min`,`self.max`))
      if type(self.fr) != types.StringType :
         self.cr.fatal("L'attribut 'fr' doit etre une chaine de caractères : %s" %`self.fr`)
      if type(self.regles) != types.TupleType :
         self.cr.fatal("L'attribut 'regles' doit etre un tuple : %s" %`self.regles`)
      if self.statut not in ['f','o','c','d'] :
         self.cr.fatal("L'attribut 'statut' doit valoir 'o','f','c' ou 'd' : %s" %`self.statut`)
      if type(self.docu) != types.StringType :
         self.cr.fatal("L'attribut 'docu' doit etre une chaine de caractères : %s" %`self.docu`)
      self.verif_cata_regles()

