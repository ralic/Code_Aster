#@ MODIF V_ETAPE Validation  DATE 26/09/2003   AUTEUR DURAND C.DURAND 
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
   Ce module contient la classe mixin ETAPE qui porte les méthodes
   nécessaires pour réaliser la validation d'un objet de type ETAPE
   dérivé de OBJECT.

   Une classe mixin porte principalement des traitements et est
   utilisée par héritage multiple pour composer les traitements.
"""
# Modules Python
import string,types,sys
import traceback

# Modules EFICAS
import V_MCCOMPO
from Noyau.N_Exception import AsException
from Noyau.N_utils import AsType

class ETAPE(V_MCCOMPO.MCCOMPO):
   """
   """

   def valid_child(self):
       """ Cette methode teste la validite des mots cles de l'etape """
       for child in self.mc_liste :
           if not child.isvalid():
              return 0 
       return 1

   def valid_regles(self,cr):
       """ Cette methode teste la validite des regles de l'etape """
       text_erreurs,test_regles = self.verif_regles()
       if not test_regles :
          if cr == 'oui' : self.cr.fatal(string.join(("Règle(s) non respectée(s) :", text_erreurs)))
          return 0 
       return 1

   def valid_sdnom(self,cr):
       """ Cette methode teste la validite du nom du concept produit par l'etape """
       valid=1
       if self.sd.nom != None :
          if self.jdc and self.jdc.definition.code == 'ASTER' and len(self.sd.nom) > 8 :
             #  le nom de la sd doit avoir une longueur <= 8 caractères pour ASTER
             if cr == 'oui' :
                self.cr.fatal("Le nom de concept %s est trop long (8 caractères maxi)" %self.sd.nom)
             valid = 0
          if string.find(self.sd.nom,'sansnom') != -1 :
             # la SD est 'sansnom' : --> erreur
             if cr == 'oui' :
                self.cr.fatal("Pas de nom pour le concept retourné")
             valid = 0
          elif string.find(self.sd.nom,'SD_') != -1 :
             # la SD est 'SD_' cad son nom = son id donc pas de nom donné par utilisateur : --> erreur
             if cr == 'oui' :
                self.cr.fatal("Pas de nom pour le concept retourné")
             valid = 0
       return valid

   def get_valid(self):
       if hasattr(self,'valid'):
          return self.valid
       else:
          self.valid=None
          return None

   def set_valid(self,valid):
       old_valid=self.get_valid()
       self.valid = valid
       self.state = 'unchanged'
       if not old_valid or old_valid != self.valid : 
           self.init_modif_up()

   def isvalid(self,sd='oui',cr='non'):
      """ 
         Methode pour verifier la validité de l'objet ETAPE. Cette méthode
         peut etre appelée selon plusieurs modes en fonction de la valeur
         de sd et de cr.

         Si cr vaut oui elle crée en plus un compte-rendu.

         Cette méthode a plusieurs fonctions :

          - mettre à jour l'état de self (update)

          - retourner un indicateur de validité 0=non, 1=oui

          - produire un compte-rendu : self.cr

      """
      if CONTEXT.debug : print "ETAPE.isvalid ",self.nom
      if self.state == 'unchanged' :
        return self.valid
      else:
        valid=self.valid_child()
        valid=valid * self.valid_regles(cr)

        if self.reste_val != {}:
          if cr == 'oui' :
            self.cr.fatal("Mots cles inconnus :" + string.join(self.reste_val.keys(),','))
          valid=0

        if sd == "non":
          # Dans ce cas, on ne teste qu'une validité partielle (sans tests sur le concept produit)
          # Conséquence : on ne change pas l'état ni l'attribut valid, on retourne simplement
          # l'indicateur de validité valid
          return valid

        if self.definition.reentrant == 'n' and self.reuse:
          # Il ne peut y avoir de concept reutilise avec un OPER non reentrant
          if cr == 'oui' : self.cr.fatal('Operateur non reentrant : ne pas utiliser reuse ')
          valid=0

        if self.sd == None:
          # Le concept produit n'existe pas => erreur
          if cr == 'oui' : self.cr.fatal("Concept retourné non défini")
          valid = 0
        else:
          valid = valid * self.valid_sdnom(cr)

        if valid:
          valid = self.update_sdprod(cr)

        self.set_valid(valid)

        return self.valid

   def update_sdprod(self,cr='non'):
      """ 
           Cette méthode met à jour le concept produit en fonction des conditions initiales :

            1- Il n'y a pas de concept retourné (self.definition.sd_prod == None)

            2- Le concept retourné n existait pas (self.sd == None)

            3- Le concept retourné existait. On change alors son type ou on le supprime

           En cas d'erreur (exception) on retourne un indicateur de validité de 0 sinon de 1
      """
      sd_prod=self.definition.sd_prod
      if type(sd_prod) == types.FunctionType: # Type de concept retourné calculé
        d=self.cree_dict_valeurs(self.mc_liste)
        try:
          sd_prod= apply(sd_prod,(),d)
        except:
          # Erreur pendant le calcul du type retourné
          if CONTEXT.debug:traceback.print_exc()
          self.sd=None
          if cr == 'oui' : 
             l=traceback.format_exception(sys.exc_info()[0],
                                           sys.exc_info()[1],
                                           sys.exc_info()[2])
             self.cr.fatal('Impossible d affecter un type au résultat\n'+string.join(l[2:]))
          return 0
      # on teste maintenant si la SD est r\351utilis\351e ou s'il faut la cr\351er
      valid=1
      if self.reuse:
        if AsType(self.reuse) != sd_prod:
          if cr == 'oui' : self.cr.fatal('Type de concept reutilise incompatible avec type produit')
          valid= 0
        if self.sdnom!='':
           if self.sdnom[0] != '_' and self.reuse.nom != self.sdnom:
             # Le nom de la variable de retour (self.sdnom) doit etre le meme que celui du concept reutilise (self.reuse.nom)
             if cr == 'oui' : 
                self.cr.fatal('Concept reutilise : le nom de la variable de retour devrait etre %s et non %s' %(self.reuse.nom,self.sdnom))
             valid= 0
        if valid:self.sd=self.reuse
      else:
        if sd_prod == None:# Pas de concept retourné
          # Que faut il faire de l eventuel ancien sd ?
          self.sd = None
        else:
          if self.sd: 
             # Un sd existe deja, on change son type
             self.sd.__class__=sd_prod
          else: 
             # Le sd n existait pas , on ne le crée pas
             if cr == 'oui' : self.cr.fatal("Concept retourné non défini")
             valid=0 
        if self.definition.reentrant == 'o':
           if cr == 'oui' : self.cr.fatal('Commande obligatoirement reentrante : specifier reuse=concept')
           valid=0 
      return valid


   def report(self):
      """ 
          Methode pour generation d un rapport de validite
      """
      self.cr=self.CR(debut='Etape : '+self.nom \
                + '    ligne : '+`self.appel[0]`\
                + '    fichier : '+`self.appel[1]`,
                 fin = 'Fin Etape : '+self.nom)
      self.state = 'modified'
      try:
        self.isvalid(cr='oui')
      except AsException,e:
        if CONTEXT.debug : traceback.print_exc()
        self.cr.fatal(string.join(('Etape :',self.nom,
                              'ligne :',`self.appel[0]`,
                              'fichier :',`self.appel[1]`,str(e))))
      for child in self.mc_liste:
        self.cr.add(child.report())
      return self.cr

