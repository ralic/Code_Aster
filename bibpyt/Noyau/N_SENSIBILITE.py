#@ MODIF N_SENSIBILITE Noyau  DATE 07/09/2009   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE COURTOIS M.COURTOIS
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

"""
   Ce module contient les règles nécessaires aux commandes sensibles
   pour renseigner l'attribut etape.sd.sensi, gérer le caractère réentrant
   sur présence de la sensibilité.
"""

from types import TupleType, ListType
EnumTypes = (TupleType, ListType)

from N_REGLE import REGLE

# -----------------------------------------------------------------------------
class CONCEPT_SENSIBLE(REGLE):
   """Règle permettant de renseigner au niveau du catalogue comment sera
   rempli le concept (valeur nominale ou dérivée(s) ou les deux...).
   """
   def __init__(self, mode, mocle='SENSIBILITE'):
      """Constructeur.

         mode : manière dont la commande rempli le concept
            - 'ENSEMBLE' : concept nominal ET dérivées en une seule passe
            - 'SEPARE'   : concept nominal OU dérivée (une ou plusieurs)
            
         mocle : mot-clé contenant les paramètres sensibles.
      """
      REGLE.__init__(self)
      self.mocle = mocle
      self._modes = { 'ENSEMBLE' : 0, 'SEPARE' : 1 }
      self.mode = self._modes.get(mode, self._modes['ENSEMBLE'])

   def gettext(self):
      """Pour EFICAS
      """
      return ''

   def verif(self, args):
      """Retourne texte + 1 si ok, 0 si nook.
      On stocke dans sd.sensi l'étape courante, c'est-à-dire celle qui
      renseigne le concept si cela n'a pas déjà été fait (car verif est
      appelé à chaque validation).
      """
      obj = args["self"]
      etape = obj.etape
      id_etape = '%s_%s' % (etape.id, id(etape))
      if etape.sd == None:
          return '',1
      if not hasattr(etape.sd,"sensi"):
         etape.sd.sensi = {}
      # si ENSEMBLE, la sd nominale est forcément produite
      if self.mode == self._modes['ENSEMBLE'] and not etape.sd.sensi.has_key('nominal'):
         etape.sd.sensi['nominal'] = id_etape
      # liste des paramètres sensibles
      valeur = obj[self.mocle]
      if valeur == None:
         # pas de sensibilité, la sd nominale est produite
         if not etape.sd.sensi.has_key('nominal'):
            etape.sd.sensi['nominal'] = id_etape
         return '', 1
      if not type(valeur) in EnumTypes:
         valeur = [valeur,]
      for v in valeur:
         if not etape.sd.sensi.has_key(v.get_name()):
            etape.sd.sensi[v.get_name()] = id_etape
      return '', 1


# -----------------------------------------------------------------------------
class REUSE_SENSIBLE(REGLE):
   """Limite le caractère réentrant de la commande.
   On autorisera reuse seulement si le concept (au sens fortran) n'a pas déjà
   été calculé (d'après sd.sensi). Ce sera interdit dans les cas suivants :
      - sd nominale calculée et SENSIBILITE absent
      - PS1 dans SENSIBILITE et sd dérivée par rapport à PS1 calculée
   """
   def __init__(self, mocle='SENSIBILITE'):
      """Constructeur.
         mocle : mot-clé SENSIBILITE.
      """
      REGLE.__init__(self)
      self.mocle = mocle

   def gettext(self):
      """Pour EFICAS
      """
      return ''

   def verif(self,args):
      """Retourne texte + 1 si ok, 0 si nook = reuse interdit.
      Comme CONCEPT_SENSIBLE est appelé avant (et à chaque validation),
      on regarde si sd.sensi[ps] a été renseigné par une étape précédente.
      """
      obj = args["self"]
      etape = obj.etape
      id_etape = '%s_%s' % (etape.id, id(etape))
      sd = etape.sd
      # si la commande n'est pas réentrante, rien à faire
      if etape.reuse is not None:
         valeur = obj[self.mocle]
         if valeur is None:
            if not hasattr(sd, 'sensi') or sd.sensi.get('nominal', id_etape) != id_etape:
               # pas de sensibilite et concept nominal déjà calculé : reuse interdit
               text = "Commande non réentrante en l'absence de sensibilité."
               return text, 0
         else:
            if not type(valeur) in EnumTypes:
               valeur = [valeur,]
            for ps in valeur:
               if hasattr(sd, 'sensi') and sd.sensi.get(ps.nom, id_etape) != id_etape:
                  # concept dérivé par rapport à ps déjà calculé : reuse interdit
                  text = "Commande non réentrante : dérivée par rapport à %s déjà calculée" % ps.nom
                  return text, 0
      return '', 1


# -----------------------------------------------------------------------------
class DERIVABLE(REGLE):
   """Déclare que le concept fourni derrière un mot-clé est dérivable.
   Sa présence ne suffit pas à le valider, il faut encore que son attribut
   '.sensi' soit cohérent avec le contenu du mot-clé SENSIBILITE (ou l'absence
   de celui-ci).
   """
   def __init__(self, mocle):
      """Constructeur.
         mocle : mot-clé dérivable.
      """
      REGLE.__init__(self)
      self.mocle = mocle

   def gettext(self):
      """Pour EFICAS
      """
      return ''

   def verif(self,args):
      """
      """
      obj = args["self"]
      try:
         concept = obj[self.mocle]
      except IndexError:
         return '', 1
      if not type(concept) in EnumTypes:
         concept = [concept,]
      l_ps = obj["SENSIBILITE"]
      for co in concept:
         if co is None:
            text = "Concept non défini (None) sous le mot-clé %s" % self.mocle
            return text, 0
         if not l_ps:
            # pas de sensibilité
            if hasattr(co,"sensi") and not co.sensi.get('nominal'):
               text = "%s ne contient que des valeurs dérivées, utilisez le mot cle SENSIBILITE" %\
                     co.nom
               return text, 0
         else:
            # sensibilité spécifiée
            if not type(l_ps) in EnumTypes:
               l_ps = [l_ps,]
            for ps in l_ps:
               if not hasattr(co,"sensi") or not co.sensi.get(ps.nom):
                  text = "La dérivée de %s par rapport à %s n'est pas disponible." %\
                        (co.nom, ps.nom)
                  return text, 0
      return '', 1

