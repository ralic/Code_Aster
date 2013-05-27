# -*- coding: iso-8859-1 -*-
# person_in_charge: mathieu.courtois at edf.fr
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
"""
# Modules Python
import sys

from Noyau import N_ASSD
from Noyau.N_info import message, SUPERV

# Modules Eficas
from B_CODE import CODE

class JDC(CODE):
  """
  Cette classe implémente les méthodes de l'objet JDC pour la phase de construction (Build).

  Les méthodes principales sont :
     - Build, qui réalise la construction de toutes les étapes du jeu de commandes
  """

  def __init__(self):
     self.ini=0
     self.icmd=0
     self.actif_status=0

  def register(self,etape):
     """
        Cette méthode ajoute etape dans la liste des etapes : self.etapes
        et retourne un numéro d'enregistrement
        En plus, elle rend actives les étapes comprises entre DEBUT, POURSUITE
        et FIN et inactives les autres.
     """
     if etape.nom in ('DEBUT', 'POURSUITE') and self.actif_status == 0:
        # On passe en état actif
        self.actif_status=1
        etape.actif=1
     elif etape.nom == 'FIN':
        # On passe en état inactif après FIN
        etape.actif=1
        self.actif_status=-1
     elif self.actif_status == 1:
        etape.actif=1
     else :
        etape.actif=0

     self.etapes.append(etape)
     self.index_etapes[etape] = len(self.etapes) - 1
     return self.g_register(etape)


  def Build(self):
    """
    Fonction : Construction des étapes du jeu de commandes

    Toutes les étapes n'ont pas besoin d'etre construites.
    En général, seules certaines macros nécessitent d'etre construites.
    Cependant, on demande à toutes les étapes de se construire,
    cette phase pouvant etre réduite à sa plus simple expression.
    Il faut prendre garde que en cas d'exécution en mode commande par
    commande, la construction doit etre immédiatement suivie par l'exécution
    éventuellement précédée par la vérification
    """
    # Pour etre sur de ne pas se planter sur l appel a set_context on le met d abord a blanc
    CONTEXT.unset_current_step()
    CONTEXT.set_current_step(self)
    # On reinitialise le compte-rendu self.cr
    self.cr=self.CR(debut="CR de 1ere phase de construction de JDC",
                     fin  ="fin CR de 1ere phase de construction de JDC",
                    )

    ret=self._Build()
    if ret != 0:
      CONTEXT.unset_current_step()
      return ret
    self.g_context={}
    ier=0
    for e in self.etapes:
      if not e.isactif():continue
      #message.debug(SUPERV, "call etape.Build : %s, %s", e.nom, e)
      ret=e.Build()
      ier=ier+ret
      if ret == 0:
        e.update_context(self.g_context)
    #  On remet le contexte à blanc : impossible de créer des étapes
    CONTEXT.unset_current_step()
    return ier

  def _Build(self):
     """
         Cette méthode réalise le traitement de construction pour le
         JDC lui meme
     """
     if CONTEXT.debug : print "Build_JDC ",self.nom
     self.initexec()
     return 0

  def initexec(self):
     if not self.ini :
       self.codex.argv(sys.argv)
       self.codex.init(CONTEXT.debug)
       self.ini=1

  def get_sd_avant_etape(self,nom_sd,etape):
     """
         Cette méthode retourne la SD de nom nom_sd qui est éventuellement
          définie avant etape
     """
     d=self.get_contexte_avant(etape)
     sd= d.get(nom_sd,None)
     if not isinstance(sd, N_ASSD.ASSD):
         sd = None
     return sd
