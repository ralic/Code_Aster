#@ MODIF B_MACRO_ETAPE Build  DATE 11/04/2012   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE COURTOIS M.COURTOIS
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
# ======================================================================

"""
"""

# Modules Python
import traceback
import sys
import types

# Modules EFicas
import B_ETAPE
from Noyau.N_Exception import AsException
from Noyau import N__F
from Noyau.N_utils import AsType
import B_utils

class MACRO_ETAPE(B_ETAPE.ETAPE):
   """
   Cette classe implémente les méthodes relatives à la phase de construction
   d'une macro-etape.
   """
   macros={}

   def __init__(self):
      pass

   def Build(self):
      """
      Fonction : Construction d'une étape de type MACRO

      La construction n'est à faire que pour certaines macros.
      Ensuite on boucle sur les sous étapes construites
      en leur demandant de se construire selon le meme processus
      """
      self.set_current_step()
      self.building=None
      # Chaque macro_etape doit avoir un attribut cr du type CR
      # (compte-rendu) pour stocker les erreurs eventuelles
      # et doit l'ajouter au cr de l'etape parent pour construire un
      # compte-rendu hierarchique
      self.cr=self.CR(debut='Etape : '+self.nom + '    ligne : '+`self.appel[0]` + '    fichier : '+`self.appel[1]`,
                      fin = 'Fin Etape : '+self.nom)

      self.parent.cr.add(self.cr)
      try:
         ier = self._Build()
         if ier == 0 and self.jdc.par_lot == 'OUI':
            # Traitement par lot
            for e in self.etapes:
                if not e.isactif():continue
                ier=ier+e.Build()

         self.reset_current_step()
         return ier
      except:
         # Si une exception a ete levee, on se contente de remettre le step courant au pere
         # et on releve l'exception
         self.reset_current_step()
         raise

   def Build_alone(self):
      """
          Construction d'une étape de type MACRO.

          On ne construit pas les sous commandes et le
          current step est supposé correctement initialisé
      """
      ier = self._Build()
      return ier

   def _Build(self):
      """
         Cette méthode réalise le traitement de construction pour
         l'objet lui meme
      """
      if CONTEXT.debug : print "MACRO_ETAPE._Build ",self.nom,self.definition.op

      ier=0
      try:
         if self.definition.proc is not None:
            # On est dans le cas d'une macro en Python. On evalue la fonction
            # self.definition.proc dans le contexte des valeurs de mots clés (d)
            # La fonction proc doit demander la numerotation de la commande (appel de set_icmd)
            d=self.cree_dict_valeurs(self.mc_liste)
            ier= apply(self.definition.proc,(self,),d)
         elif self.macros.has_key(self.definition.op):
            ier= self.macros[self.definition.op](self)
         else:
            # Pour presque toutes les commandes (sauf FORMULE et POURSUITE)
            # le numero de la commande n est pas utile en phase de construction
            # Néanmoins, on le calcule en appelant la methode set_icmd avec un
            # incrément de 1
            # un incrément de 1 indique que la commande compte pour 1 dans
            # la numérotation globale
            # un incrément de None indique que la commande ne sera pas numérotée.
            self.set_icmd(1)

         if ier:
            self.cr.fatal(_(u"Erreur dans la macro %s\nligne : %s fichier : "),
                self.nom, self.appel[0], self.appel[1])

      except AsException,e:
         ier=1
         self.cr.fatal(_(u"Erreur dans la macro %s\n%s"), self.nom, e)
      except (EOFError,self.jdc.UserError):
         raise
      except:
         ier=1
         l=traceback.format_exception(sys.exc_info()[0],sys.exc_info()[1],sys.exc_info()[2])
         self.cr.fatal(_(u"Erreur dans la macro %s\n%s"), self.nom, ' '.join(l))

      return ier

   def gcncon(self,type):
      """
          Entrees:
            type vaut soit
                    '.' : le concept sera detruit en fin de job
                    '_' : le concept ne sera pas detruit
          Sorties:
            resul  nom d'un concept delivre par le superviseur
                   Ce nom est de la forme : type // '9ijklmn' ou ijklmn est un nombre
                   incremente a chaque appel pour garantir l unicite des noms
                   Il est donc differencie tant que l'on ne depasse pas 8999999,
                   de ceux donnes par le fortran qui sont de
                   la forme : type // 'ijklmnp'
          Fonction:
            Delivrer un nom de concept non encore utilise et unique
      """
      ### self.jdc.nsd=self.jdc.nsd+1 #CD : ca me semble inutile
      return type + "9" + str(self.jdc.nsd).zfill(6)

   def DeclareOut(self,nom,concept):
      """
          Methode utilisee dans une macro lors de la construction
          de cette macro en Python (par opposition a une construction en Fortran).
          Elle a pour but de specifier le mapping entre un nom de concept local
          a la macro (nom) et le concept de sortie effectif qui existe deja
          au moment de la construction
          Cette information sera utilisee lors de la creation du concept produit
          par une sous commande créée postérieurement.
      """
      self.Outputs[nom]=concept

   def get_sd_avant_etape(self,nom,etape):
      """
          Retourne le concept de nom nom defini avant l etape etape
      """
      sd=self.parent.get_sd_avant_etape(nom,self)
      if not sd:
         d=self.get_contexte_avant(etape)
         sd= d.get(nom,None)
      return sd

   def get_sdprod_byname(self,name):
      """
          Fonction : Retourne le concept produit de la macro self dont le nom est name
                     Si aucun concept produit n'a le nom name, retourne None
      """
      sd=None
      if self.sd and self.sd.nom == name :
         sd= self.sd
      else :
         for sdprod in self.sdprods:
            if sdprod.nom == name :
               sd= sdprod
               break
      return sd
