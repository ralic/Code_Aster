#@ MODIF E_ETAPE Execution  DATE 07/11/2011   AUTEUR COURTOIS M.COURTOIS 
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
# ======================================================================

"""
"""
# Modules Python
import sys
import os
from os import times

# Modules Eficas
from Noyau.N_utils import prbanner
from Noyau.N_Exception import AsException
from Noyau.N_MACRO_ETAPE import MACRO_ETAPE
from Noyau.N_info import message, SUPERV
from strfunc import convert

import genpy
import aster
import checksd

class ETAPE:
   """
   Cette classe implémente les méthodes relatives à la phase d'execution.

   Les méthodes principales sont:
      - Exec, realise la phase d'execution, en mode par lot
      - Execute, realise la phase d'execution, en mode commande par commande
   """

   def Exec(self):
      """
      Realise une passe d'execution sur l'operateur "fortran" associé
      si le numero d'operateur (self.definition.op) est défini.

      On execute l'operateur numero self.definition.op
      en lui passant les arguments :
        - self : la commande courante
        - lot : le mode d'execution qui peut prendre comme valeur :
               -  0 = execution par lot (verification globale avant execution)
               -  1 = execution commande par commande (verification + execution commande par commande)
        - ipass : passe d'execution
               -  1 = verifications supplémentaires
               -  2 = execution effective
        - icmd  : numéro d'ordre de la commande
      Retour : iertot = nombre d erreurs
      """
      from Utilitai.Utmess import UTMESS, MessageLog
      if CONTEXT.debug :
           prbanner(" appel de l operateur %s numero %s " % (self.definition.nom,self.definition.op))

      # On n'execute pas les etapes qui n'ont pas de numero d'operateur associé
      if self.definition.op is None :return 0

      assert(type(self.modexec)==int),"type(self.modexec)="+`type(self.modexec)`
      assert(type(self.definition.op)==int),"type(self.definition.op)="+`type(self.definition.op)`

      ier=0

      # Il ne faut pas executer les commandes non numerotees
      # Pour les affichages et calculs de cpu, on exclut les commandes
      # tout de suite executees (op_init) de type FORMULE. Sinon, ca
      # produit un affichage en double.
      if self.icmd is not None:
          # appel de la methode oper dans le module codex
          if (self.modexec == 2) and (self.definition.op_init==None):
             self.AfficheTexteCommande()
             if self.sd and self.jdc.sdveri:
                l_before = checksd.get_list_objects()

          self.jdc.timer.Stop(' . part Superviseur')
          self.jdc.timer.Start(' . part Fortran', num=1.2e6)
          ier=self.codex.oper(self,self.jdc.jxveri,self.modexec,self.icmd)
          self.jdc.timer.Stop(' . part Fortran')
          self.jdc.timer.Start(' . part Superviseur')

          # enregistrement des concepts sensibles en attente
          self.jdc.memo_sensi.register_sensi()

          if (self.modexec == 2) and (self.definition.op_init==None):
             # marque le concept comme calculé
             if self.sd:
               self.sd.executed = 1
             # vérification de la SD produite
             if self.sd and self.jdc.sdveri:
                # on force la vérif si :
                     # concept réentrant
                     # sd.nom absent du contexte (detruit ? car les nouveaux sont forcément vérifiés)
                     # pb avec macro reentrante et commande non reentrante : cf. ssnv164b ou ssll14a
                force = (self.reuse is not None) \
                     or (self.parent.get_contexte_avant(self).get(self.sd.nom) is None) \
                     or (self.parent != self.jdc)
                self.jdc.sd_checker.force(force)
                self.jdc.timer.Start('   . sdveri', num=1.15e6)
                self.jdc.sd_checker = checksd.check(self.jdc.sd_checker, self.sd, l_before, self)
                self.jdc.timer.Stop('   . sdveri')
             # pour arreter en cas d'erreur <E>
             MessageLog.reset_command()
             # affichage du texte de la commande
             self.AfficheFinCommande()
      else:
          if self.modexec == 2:
             # affichage du texte de la commande
             self.AfficheTexteCommande()

      if CONTEXT.debug :
           prbanner(" fin d execution de l operateur %s numero %s " % (self.definition.nom,
                                                                       self.definition.op))
      return ier

   def AfficheTexteCommande( self, sortie=sys.stdout ) :
      """
      Methode : ETAPE.AfficheTexteCommande
      Intention : afficher sur la sortie standard (par defaut) le cartouche de
                      la commande avant son execution.
      """
      from Utilitai.Utmess import UTMESS
      # top départ du chrono de la commande
      etiq = self.nom
      if (isinstance(self.parent,MACRO_ETAPE)) or \
         (self.parent.nom=='INCLUDE'         ):
         etiq = ' . ' + etiq
      self.jdc.timer.Start(id(self), name=etiq)

      # impression du fichier .code : compte rendu des commandes et
      # mots clés activés par l'ETAPE
      if self.jdc.fico!=None :
        v=genpy.genpy(defaut='avec',simp='into')
        self.accept(v)
        chaine = ' %-10s%-20s' % (self.jdc.fico, self.nom)
        for mc in v.args.keys():
            if type(v.args[mc]) in (str, unicode):
              chainec = '%s %-20s%-20s%-20s' % (chaine, '--', mc, v.args[mc])
              aster.affiche('CODE', chainec)
            elif type(v.args[mc]) == list:
              for mcs in v.args[mc]:
                 for mcf in mcs.keys():
                  chainec = '%s %-20s%-20s%s' % (chaine, mc, mcf, mcs[mcf])
                  aster.affiche('CODE', chainec)
            elif type(v.args[mc]) == dict:
                mcs = v.args[mc]
                for mcf in mcs.keys():
                  chainec = '%s %-20s%-20s%s' % (chaine, mc, mcf, mcs[mcf])
                  aster.affiche('CODE', chainec)

      if (not isinstance(self.parent,MACRO_ETAPE)) or \
         (self.parent.nom=='INCLUDE'             ) or \
         (self.jdc.impr_macro==1                 ) :

         # Affichage numero de la commande (4 digits)
         if self.sd != None:
            type_concept = self.sd.__class__.__name__
         else:
            type_concept = '-'

         UTMESS('I', 'VIDE_1')
         UTMESS('I', 'SUPERVIS2_70')
         if self.icmd != None:
            UTMESS('I', 'SUPERVIS2_71', vali=self.icmd, valk=type_concept)
            UTMESS('I', 'SUPERVIS2_70')
         else:
            # commande non comptabilisée (INCLUDE)
            UTMESS('I', 'SUPERVIS2_72', valk=type_concept)

         # recuperation du texte de la commande courante dans la chaine
         # commande_formatee
         v=genpy.genpy(defaut='avec')
         self.accept(v)
         commande_formatee = v.formate_etape()
         aster.affiche('MESSAGE', convert(commande_formatee))

      return

   def AfficheFinCommande( self, avec_temps=True, sortie=sys.stdout ) :
      """
      Methode : ETAPE.AfficheFinCommande
      Intention : afficher sur la sortie standard (par defaut) la fin du
                  cartouche de la commande apres son execution.
      """
      from Utilitai.Utmess import UTMESS
      voir = (not isinstance(self.parent,MACRO_ETAPE)) or \
             (self.parent.nom=='INCLUDE'             ) or \
             (self.jdc.impr_macro==1                 )
      # stop pour la commande
      cpu_user, cpu_syst, elapsed = self.jdc.timer.StopAndGet(id(self), hide=not voir)
      if voir :
         if avec_temps:
            rval = aster.jeinfo()
            if (rval[5] > 0. and rval[8] > 0.) :
              UTMESS('I', 'SUPERVIS2_73', valr=(rval[8]/1024, rval[4], rval[1]))
            else :
              UTMESS('I', 'SUPERVIS2_74', valr=(rval[4], rval[1]))
            UTMESS('I', 'SUPERVIS2_75', vali=self.icmd, valr=(cpu_syst+cpu_user, cpu_syst, elapsed))
         else :
            UTMESS('I', 'SUPERVIS2_76', valk=self.nom)
         UTMESS('I', 'SUPERVIS2_70')

         if cpu_user > 60. and cpu_syst > 0.5*cpu_user :
            if avec_temps and int(rval[7]) > 0 :
              UTMESS('A','SUPERVIS_94',valr=(cpu_syst,cpu_user),vali=(50,int(rval[7])))
            else :
              UTMESS('A','SUPERVIS_95',valr=(cpu_syst,cpu_user),vali=(50))

      return

   def Execute(self):
      """
      Cette methode realise l execution complete d une etape, en mode commande par commande :
             - construction,
             - verification,
             - execution
      en une seule passe. Utilise en mode PAR_LOT='NON'

      L'attribut d'instance executed indique que l'etape a deja ete executee
      Cette methode peut etre appelee plusieurs fois mais l'execution proprement
      dite ne doit etre realisee qu'une seule fois.
      Le seul cas ou on appelle plusieurs fois Execute est pour la
      commande INCLUDE (appel dans op_init)
      """
      message.debug(SUPERV, "%s par_lot=%s", self.nom, self.jdc and self.jdc.par_lot)
      if not self.jdc or self.jdc.par_lot != "NON" :
         return

      if hasattr(self,"executed") and self.executed == 1:return
      self.executed=1

      cr=self.report()
      self.parent.cr.add(cr)
      if not cr.estvide():
        raise EOFError

      #self.set_current_step()
      #try:
      if True:
         self.Build()

         self.setmode(1)
         self.Exec()
         self.setmode(2)
         try:
            self.Exec()
         except self.codex.error:
            self.detruit_sdprod()
            raise
      #except:
          #self.reset_current_step()
      #self.reset_current_step()

   def detruit_sdprod(self):
      """ Cette méthode supprime le concept produit par la commande
          du registre tenu par le JDC
      """
      if self.sd is not None:
          self.jdc.del_concept(self.sd.nom)

   def BuildExec(self):
      """
      Cette methode realise l execution complete d une etape, en mode commande par commande :
             - construction,
             - execution
      en une seule passe. Utilise en mode PAR_LOT='OUI'

      L'attribut d'instance executed indique que l'etape a deja ete executee
      Cette methode peut etre appelee plusieurs fois mais l'execution proprement
      dite ne doit etre realisee qu'une seule fois.
      Le seuls cas ou on appelle plusieurs fois Execute est pour la
      commande INCLUDE (appel dans op_init)
      """
      message.debug(SUPERV, "BuildExec %s", self.nom)
      if hasattr(self,"executed") and self.executed == 1:return
      self.executed=1

      #self.set_current_step()
      #try:
      if True:
         # Construction des sous-commandes
         self.Build()

         self.setmode(1)
         self.Exec()
         self.setmode(2)
         self.Exec()
      #except:
         #self.reset_current_step()
         #raise
      #self.reset_current_step()

   def get_liste_etapes(self,liste):
      liste.append(self.etape)

