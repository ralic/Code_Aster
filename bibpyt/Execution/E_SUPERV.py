#@ MODIF E_SUPERV Execution  DATE 13/03/2012   AUTEUR COURTOIS M.COURTOIS 
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
import sys
import os
import os.path as osp
import traceback
import re

# Pas d'import des autres packages d'aster car l'import de ce module est
# fait avant l'ajout des paths. Les autres imports seront possibles une fois
# les arguments de la ligne de commande parsés.
import E_Core
from strfunc import convert

class SUPERV:
   usage="""
 USAGE :
    asteru JDC.py --bibpyt="rep" --commandes="fic_commandes"
                [--memjeveux=taille_en_Mw | --memory=taille_en_Mo]
                [-rep_mat repertoire_materiau] [-rep_dex repertoire_datg]
                [-interact] [-verif]

    L'ancienne syntaxe reste possible pour des raisons de compatibilité :
       asteru JDC.py -eficas_path "rep" -commandes "fic_commandes" [-memjeveux taille_en_Mw]
                  [-rep_mat repertoire_materiau] [-rep_dex repertoire_datg]
                  [-interact] [-verif]

 Exemple:

    asteru JDC.py ---bibpyt=/opt/aster/stable/bibpyt --commandes=sslp09a.comm --memory=128
"""

   def __init__(self):
       self.jdc = None
       self.timer = None

   def MESSAGE(self,chaine):
      """
          La fonction MESSAGE n'est utilisee que dans le script courant pour afficher
          des messages sur la sortie des erreurs.
      """
      sys.stdout.flush()
      sortie = sys.stdout
      sortie.write( "JDC.py : " )
      sortie.write( chaine )
      sortie.write( '\n' )
      sortie.flush()
      return

   def error(self, *args):
      """Cet enrobage permet de s'assurer que le sys.path a été enrichi
      pour permettre d'importer Noyau."""
      from Noyau.N_info import message, SUPERV
      message.error(SUPERV, *args)

   def register(self):
      """Enregistre le JDC et les objets nécessaires à aster_core."""
      import aster_core
      from Utilitai.Utmess import MessageLog
      aster_core.register(self.jdc, self.coreopts, MessageLog, E_Core)

   def set_path(self):
      """Ajout des chemins pour les imports
      """
      bibpyt = self.coreopts.get_option('bibpyt')
      sys.path.insert(0, '.')
      sys.path.insert(0, bibpyt)
      sys.path.append(osp.join(bibpyt, 'Cata'))

   def set_i18n(self):
       """Met en place les fonctions d'internationalisation."""
       import i18n

   def init_timer(self):
      """Initialise le timer au plus tot
      """
      try:
         from Utilitai.as_timer import ASTER_TIMER
         self.timer = ASTER_TIMER(format='aster')
         self.timer.Start('init (jdc)')
         self.timer.Start(' . part Superviseur', num=1.1e6)
         ier = 0
      except:
         print traceback.print_exc()
         ier = 1
      return ier

   def imports(self):
      try :
         import Cata
         from Cata import cata
         from Cata.cata import JdC
         self.cata=cata
         self.JdC=JdC
         CONTEXT.unset_current_step()
      except :
         print traceback.print_exc()
         return 1

   def testeCata(self):
      """
         Verifie que le catalogue de commandes est valide
      """
      cr = self.JdC.report()
      if not cr.estvide() :
         self.error("ERREUR A LA VERIFICATION DU CATALOGUE - INTERRUPTION")
         self.error(">> Catalogue de commandes : DEBUT RAPPORT")
         self.error(str(cr))
         self.error(">> Catalogue de commandes : FIN RAPPORT")
         return 1

   def InitJDC(self, params):
      """
         Construit et execute le jeu de commandes
      """
      fort1 = self.coreopts.get_option('fort1')
      f = open(fort1, 'r')
      text = f.read()
      print "# ------------------------------------------------------------------------------------------"
      print convert(_(u"""# Impression du contenu du fichier de commandes à exécuter :"""))
      print "# ------------------------------------------------------------------------------------------"
      print convert(text)
      print "# ------------------------------------------------------------------------------------------"
      print "# ------------------------------------------------------------------------------------------"
      f.close()

      args = {}
      self.jdc = self.JdC(procedure=text, cata=self.cata, nom=fort1,
             context_ini=params, **args
           )

   def CompileJDC(self):
      """Compile the JDC content (byte-compile).
      Python syntax errors will be detected here."""
      assert self.jdc is not None, 'jdc must be initialized (call Init(...) before)'
      j = self.jdc
      # on enregistre les objets dans aster_core dès que le jdc est créé
      self.register()

      # on transmet le timer au jdc
      j.timer = self.timer

      # On compile le texte Python
      j.timer.Start(" . compile")
      j.compile()
      j.timer.Stop(" . compile")

      if not j.cr.estvide():
         self.error("ERREUR DE COMPILATION DANS ACCAS - INTERRUPTION")
         self.error(">> DEBUT RAPPORT")
         self.error(str(j.cr))
         self.error(">> FIN RAPPORT")
         j.supprime()
         return 1

   def ExecCompileJDC(self):
      """Execute the JDC :
      - with PAR_LOT='OUI', only the ETAPE objects are built ;
      - with PAR_LOT='NON', the operators are immediatly called after its ETAPE
        object is created.
      """
      assert self.jdc is not None, 'jdc must be initialized (call Init(...) before)'
      j = self.jdc
      j.timer.Start(" . exec_compile")
      j.exec_compile()
      j.timer.Stop(" . exec_compile")
      ier=0
      if not j.cr.estvide():
         self.error("ERREUR A L'INTERPRETATION DANS ACCAS - INTERRUPTION")
         self.error(">> DEBUT RAPPORT")
         self.error(str(j.cr))
         self.error(">> FIN RAPPORT")
         ier=1

      if self.coreopts.get_option('interact'):
         # Si l'option -interact est positionnée on ouvre un interpreteur interactif
         j.interact()
      return ier

   def CheckCata(self):
      """Check Code_Aster syntax (using cata.py)."""
      assert self.jdc is not None, 'jdc must be initialized (call Init(...) before)'
      j = self.jdc
      j.timer.Start(" . report")
      cr = j.report()
      j.timer.Stop(" . report")
      if not cr.estvide():
         self.error("ERREUR A LA VERIFICATION SYNTAXIQUE - INTERRUPTION")
         self.error(">> DEBUT RAPPORT")
         self.error(str(cr))
         self.error(">> FIN RAPPORT")
         return 1

   def ChangeJDC(self):
      """Modify the JDC object depending on the called features.
      Only for sensitivity yet."""
      assert self.jdc is not None, 'jdc must be initialized (call Init(...) before)'
      j = self.jdc
      # Modification du JDC dans le cas de sensibilité
      # On détermine si le jdc en cours est concerné par un calcul de sensibilité
      # . Si c'est le cas, on crée un nouveau jdc. On controle ce nouveau jdc. Si tout
      #   va bien, on remplace l'objet qui contenait le jdc initial par le nouveau.
      # . Sinon, on ne fait rien.
      codret, est_sensible = j.is_sensible()
      if codret == 0 :
        if est_sensible :
          j.timer.Start(" . sensi")
          codret, new_j = j.cree_jdc_sensible()
          j.timer.Stop(" . sensi")
          if codret == 0 :
            cr=new_j.report()
            if not cr.estvide():
              codret = 1
              print ">> JDC.py : DEBUT RAPPORT"
              print cr
              print ">> JDC.py : FIN RAPPORT"
      if codret == 0 :
        if est_sensible :
            # ne pas appeler la methode supprime car on ne copie pas les etapes
            # (risque de perte d'informations)
            # j.supprime()
            self.jdc = new_j
      else :
        self.MESSAGE("ERREUR AU DECODAGE DES SENSIBILITES - INTERRUPTION")
        return 1
      # fin des initialisations
      j.timer.Stop("init (jdc)")

   def Execute(self, params):
      """Execution of the JDC object."""
      ier = self.ParLotMixte()
      return ier

   def ParLot(self):
      """Execute the JDC calling Build and Exec methods."""
      # not used for Code_Aster
      assert self.jdc is not None, 'jdc must be initialized (call Init(...) before)'
      j = self.jdc
      try:
         ier=j.Build()
         if ier or not j.cr.estvide():
            self.MESSAGE("ERREUR A LA CONSTRUCTION DES MACROS - INTERRUPTION")
            print ">> JDC.py : DEBUT RAPPORT"
            print j.cr
            print ">> JDC.py : FIN RAPPORT"
            return 1
      except :
         self.MESSAGE("ERREUR INOPINEE - INTERRUPTION")
         traceback.print_exc()
         return 1

      cr=j.report()
      if not cr.estvide():
         self.MESSAGE("ERREUR A LA VERIFICATION DES MACROS - INTERRUPTION")
         print ">> JDC.py : DEBUT RAPPORT"
         print cr
         print ">> JDC.py : FIN RAPPORT"
         return 1
      try:
         ier=j.Exec()
         if ier :
            self.MESSAGE("ERREUR A L'EXECUTION - INTERRUPTION")
            return 1
      except EOFError:
          return 0
      except :
         self.MESSAGE("ERREUR INOPINEE - INTERRUPTION")
         traceback.print_exc()
         return 1

   def ParLotMixte(self):
       """Execute the JDC using BuildExec"""
       from Noyau.N_JDC    import MemoryErrorMsg
       assert self.jdc is not None, 'jdc must be initialized (call Init(...) before)'
       j = self.jdc
       j.set_par_lot("NON")
       try:
           j.BuildExec()
           ier=0
           if not j.cr.estvide():
               self.MESSAGE("ERREUR A L'EXECUTION - INTERRUPTION")
               ier=1
               print ">> JDC.py : DEBUT RAPPORT"
               print j.cr
               print ">> JDC.py : FIN RAPPORT"
           return ier
       except MemoryError:
           self.MESSAGE("ERREUR INOPINEE - INTERRUPTION")
           print ">> JDC.py : DEBUT RAPPORT"
           self.MESSAGE(MemoryErrorMsg)
           traceback.print_exc()
           print ">> JDC.py : FIN RAPPORT"
           return 1
       except :
           self.MESSAGE("ERREUR INOPINEE - INTERRUPTION")
           traceback.print_exc()
           return 1

   def InitEnv(self):
      """Initialize the environment (language & encoding, paths...)"""
      # import after getting opts as is may change sys.path
      from E_utils import copierBase, lierRepertoire
      if self.coreopts.get_option('totalview') == 1:
         curPID = os.getpid()
         pathOrigine = os.getcwd()
         pathDestination = osp.join(pathOrigine, "tv_" + str(curPID))
         # Creation des liens symboliques vers les fichiers du
         # repertoire courant dans un sous repertoire
         lierRepertoire(pathOrigine, pathDestination, ["tv_"])
         copierBase(pathOrigine, pathDestination)
         os.chdir(pathDestination)

      self.set_i18n()
      ier = self.init_timer()
      if ier:return ier
      ier=self.imports()
      if ier:return ier

   def Finish(self):
      """Allow to call cleanup functions."""
      from E_utils import supprimerRepertoire
      if self.coreopts.get_option('totalview') == 1:
         supprimerRepertoire(os.getcwd())

   def main(self, params={}, coreopts=None):
      """
           Programme principal. Appelle les methodes internes qui realisent les
           divers traitements
      """
      if not coreopts:
          coreopts = E_Core.getargs()
      self.coreopts = coreopts

      ier = self.InitEnv()
      if ier:
         return ier
      #ier=self.testeCata();if ier:return ier
      self.InitJDC(params)
      ier = self.CompileJDC()
      if ier:
         return ier
      ier = self.ExecCompileJDC()
      if ier:
         return ier

      if self.jdc.par_lot == 'NON':
         print convert(_(u"""--- Fin de l'exécution"""))
         return ier

      ier = self.CheckCata()
      if ier:
         return ier
      if self.coreopts.get_option('verif'):
         return ier
      ier = self.ChangeJDC()
      if ier:
         return ier
      ier = self.Execute(params)
      self.Finish()
      return ier or 0


def main():
    """Main."""
    appli = SUPERV()
    ier = appli.main(coreopts=E_Core.getargs(sys.argv))
    sys.exit(ier)


if __name__ == '__main__':
   main()
#   import profile
#   profile.run('main()')

