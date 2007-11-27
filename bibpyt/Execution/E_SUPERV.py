#@ MODIF E_SUPERV Execution  DATE 28/11/2007   AUTEUR COURTOIS M.COURTOIS 
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

"""
# Modules Python
import sys
import os
import traceback
import re

class SUPERV:
   usage="""
 USAGE :

    pyaster JDC.py -eficas_path "rep" -commandes "fic_commandes" [-memjeveux taille_en_Mw]
                      [-rep_mat repertoire_materiau] [-interact] [-verif]

    rep : est le repertoire contenant les paquetages de modules python d'Eficas ;
    fic_commandes : est le nom du fichier de commandes :
           - extension ".py" si les commandes sont au format python ;
           - extension ".comm" si les commandes sont au format Aster, dans ce cas
             les commandes sont converties dans le fichier dont le nom se termine
             par .py ;
           - dans tous les autres cas les commandes sont au format python.
    repertoire_materiau : est le repertoire contenant la description des materiaux pour INCLUDE_MATERIAU
    interact : si présent, indique que le superviseur passera en interactif apres avoir interprété le
               fichier de commandes fic_commandes
    verif : si présent indique que seule la phase de vérification sera exécutée

    ATTENTION

    1. Pour le calcul, c'est ASTER V6 qui est utilise, autant pour l'edition de liens
       de pyaster que pour le catalogue cata.py.

    2. les allocations fortran fort.2, ... doivent avoir ete effectuees dans le shell qui
       lance python.
 Exemple:

    pyaster JDC.py -eficas_path $(HOME)/Eficas -commandes sslp09a.comm -memjeveux 8
"""
#XXX Voir s'il est possible de rajouter le traitement des options rep_outils, rep_dex, h, help, aide

   def __init__(self):pass

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

   def getargs(self):
      """
          Récupération des arguments passés à la ligne de commande
      """
      self.CHEMIN=None
      self.nomFichierCommandes=None
      k=0
      arg_debug=1
      self.rep_mat=None
      self.tempsMax=0.
      self.verif=0
      self.interact=0
      for arg in sys.argv :
         if sys.argv[k] == '-eficas_path' :
            self.CHEMIN=sys.argv[k+1]
            if not os.path.isdir(self.CHEMIN):
               self.MESSAGE('Ce chemin est introuvable : "'+self.CHEMIN+"'")
               return 1
            if  "Accas" not in os.listdir( self.CHEMIN ) :
               self.MESSAGE( "Ce chemin d'Eficas est ERRONE : '"+`self.CHEMIN`+"'" )
               return 1
         elif sys.argv[k] == '-commandes' :
            self.nomFichierCommandes=sys.argv[k+1]
            #self.nomFichierCommandes=os.path.abspath(sys.argv[k+1])
         elif sys.argv[k] == '-tpmax' :
            self.tempsMax=float(sys.argv[k+1])
         elif sys.argv[k] == '-rep_mat' :
            self.rep_mat=sys.argv[k+1]
            #self.rep_mat=os.path.abspath(sys.argv[k+1])
         elif sys.argv[k] == '-debug' :
            self.debug=arg_debug
         elif sys.argv[k] == '-verif' :
            self.verif=1
         elif sys.argv[k] == '-interact' :
            self.interact=1
         else :
            pass
         k=k+1

      if self.CHEMIN==None :
         print """JDC.py. Il faut passer un chemin en argument :
                           python JDC.py -eficas_path chemin"""
         print self.usage
         return 1
      elif self.nomFichierCommandes==None :
         print """JDC.py. Il faut passer un nom de fichier de commandes en argument :
                          python JDC.py -commandes nom_fichier"""
         print self.usage
         return 1

   def set_path(self):
      """Ajout des chemins pour les imports
      """
      sys.path.insert(0, '.')
      sys.path.insert(0, self.CHEMIN)
      sys.path.append(os.path.join(self.CHEMIN,'Cata'))

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
         self.MESSAGE("ERREUR A LA VERIFICATION DU CATALOGUE - INTERRUPTION")
         print ">> Catalogue de commandes : DEBUT RAPPORT"
         print cr
         print ">> Catalogue de commandes : FIN RAPPORT"
         return 1

   def Execute(self):
      """
         Construit et execute le jeu de commandes
      """
      f=open(self.nomFichierCommandes,'r')
      text=f.read()
      print '=========================================='
      print '=========================================='
      print text
      print '=========================================='
      print '=========================================='
      f.close()
      args={}
      if self.tempsMax:args['tempsMax']=self.tempsMax
      if self.rep_mat :args['rep_mat'] =self.rep_mat

      j=self.JdC(procedure=text,cata=self.cata,nom=self.nomFichierCommandes,
             **args
           )

      # on transmet le timer au jdc
      j.timer = self.timer

      # On compile le texte Python
      j.timer.Start(" . compile")
      j.compile()
      j.timer.Stop(" . compile")

      if not j.cr.estvide():
         self.MESSAGE("ERREUR DE COMPILATION DANS ACCAS - INTERRUPTION")
         print ">> JDC.py : DEBUT RAPPORT"
         print j.cr
         print ">> JDC.py : FIN RAPPORT"
         j.supprime()
         return 1

      j.timer.Start(" . exec_compile")
      j.exec_compile()
      j.timer.Stop(" . exec_compile")
      ier=0
      if not j.cr.estvide():
         self.MESSAGE("ERREUR A L'INTERPRETATION DANS ACCAS - INTERRUPTION")
         ier=1
         print ">> JDC.py : DEBUT RAPPORT"
         print j.cr
         print ">> JDC.py : FIN RAPPORT"

      if self.interact:
         # Si l'option -interact est positionnée on ouvre un interpreteur interactif
         j.interact()

      if j.par_lot == 'NON':
         print "FIN EXECUTION"
         if j.fico!=None :
            open('fort.15', 'a').write(open('ficode', 'r').read())
         return ier

      # Verification de la validite du jeu de commande
      j.timer.Start(" . report")
      cr=j.report()
      j.timer.Stop(" . report")
      if not cr.estvide():
         self.MESSAGE("ERREUR A LA VERIFICATION SYNTAXIQUE - INTERRUPTION")
         print ">> JDC.py : DEBUT RAPPORT"
         print cr
         print ">> JDC.py : FIN RAPPORT"
         return 1

      if self.verif:return

#     Modification du JDC dans le cas de sensibilité
#     On détermine si le jdc en cours est concerné par un calcul de sensibilité
#     . Si c'est le cas, on crée un nouveau jdc. On controle ce nouveau jdc. Si tout
#       va bien, on remplace l'objet qui contenait le jdc initial par le nouveau.
#     . Sinon, on ne fait rien.
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
            #ne pas appeler la methode supprime car on ne copie pas les etapes (risque de perte d'informations)
            #j.supprime()
            j = new_j
      else :
        self.MESSAGE("ERREUR AU DECODAGE DES SENSIBILITES - INTERRUPTION")
        return 1
      # fin des initialisations
      j.timer.Stop("init (jdc)")
      #ier= self.ParLot( j )
      ier= self.ParLotMixte( j )
      return ier


   def ParLot(self,j):

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


      j.setmode(1)
      ier=j.Exec()
      if ier :
         self.MESSAGE("ERREUR A LA VERIFICATION FORTRAN - INTERRUPTION")
         return 1

      j.setmode(2)
      try:
         ier=j.Exec()
         if ier :
            self.MESSAGE("ERREUR A L'EXECUTION - INTERRUPTION")
            return 1
      except EOFError:
         if j.fico!=None :
            open('fort.15', 'a').write(open('ficode', 'r').read())
         return 0
      except :
         self.MESSAGE("ERREUR INOPINEE - INTERRUPTION")
         traceback.print_exc()
         return 1


   def ParLotMixte(self,j):
       """
       """
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

           if j.fico!=None :
               open('fort.15', 'a').write(open('ficode', 'r').read())

           return ier
       except :
           self.MESSAGE("ERREUR INOPINEE - INTERRUPTION")
           traceback.print_exc()
           return 1

   def main(self):
      """
           Programme principal. Appelle les methodes internes qui realisent les
           divers traitements
      """
      ier=self.getargs()
      if ier:return ier

      self.set_path()

      ier = self.init_timer()
      if ier:return ier

      ier=self.imports()
      if ier:return ier

      #ier=self.testeCata();if ier:return ier

      return self.Execute()


def main():
    appli=SUPERV()
    ier=appli.main()
    sys.exit(ier)

if __name__ == '__main__':
   main()
#   import profile
#   profile.run('main()')

