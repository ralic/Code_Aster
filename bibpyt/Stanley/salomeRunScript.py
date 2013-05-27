# -*- coding: iso-8859-1 -*-
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

import tempfile
import re
import os

debug = False


# ----------------------------------------------------------------------------------------------------------------------
def MakeTempScript(SALOMESCRIPT, **args):
   """
      Construit un nouveau script en remplacant les variables par leur valeur
      args contient le dictionnaire des champs a remplacer (entoures de $ dans le script) et leur valeur
   """
   try:
       import aster
       from Utilitai.Utmess import  UTMESS
   except:
       def UTMESS(code='I', txt='',valk='', vali='', valr=''):
           print 'Le champs :"%s" n''a pas été trouvé dans le script Salome, mais il n''est peut être pas nécessaire.' % valk

   try:
      # Recupere le script Salome
      f = open(SALOMESCRIPT, 'r')
      txt = f.read()
      f.close()

      # Remplace les champs par leur valeur
      for para, pval in args.items():
          #print para, pval
          if pval: pval = "'%s'" % str(pval)
          else:    pval = "''"
          if not txt.find(para)!=-1: UTMESS('I', 'EXECLOGICIEL0_19', valk=para) 
          exp = re.compile('^( *)(%s *=.*)$' % para, re.MULTILINE)
          txt = exp.sub('\g<1>%s = %s' % (para, pval), txt)

      # Ecrire le nouveau script
      fw = tempfile.NamedTemporaryFile(mode='w', suffix='.py')
#      tmpfile = fw.name
      tmpfile = os.path.join(os.getcwd(), os.path.basename(fw.name) )
      fw.close()

#      tmpfile = TempFileName(mode='w', suffix='.py')
      fw = open(tmpfile, 'w')
      fw.write( txt + '\n' )
      fw.close()

   except Exception, e:
      raise Exception("Erreur : \n%s" % e)

   if debug: print "tmpfile: %s" % tmpfile

   return tmpfile


# ----------------------------------------------------------------------------------------------------------------------
def DelTempScript(SALOMESCRIPT):
   """
      Efface le script temporaire
   """
   try:
      os.remove(SALOMESCRIPT)
   except Exception, e:
      print "Impossible d'effacer le script : %s\n\nErreur : %s" % (SALOMESCRIPT, e)


# ----------------------------------------------------------------------------------------------------------------------
def RunScript(SALOMESCRIPT, CHOIX, INPUTFILE):
   """
      Lance le script temporaire et l'efface
   """
   tmpfile = MakeTempScript( SALOMESCRIPT=SALOMESCRIPT, CHOIX=CHOIX, INPUTFILE=INPUTFILE, OUTPUTFILE=False, STUDY=False )
   execfile(r"%s" % tmpfile)
   if not debug: DelTempScript(tmpfile)



# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------
if __name__ == '__main__':

   # --------------------------------------------------------
   # ---------------------------------------------------------
   # Tests unitaire a lancer depuis Salome
   # Remplacer les deux chemins ci-dessous par vos surcharges, sinon le script tentera de detecter l'installation d'Aster

#    root_astest  = '/home/assire/DEV/PYLOTAGE/astest'
#    salomeScript = '/home/assire/DEV/PYLOTAGE/bibpyt/Templates/salomeScript.py'

   root_astest  = ''
   salomeScript = ''

   # ---------------------------------------------------------
   # ---------------------------------------------------------
   # Essaye de detecter le repertoire d'Aster 
   lst_aster = ['/aster', '/local00/aster', '/home/aster']
   lst_vers  = ['NEW11', 'NEW10', 'NEW9', 'STA11.3', 'STA11.2', 'STA11.1', 'STA10.3', 'STA10.2', 'STA10.1']


   # Essaye de detecter le script Salome
   if not salomeScript or not os.path.isfile(salomeScript):
      salomeScript = False
      for rep in lst_aster:
         for vers in lst_vers:
            p = os.path.join(rep, vers)
            script = os.path.join(p, 'bibpyt', 'Templates', 'salomeScript.py')
            if os.path.isfile(script): 
               salomeScript = script
               root_astest  = os.path.join(p, 'astest')
               print "Script Salome detecte : %s" % salomeScript
               break


   # Essaye de detecter le repertoire astest
   if not root_astest or not os.path.isdir(root_astest):
      root_astest  = False
      for rep in lst_aster:
         for vers in lst_vers:
            p = os.path.join(rep, vers)
            astest  = os.path.join(p, 'astest')
            if os.path.isdir(astest):
               root_astest  = astest
               print "Repertoire astest detecte : %s" % root_astest
               break


   if not root_astest:
      raise Exception("Impossible de detecter le repertoire Aster. Remplir la variable root_astest.")

   if not salomeScript:
      raise Exception("Impossible de detecter le script Salome. Remplir la variable salomeScript.")


   # Les fichiers de donnes sont des fichiers du test zzzz141a
   prefix = os.path.join(root_astest, 'zzzz141a.')


   # ---------------------------------------------------------
   # ---------------------------------------------------------
   # Tests unitaires

   # Detection des etudes Salome
   script = os.path.join( os.path.dirname(salomeScript), 'salomeGetStudies.py' )
   tmpfile = MakeTempScript( SALOMESCRIPT=script )
   execfile(r"%s" % tmpfile)
   DelTempScript(tmpfile)


   CHOIX = 'DEPL'
   INPUTFILE = prefix + '91'  # Stanley_DEPL.rmed
   tmpfile = MakeTempScript( SALOMESCRIPT=salomeScript, CHOIX=CHOIX, INPUTFILE=INPUTFILE, OUTPUTFILE=False, STUDY=False )
   execfile(r"%s" % tmpfile)
   DelTempScript(tmpfile)


   CHOIX = 'GAUSS'
   INPUTFILE = prefix + '92'  # Stanley_ELGA.rmed
   tmpfile = MakeTempScript( SALOMESCRIPT=salomeScript, CHOIX=CHOIX, INPUTFILE=INPUTFILE, OUTPUTFILE=False, STUDY=False )
   execfile(r"%s" % tmpfile)
   DelTempScript(tmpfile)


   CHOIX = 'ISO'
   INPUTFILE = prefix + '93'  # Stanley_NOEU.rmed
   tmpfile = MakeTempScript( SALOMESCRIPT=salomeScript, CHOIX=CHOIX, INPUTFILE=INPUTFILE, OUTPUTFILE=False, STUDY=False )
   execfile(r"%s" % tmpfile)
   DelTempScript(tmpfile)


   CHOIX = 'ISO'
   INPUTFILE = prefix + '94'  # Stanley_ELNO.rmed
   tmpfile = MakeTempScript( SALOMESCRIPT=salomeScript, CHOIX=CHOIX, INPUTFILE=INPUTFILE, OUTPUTFILE=False, STUDY=False )
   execfile(r"%s" % tmpfile)
   DelTempScript(tmpfile)


   CHOIX = 'COURBE'
   INPUTFILE = prefix + '95'  # Stanley_Table.txt
   tmpfile = MakeTempScript( SALOMESCRIPT=salomeScript, CHOIX=CHOIX, INPUTFILE=INPUTFILE, OUTPUTFILE=False, STUDY=False )
   execfile(r"%s" % tmpfile)
   DelTempScript(tmpfile)


   CHOIX = 'ON_DEFORMED'
   INPUTFILE = prefix + '96'  # Stanley_SIGM_DEPL.rmed
   tmpfile = MakeTempScript( SALOMESCRIPT=salomeScript, CHOIX=CHOIX, INPUTFILE=INPUTFILE, OUTPUTFILE=False, STUDY=False )
   execfile(r"%s" % tmpfile)
   DelTempScript(tmpfile)
