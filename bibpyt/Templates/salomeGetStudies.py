#@ MODIF salomeGetStudies Templates  DATE 01/03/2011   AUTEUR ASSIRE A.ASSIRE 
# -*- coding: iso-8859-1 -*-
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

# Script permettant de recuperer les etudes ouvertes dans Salome
import salome
import SALOMEDS
import tempfile


#%====================Choix et fichier================================%
# Cette partie est modifiee par Stanley
OUTPUTFILE = '$OUTPUTFILE$'


#%====================Liste des etudes ouvertes====================%
# On liste les etudes ouvertes et on sort
Liste_Study = salome.myStudyManager.GetOpenStudies()
try:
   # Si pas de fichier de sortie, on met dans un fichier temporaire
   if OUTPUTFILE=='$OUTPUT'+'FILE$':   # hack pour eviter de substituer cette ligne
      fw = tempfile.NamedTemporaryFile(mode='w')
      OUTPUTFILE = fw.name
      fw.close()

   # On ecrit les etudes ouvertes dans un fichier
   fw = file(OUTPUTFILE,'w')
   fw.write( '\n'.join(Liste_Study) + '\n' )
except Exception, e:
   raise "Erreur : \n%s" % e

print 20*"-" + " List of open Studies " + 20*"-" + "\n" + '\n'.join(Liste_Study) + "\n" + 62*"-"
#%=================================================================%
