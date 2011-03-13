#@ MODIF salomeVisu Stanley  DATE 15/03/2011   AUTEUR ASSIRE A.ASSIRE 
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

debug = False

import os, commands, string, sys, socket, getpass
from Utilitai.Utmess import UTMESS
#from pylotage.TOOLS import *
from graphiqueTk import *
import cata_champs
cata = cata_champs.CATA_CHAMPS()

from Cata.cata import *
from Accas import _F

# Multi-langues
try:
   import gettext
   _ = gettext.gettext
except:
   def _(mesg):
      return mesg


# Type de visualisation
#ScalarMap            = 'ScalarMap'          
DeformedShape        = 'DeformedShape' 
IsoSurfaces          = 'IsoSurfaces'
CutPlanes            = 'CutPlanes'
Plot2D               = 'Plot2D'
#GaussPointsOnField   = 'GaussPointsOnField'

DeformedShapeOnField = 'DEPL'
GaussPointsOnField   = 'GAUSS'
ScalarMap            = 'ISO'          

# =========================================================================

class VISU:
    def __init__( self,  param ) :
        """
        param : dictionnaire pouvant contenir( optionnel ):
        machine_salome      : nom de la machine Salome dans laquelle on souhaite faire la VISU
        machine_salome_port : port du NS Salome
        
        remarque : 
        le service NS Salome est deduit des 2 paramètres ci-dessus.
        le service NS Salome peut egalement etre specifie dans les arguments de la ligne de commande ( ORBInitRef )
        
        """
        self.param         = param              # parametres Stanley
        self.studyName     = None               # nom de l'etude SALOME dans laquelle on fait la visualisation

        if debug: print "AA1/param=", self.param, dir(self.param)

        # Construction paramètre pour SALOME
        self.salome_host           = param['machine_salome']
        self.salome_port           = int(param['machine_salome_port'])
        self.salome_runscript      = param['machine_salome_runscript']
        self.machine_salome_login  = param['machine_salome_login']
        self.mode                  = param['mode']
        
        if debug: print "AA1/", self.salome_host, self.salome_port, self.salome_runscript, self.machine_salome_login, self.mode
        print "AA1/", self.salome_host, self.salome_port, self.salome_runscript, self.machine_salome_login, self.mode

        if self.mode == 'LOCAL' :
            self.salome_host = 'localhost'


        # selection de l'étude SALOME ( parmi celles ouvertes )
        try:
            studyList = self.__studyList( param )
        except:
            UTMESS('A','STANLEY_14')
            return 

        if studyList:
            if len( studyList ) == 1:           # une seule etude -> on publie dans celle-ci
                self.studyName = studyList[0]
            else:                               # plusieurs études -> l'utilisateur doit en selectionner une
                studyList.extend(["New"])
                self.studyName = SAISIE_MODE( studyList, _("Choix de l'etude SALOME pour visualisation"), vbar=1 )
        else:
            UTMESS('A','STANLEY_19', valk=[self.salome_host, str(self.salome_port), self.salome_runscript])
            return


    # --------------------------------------------------------------------------
    def __studyList( self, param ):
        """
        Retourne la liste des études         
        """

        result = []

        _UL=INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
        unite=_UL['UNITE_LIBRE',1]

        dSALOME = { 'CHEMIN_SCRIPT'    : './Python/Templates/salomeGetStudies.py',
                    'SALOME_HOST'      : self.salome_host,
                    'SALOME_PORT'      : self.salome_port,
                    'FICHIERS_SORTIE'  : [ './fort.%s' % unite ],
                    'SALOME_RUNAPPLI'  : self.salome_runscript,
                  }

        if self.mode == 'LOCAL':
            EXEC_LOGICIEL(CODE_RETOUR_MAXI=-1,
                          INFO=2,
                          SALOME=dSALOME
                          );               
        else:
            dMACHINE_DISTANTE = {
                    'SSH_ADRESSE'      : self.salome_host,
                    'SSH_LOGIN'        : self.machine_salome_login
                  }

            EXEC_LOGICIEL(CODE_RETOUR_MAXI=-1,
                          INFO=2,
                          SALOME=dSALOME,
                          MACHINE_DISTANTE=dMACHINE_DISTANTE
                          );               

        try:
           f=open('./fort.%s' % unite, 'r')
           result=[ study.strip() for study in f.readlines() ]
           f.close()
        except: pass
        if debug: print 'AA1/Studies list=', result
        if len(result)==0:
            UTMESS('A','STANLEY_19', valk=[self.salome_host, str(self.salome_port), self.salome_runscript])

        DEFI_FICHIER(ACTION='LIBERER',UNITE=unite)

        return result


    # --------------------------------------------------------------------------
    def Terminal_ouvert(self) :
        """
            Retourne 1 si le terminal est ouvert, 0 sinon
        """
        return 0


    # --------------------------------------------------------------------------
    def Fermer(self) :
        """
            Ferme le terminal (si necessaire)  
            Fais le menage dans l'objet
        """
        pass


    # --------------------------------------------------------------------------
    def Show( self ) :
        """
        Lance la visualisation dans SALOME
        """
        raise "Non implémentée"


# =========================================================================

class ISOVALEURS( VISU ):
    def __init__( self, fichier, param,  selection ) :        
        if not os.path.exists( fichier ):
            raise _("Fichier MED résultat de Stanley non accessible par SALOME : ") + fichier

        VISU .__init__( self,  param )

        # Si on n'a pas trouvé de session Salome ouverte on sort
        if not self.studyName: return
        if self.studyName=="New": self.studyName=''

        self.fichier       = os.path.abspath( fichier )         # chemin absolu du fichier MED fourni par Stanley        
        self.visuType      = None                               # type de visualisation
        self.selection     = selection

        # parsing fichier MED (nom maillage + nom champ)
        self.medInfo = MEDInfo( selection )

        # selection d'un type de visualisation ( parmi celles possibles )
        self.visuType = self.__visuType( self.selection )        

        if debug: print "AA3/", self.fichier, self.visuType, self.selection

        # et enfin la visualisation..
        self.Show(self.fichier, self.visuType)


    # --------------------------------------------------------------------------
    def __visuType(self, selection):
        """
        selection du type de visualisation selon le nom du champ de la selection donnée en paramètre 
        ScalarMap           
        DeformedShapeOnField
        GaussPointsOnField
        """
        nom_champ = cata[selection.nom_cham].nom
        nom_type  = cata[selection.nom_cham].type
        
        if nom_champ == 'DEPL' and selection.nom_cmp[0] == 'TOUT_CMP': 
#            result = DeformedShape
            result = DeformedShapeOnField
        elif nom_type == 'ELGA':
            result = GaussPointsOnField
        else: 
            result = ScalarMap        
        return result


    # --------------------------------------------------------------------------
    def Show(self, medFilePath, visuType) :
        """
        Lecture d'un fichier MED par le composant VISU de SALOME.

        @type     medFilePath : string
        @param  medFilePath:  chemin du fichier MED

        @type     visuType :     integer
        @param  visuType:      type de visualisation
        """

        dSALOME = { 'CHEMIN_SCRIPT'    : './Python/Templates/salomeScript.py',
                    'SALOME_HOST'      : self.salome_host,
                    'SALOME_PORT'      : self.salome_port,
                    'FICHIERS_ENTREE'  : [ medFilePath ],
                    'SALOME_RUNAPPLI'  : self.salome_runscript,
                    'NOM_PARA'         : [ 'CHOIX', 'STUDY' ],
                    'VALE'             : [ self.visuType, self.studyName ],
                  }

        if self.mode == 'LOCAL':
            EXEC_LOGICIEL(CODE_RETOUR_MAXI=-1,
                          INFO=2,
                          SALOME=dSALOME
                          );               
        else:
            dMACHINE_DISTANTE = {
                    'SSH_ADRESSE'      : self.salome_host,
                    'SSH_LOGIN'        : self.machine_salome_login
                  }

            EXEC_LOGICIEL(CODE_RETOUR_MAXI=-1,
                          INFO=2,
                          SALOME=dSALOME,
                          MACHINE_DISTANTE=dMACHINE_DISTANTE
                          );               


        if not debug:
            try:
                os.remove(medFilePath)
            except:
                pass

        UTMESS('I','STANLEY_20')



# =========================================================================

class COURBES( VISU ):
    def __init__( self,  l_courbes, param, selection, datafile ):

        VISU .__init__(self, param)

        # Si on n'a pas trouvé de session Salome ouverte on sort
        if not self.studyName: return
        if self.studyName=="New": self.studyName=''

        self.tables     = {}
        self.l_courbes  = l_courbes
        self.selection  = selection
        self.datafile   = datafile

        self.Show(self.datafile)

        
#     # --------------------------------------------------------------------------
#     def __writeSalomeTables( self, l_courbes, selection ):
#         """                
#         """
#         result =   {}
#         tabList = {}        
# 
#         try:       
#            
#             for courbe in l_courbes: 
#                 composantName   = courbe[1].split('---')[0].strip()
#                 if selection.geom[0] == 'POINT' :
#                     ordre = 1
#                 elif selection.geom[0] == 'CHEMIN' :                    
#                     ordre = courbe[1].split('=')[1].strip()
# 
#                 if not tabList.has_key(  composantName ):
#                     tabList[ composantName ] = []  # un tableau par composante
#                 tabList[ composantName ].insert( int(float(ordre)), str( courbe[0] ))
# 
#             for composantName, liste  in tabList.items():                
#                 data            = ""
#                 theTableName    = "table Stanley"                
#                 
#                 newListe =map( string.split, liste )
#                 
#                 m = [[newListe[0][i]] + [newListe[j][i+1] for j in range(len(newListe))] for i in range(0, len(newListe[0]), 2)]
#                 
#                 if selection.geom[0] == 'POINT' :
#                     theTableName +='_%s_%s_sur_%s'%( selection.nom_cham,  composantName , selection.nom_va )
#                 elif selection.geom[0] == 'CHEMIN' :                    
#                     theTableName +='_%s_%s_sur_%s'%( selection.nom_cham,  composantName, 'ABSC_CURV ' + selection.geom[1][0])
# 
#                 theTableName += ' '+selection.nom_va+' : ' + str( selection.vale_va )
# 
#                 prefix  = '#TITLE: ' + theTableName + os.linesep
#                 data += prefix  
# 
#                 for a in m :
#                     for b in a :
#                         data+=b+" "
#                     data+=os.linesep
#                 theTxtFilePathName = os.path.join( '/tmp',  composantName )
#                 if os.path.exists( theTxtFilePathName):
#                     os.remove( theTxtFilePathName )
#                 f = open( theTxtFilePathName,'w')
#                 f.write( data )                
#                 f.close()
#                 result [ theTableName ]  = theTxtFilePathName
# 
#         except: 
#             msg = str(sys.exc_info()[0]) +  str(sys.exc_info()[1]) + str(sys.exc_info()[2])
#             raise _("Erreur construction table de valeur pour visualisation 2D SALOME") + msg
# 
# 
#         return result



    # --------------------------------------------------------------------------
    def Show(self, datafile) :
        """
        Lecture d'un fichier MED par le composant VISU de SALOME.
        """        

        dSALOME = { 'CHEMIN_SCRIPT'    : './Python/Templates/salomeScript.py',
                    'SALOME_HOST'      : self.salome_host,
                    'SALOME_PORT'      : self.salome_port,
                    'FICHIERS_ENTREE'  : [ datafile ],
                    'SALOME_RUNAPPLI'  : self.salome_runscript,
                    'NOM_PARA'         : [ 'CHOIX', 'STUDY' ],
                    'VALE'             : [ 'COURBE', self.studyName ],
                  }

        if self.mode == 'LOCAL':
            EXEC_LOGICIEL(CODE_RETOUR_MAXI=-1,
                          INFO=2,
                          SALOME=dSALOME
                          );               
        else:
            dMACHINE_DISTANTE = {
                    'SSH_ADRESSE'      : self.salome_host,
                    'SSH_LOGIN'        : self.machine_salome_login
                  }

            EXEC_LOGICIEL(CODE_RETOUR_MAXI=-1,
                          INFO=2,
                          SALOME=dSALOME,
                          MACHINE_DISTANTE=dMACHINE_DISTANTE
                          );               

        if not debug:
            try:
                os.remove(datafile)
            except:
                pass



# =========================================================================

class MEDInfo:
    """
    """
    def __init__( self, selection ):
        """
        Recupère les informations( nom maillage, champ ) du fichier MED resultat en sortie de Stanley
        """
        self.name           = selection.contexte.maillage.nom
        self.fieldName      = selection.contexte.resultat.nom
        tailSize            = 8 - len( self.fieldName)
        if tailSize > 0:
            self.fieldName += tailSize * '_'
        self.fieldName     += selection.nom_cham
        
        tailSize            = 32 - len( self.fieldName )        
        if tailSize > 0:
            self.fieldName += tailSize * '_'
        
