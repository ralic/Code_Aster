#@ MODIF salomeVisuPylo Stanley  DATE 23/05/2011   AUTEUR ASSIRE A.ASSIRE 
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
from pylotage.TOOLS import *
from graphiqueTk import *
import cata_champs
cata = cata_champs.CATA_CHAMPS()

# Multi-langues
try:
   import gettext
   _ = gettext.gettext
except:
   def _(mesg):
      return mesg


# Type de visualisation
ScalarMap           = 'ScalarMap'          
DeformedShape       = 'DeformedShape' 
IsoSurfaces         = 'IsoSurfaces'
CutPlanes           = 'CutPlanes'
Plot2D              = 'Plot2D'
GaussPointsOnField  = 'GaussPointsOnField'


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
        self.salomeParam   = ''                 # parametre SALOME pour composant de pylotage
        self.studyName     = None               # nom de l'etude SALOME dans laquelle on fait la visualisation
        
        # Construction paramètre pour SALOME
        self.salomeParam = self.__salomeParam( param )
                                
        if not self.salomeParam:
            raise _("Erreur VISU Salome")
            
        # selection de l'étude SALOME ( parmi celles ouvertes )
        try:
           studyList = self.__studyList( self.salomeParam )
        except:
           UTMESS('A','STANLEY_14')
           return 

        if studyList:
            if len( studyList ) == 1:   # une seule etude -> on publie ds celle-ci
                self.studyName = studyList[0]
            else:                               # plusieurs études -> l'utilisateur doit en sélectionner une
                self.studyName = SAISIE_MODE( studyList, _("Choix de l'etude SALOME pour visualisation"), vbar=1 )

        if self.studyName:
            self.salomeParam['studyName']  = self.studyName


    def __salomeParam( self, param ):
        """
        Construit les paramètres salome à partir des paramètres Stanley
        retourne {} si incorrect
        """
        result = {}
        try:                
            if param['mode']   == 'LOCAL':
               lst = ['tmp']
               try:
                  amachineName = param[ 'machine_salome' ]
               except:
                  UTMESS('A','STANLEY_15')
                  return {}

               result[ 'machineName']  = amachineName
            elif param['mode'] == 'DISTANT':
               lst = ['machine_salome', 'tmp']
               key = 'machine_salome'
               amachineName = param[ key ]
               result[ 'machineName']  = amachineName
            else:
                raise _("Erreur MODE non implémenté, choix possible : LOCAL, DISTANT")

            # Verifications
            for var in lst:
                if not param[var].strip():
                    UTMESS('A','STANLEY_16', valk=[var])
                    return {}

            # Construction du paramètre 'ORBInitRef'
            key = 'machine_salome_port'
            nsPort = param[ key ]
            if not nsPort:
                UTMESS('A','STANLEY_17')
                return {}                    
            aORBInitRef     = 'NameService=corbaname::%s:%s' %(  amachineName, nsPort  )
            result[ 'ORBInitRef' ]  = aORBInitRef

        except KeyError:            
            UTMESS('A','STANLEY_16',valk=[key])
            return {}
        except:            
            return {}

        return result
            
    def __studyList( self, salomeParam ):
        """
        Retourne la liste des études         
        """
        result = []
        stdyMnger = Study.StudyManager( **salomeParam)
        result = stdyMnger.getOpenStudies()
                
        if not result:
            study = stdyMnger.getOrCreateStudy('Stanley')            
            if study:
                result = ['Stanley']
        return result
        
    def Terminal_ouvert(self) :
        """
            Retourne 1 si le terminal est ouvert, 0 sinon
        """
        return 0
        
    def Fermer(self) :
        """
            Ferme le terminal (si necessaire)  
            Fais le menage dans l'objet
        """
        pass
        
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

##        print 'CS_pbruno ISOVALEURS salomeParam ->',self.salomeParam
        self.fichier       = os.path.abspath( fichier )         #chemin absolu du fichier MED fourni par Stanley        
        self.visuType      = None                               #type de visualisation
        self.entityType    = None                               #type d'entité
        self.selection     = selection

        # selon mode recopie sur le poste utilisateur de fichiers si nécessaire
        if   self.param['mode'] == 'LOCAL' :
                self.__init_local( )
        elif self.param['mode'] == 'DISTANT' :
                self.__init_distant( )
        elif self.param['mode'] == 'WINDOWS' :
                self.__init_windows( )
        else:
                raise _("Erreur MODE non implémenté, choix possible : LOCAL, DISTANT, WINDOWS")
        
        # parsing fichier MED( nom maillage + nom champ + nb iteration )
        self.medInfo = MEDInfo( selection )
        self.medInfo.iteration = 1 # CS_pbruno à finir implémenter ( on affiche que le 1er instant )

        if not self.medInfo.name or not self.medInfo.fieldName or not self.medInfo.iteration:            
            raise 'Erreur lecture fichier MED : %s \n Nom maillage : %s, Nom champs %s '%( self.fichier,  self.medInfo.name, self.medInfo.fieldName )

        # selection d'un type de visualisation ( parmi celles possibles )
        self.visuType = self.__visuType( self.selection )        

        # sur quel entité ( VISU.NODE, VISU.EDGE, VISU.FACE,  VISU.CELL )
        self.entityType = self.__entityType( self.selection )

        # et enfin la visualisation..
        self.Show()


    def __init_local( self ):
        """
        Stanley fonctionne sur le poste local de l'utilisateur
        """        
        try:    os.rename( self.fichier, self.fichier  + '.pos' )
        except: pass
        self.fichier += '.pos'



    def __init_distant( self ):
        """
        Stanley fonctionne sur une machine distante de l'utilisateur.
        Il faut rapatrier le fichier sur la machine contenant salome.
        """
        result = False
        fichier = os.path.basename( self.fichier )

        mdis        = self.param['machine_salome']                                   # machine
        fdis        = self.param['tmp'] + '/' + fichier + '.pos'                     # /tmp/fort.33.pos
        fmdis       = self.param['machine_salome_login'] + '@' + mdis + ":" + fdis   # user@machine:/tmp/fort.33.pos

        # Protocole de recopie et d'execution distante
        copie     = self.param['protocole'].split('/')[0]                            # rcp ou scp

        # Copie du fichier
        cmd = copie + " " + fichier + " " + fmdis
        UTMESS('I','STANLEY_9',valk=[cmd])
        code, output = commands.getstatusoutput( cmd )
        if code!=0: 
            raise _("Erreur exécution commande : ") + cmd

        self.fichier = fdis


    def __init_windows( self ):
        """
        mode WINDOWS
        """
        UTMESS('A','STANLEY_18')
        raise _("Arret sur erreur")
        
    
    def __entityType( self, selection ):
        """
        Visu.NODE, Visu.EDGE, Visu.FACE,  Visu.CELL 
        """
        result = None
        type_champ = cata[selection.nom_cham].type

        if type_champ == 'ELGA' or type_champ == 'ELNO' or type_champ == 'ELEM':
            result = Visu.CELL 
        elif type_champ == 'NOEU':
            result = Visu.NODE
        else:
            raise _("type de champs non reconnu")
        return result
            
            
    def __visuType( self, selection):
        """
        selection du type de visualisation selon le nom du champ de la selection donnée en paramètre 
        ScalarMap           
        DeformedShape
        """
        nom_champ = cata[selection.nom_cham].nom
        nom_type  = cata[selection.nom_cham].type
        
        if nom_champ == 'DEPL' and selection.nom_cmp[0] == 'TOUT_CMP': 
            result = DeformedShape
        elif nom_type == 'ELGA':
            result = GaussPointsOnField
        else: 
            result = ScalarMap        
        #result = [ ScalarMap, DeformedShape, IsoSurfaces, CutPlanes,  Plot2D]
        return result
        
    def Show( self ) :
        """
        Lance la visualisation dans SALOME
        """
        self.__Show( self.fichier, self.medInfo, self.entityType, self.visuType, self.salomeParam )
        

    def __Show( self, medFilePath, medInfo, entity, visuType, salomeParam ) :
        """
        Lecture d'un fichier MED par le composant VISU de SALOME.

        @type     medFilePath : string
        @param  medFilePath:  chemin du fichier MED. Le chemin medFileName doit etre sur la meme machine que le composant VISU

        @type     medInfo:  class 
        @param  medInfo:  informations sur le fichier MED ( meshName, fieldName, iteration )

        @type     entity: object ?
        @param  entity:  type d'entité

        @type     visuType :     integer
        @param  visuType:      type de visualisation

        @type     salomeParam:  dictionary.
        @param  salomeParam:  parametre Salome pour initialistion composant VISU de pylotage
        """

        try:
          salomeVisu = Visu.Visu( **salomeParam )
        except Exception, e:
          UTMESS('A','STANLEY_19',valk=[salomeParam['machineName']])
          raise _("Erreur lors de la visualisation.\n\nParametres Salome :%s\n\nErreur :\n%s\n\n" % (salomeParam,e))


        ok = salomeVisu.readMED( medFilePath, medInfo.name, entity )
        if not ok:
            raise _("Erreur lecture fichier MED :") + medFilePath

        # title: nom du champ
        title = cata[self.selection.nom_cham].nom 

        if visuType == ScalarMap: # CS_pbruno :attention par defaut on trace le module du champs 
            ok = salomeVisu.ScalarMap( medInfo.fieldName, medInfo.iteration, title)
        elif visuType== DeformedShape:
            ok = salomeVisu.DeformedShape( medInfo.fieldName, medInfo.iteration, title)
        elif visuType== IsoSurfaces:
            ok = salomeVisu.IsoSurfaces( medInfo.fieldName, medInfo.iteration, title)
        elif visuType== CutPlanes:            
            planePositions =[-14.97,-10, -5.6, -4.8, -4,-3.2,-2.4,-1.6,-0.8, 0.0]
            ok = salomeVisu.DisplayCutPlanes( medInfo.fieldName, medInfo.iteration,  title, Visu.XY, 0.,0., planePositions )
        elif visuType== GaussPointsOnField:
            ok = salomeVisu.GaussPointsOnField( medInfo.fieldName, medInfo.iteration, title)
        else:
            raise _("Erreur type de visualisation non supporté")

        if not ok:
            raise _("Erreur visualisation dans SALOME")

        UTMESS('I','STANLEY_20')


# =========================================================================

class COURBES( VISU ):
    def __init__( self,  l_courbes, param, selection ):

        VISU .__init__( self, param)                

        # Si on n'a pas trouvé de session Salome ouverte on sort
        if not self.studyName: return

        self.tables     = {}
        self.l_courbes  = l_courbes
        self.selection  = selection

        # selon mode recopie sur le poste utilisateur de fichiers si nécessaire
        if   self.param['mode'] == 'LOCAL' :
                self.__init_local( )
        elif self.param['mode'] == 'DISTANT' :
                self.__init_distant( )
        elif self.param['mode'] == 'WINDOWS' :
                self.__init_windows( )
        else:
                raise _("Erreur MODE non implémenté, choix possible : LOCAL, DISTANT, WINDOWS")

        self.Show()

    def __init_local( self ):
        """
        Stanley fonctionne sur le poste local de l'utilisateur
        """        
        pass


    def __init_distant( self ):
        """
        Stanley fonctionne sur une machine distante de l'utilisateur.
        Il faut rapatrier le fichier sur la machine contenant salome.
        """
        pass


    def __init_windows( self ):
        """
        mode WINDOWS
        """
        UTMESS('A','STANLEY_18')
        raise _("Arret sur erreur")

        
    def __writeSalomeTables( self, l_courbes, selection ):
        """                
        """
        result =   {}
        tabList = {}        
                
        try:       
           
            for courbe in l_courbes: 
                composantName   = courbe[1].split('---')[0].strip()
                if selection.geom[0] == 'POINT' :
                    ordre = 1
                elif selection.geom[0] == 'CHEMIN' :                    
                    ordre = courbe[1].split('=')[1].strip()

                if not tabList.has_key(  composantName ):
                    tabList[ composantName ] = []  # un tableau par composante
                tabList[ composantName ].insert( int(float(ordre)), str( courbe[0] ))
            
            for composantName, liste  in tabList.items():                
                data            = ""
                theTableName    = "table Stanley"                
                
                newListe =map( string.split, liste )
                
                m = [[newListe[0][i]] + [newListe[j][i+1] for j in range(len(newListe))] for i in range(0, len(newListe[0]), 2)]
                
                if selection.geom[0] == 'POINT' :
                    theTableName +='_%s_%s_sur_%s'%( selection.nom_cham,  composantName , selection.nom_va )
                elif selection.geom[0] == 'CHEMIN' :                    
                    theTableName +='_%s_%s_sur_%s'%( selection.nom_cham,  composantName, 'ABSC_CURV ' + selection.geom[1][0])
                    
                theTableName += ' '+selection.nom_va+' : ' + str( selection.vale_va )

                prefix  = '#TITLE: ' + theTableName + os.linesep
                data += prefix  

                for a in m :
                    for b in a :
                        data+=b+" "
                    data+=os.linesep
                theTxtFilePathName = os.path.join( '/tmp',  composantName )
                if os.path.exists( theTxtFilePathName):
                    os.remove( theTxtFilePathName )
                f = open( theTxtFilePathName,'w')
                f.write( data )                
                f.close()
                result [ theTableName ]  = theTxtFilePathName

        except: 
            msg = str(sys.exc_info()[0]) +  str(sys.exc_info()[1]) + str(sys.exc_info()[2])
            raise _("Erreur construction table de valeur pour visualisation 2D SALOME") + msg


        return result
        

    def __Show( self, tables, salomeParam ) :
        """
        Lecture d'un fichier MED par le composant VISU de SALOME.
        
        @type   tables: dictionary
        @param  tables: dictionnaire key= nom de la table, value = chemin du fichier text contenant la table
        
        @type     salomeParam:  dictionary.
        @param  salomeParam:  parametre Salome pour initialistion composant VISU de pylotage
        """        
        salomeVisu = Visu.Visu(  **salomeParam )
        
        if self.selection.geom[0] == 'POINT' :
            tableTitle ='_%s_%s_sur_%s'%( self.selection.nom_cham, self.selection.nom_cmp, self.selection.nom_va )
        elif self.selection.geom[0] == 'CHEMIN' :                    
            tableTitle ='_%s_%s_sur_%s'%( self.selection.nom_cham, self.selection.nom_cmp, 'ABSC_CURV ' + self.selection.geom[1][0])
        tableTitle += ' '+self.selection.nom_va+' : ' + str( self.selection.vale_va )
        ok = salomeVisu.XYPlot2( self.l_courbes, tableTitle )
        if not ok:
            raise 'erreur visualisation PLOT2D dans SALOME'                
                
                

    def Show( self ) :
        """
        Lance la visualisation dans SALOME
        """
        self.__Show( self.tables, self.salomeParam )


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
        
