#@ MODIF salomeScript Templates  DATE 28/03/2011   AUTEUR ASSIRE A.ASSIRE 
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

import os

# Donnees utilisateur
#
# Pour isovaleurs
# INPUTFILE1 : Fichier de resultat MED
# CHOIX = 'DEPL', 'ISO', 'GAUSS', 'ON_DEFORMED'
#
# Pour courbes
# INPUTFILE1 : Fichier de resultat TXT
# CHOIX = 'COURBE'


#%====================Choix et fichier================================%
# CHOIX = DEPL, GAUSS, ISO, COURBE
# Cette partie est modifiee automatiquement par Stanley

INPUTFILE1  = '/tmp/mon-fichier.med'
OUTPUTFILE1 = ''
STUDY       = ''
CHOIX       = 'DEPL'


#%=================================================================%

if CHOIX not in ['DEPL','ISO','GAUSS','COURBE', 'ON_DEFORMED']: raise "Erreur de type de visualisation!"
if not os.path.isfile(INPUTFILE1): raise "Fichier %s non present!" % INPUTFILE1


#%====================Initialisation Salome================================%
import salome
import SALOMEDS
import salome_kernel
orb, lcc, naming_service, cm = salome_kernel.salome_kernel_init()
obj = naming_service.Resolve('myStudyManager')
myStudyManager = obj._narrow(SALOMEDS.StudyManager)


#%====================Initialisation etude================================%
if STUDY:
    # Si on a le nom de l'etude
    study=myStudyManager.GetStudyByName(STUDY)
    salome.salome_init(study._get_StudyId())
    import visu_gui
    import VISU
    myVisu = visu_gui.myVisu
    myVisu.SetCurrentStudy(study)

else:
    # Sinon on choisit etude courante
    salome.salome_init()
    import visu_gui
    import VISU
    myVisu = visu_gui.myVisu
    myVisu.SetCurrentStudy(salome.myStudy)
    # ou la premiere detectee ?
    #Liste_Study = salome.myStudyManager.GetOpenStudies()
    #NOM = Liste_Study[0]
    #myVisu.SetCurrentStudy(salome.myStudyManager.GetStudyByName(NOM))


myViewManager = myVisu.GetViewManager()
if myViewManager is None : raise "Erreur de creation de study"


#%===================Construction courbe======================%

if CHOIX=='COURBE':
    table_txt = myVisu.ImportTables(INPUTFILE1,0)
    if table_txt :
        IsFound,aSObject = table_txt.FindSubObject(1)
        if  IsFound :
            anID = aSObject.GetID()
            table = myVisu.CreateTable(anID)
            NRow = table.GetNbRows()
            myContainer = myVisu.CreateContainer()
            for i in range(2,NRow+1):
                resu = myVisu.CreateCurve(table,1,i)
                myContainer.AddCurve(resu)

#%====================Construction isovaleurs====================%

else :
     myResult = myVisu.ImportFile(INPUTFILE1)
     if myResult is None : raise "Erreur de fichier MED"

     MAILLAGE = myResult.GetMeshNames()[0]
     LISTE_CHAMP_CELL = myResult.GetFields(MAILLAGE,VISU.CELL)
     LISTE_CHAMP_NODE = myResult.GetFields(MAILLAGE,VISU.NODE)

     resu=[]


     if CHOIX =='ON_DEFORMED' :
         if LISTE_CHAMP_NODE ==[] : raise "Erreur de champ"
         TYPE_ENTITE=VISU.NODE
         NOM_CHAMP = LISTE_CHAMP_NODE[0]
         NUM_INST = myResult.GetTimeStampNumbers(MAILLAGE,TYPE_ENTITE,NOM_CHAMP)[0]

         if LISTE_CHAMP_CELL ==[] :
            TYPE_SCAL=VISU.NODE
            NOM_CHAMP_SCAL = LISTE_CHAMP_NODE[1]
         else :
            TYPE_SCAL=VISU.CELL
            NOM_CHAMP_SCAL = LISTE_CHAMP_CELL[0]

         nCMP=myResult.GetNumberOfComponents(MAILLAGE,TYPE_SCAL,NOM_CHAMP_SCAL)
         for i in range(1,nCMP+1) :
            res = myVisu.DeformedShapeAndScalarMapOnField(myResult,MAILLAGE,TYPE_ENTITE,NOM_CHAMP,NUM_INST)
            res.SetScalarField(TYPE_SCAL,NOM_CHAMP_SCAL,NUM_INST)
            res.SetScalarMode(i)
            resu.append(res)


     if CHOIX == 'DEPL' :
         TYPE_ENTITE=VISU.NODE
         if LISTE_CHAMP_NODE ==[] : raise "Erreur de champ"
         NOM_CHAMP = LISTE_CHAMP_NODE[0]
         NUM_INST = myResult.GetTimeStampNumbers(MAILLAGE,TYPE_ENTITE,NOM_CHAMP)[0]
         res = myVisu.DeformedShapeOnField(myResult,MAILLAGE,TYPE_ENTITE,NOM_CHAMP,NUM_INST)
         res.ShowColored(True)
         nCMP=1
         resu.append(res)


     if CHOIX == 'GAUSS' :
         TYPE_ENTITE=VISU.CELL
         if LISTE_CHAMP_CELL ==[] : raise "Erreur de champ"
         NOM_CHAMP = LISTE_CHAMP_CELL[0]
         NUM_INST = myResult.GetTimeStampNumbers(MAILLAGE,TYPE_ENTITE,NOM_CHAMP)[0]
         nCMP=myResult.GetNumberOfComponents(MAILLAGE,TYPE_ENTITE,NOM_CHAMP)
         for i in range(1,nCMP+1) :
             res = myVisu.GaussPointsOnField(myResult,MAILLAGE,TYPE_ENTITE,NOM_CHAMP,NUM_INST)
             res.SetIsDispGlobalScalarBar(False)
             res.SetScalarMode(i)
             resu.append(res)


     if CHOIX == 'ISO' :
         if  LISTE_CHAMP_CELL ==[] : 
             TYPE_ENTITE = VISU.NODE
             if  LISTE_CHAMP_NODE == [] : raise "Erreur de champ"
             NOM_CHAMP = LISTE_CHAMP_NODE[0]
         else :
             TYPE_ENTITE=VISU.CELL
             NOM_CHAMP = LISTE_CHAMP_CELL[0]

         NUM_INST = myResult.GetTimeStampNumbers(MAILLAGE,TYPE_ENTITE,NOM_CHAMP)[0]
         nCMP=myResult.GetNumberOfComponents(MAILLAGE,TYPE_ENTITE,NOM_CHAMP)
         for i in range(1,nCMP+1) :
           res= myVisu.ScalarMapOnField(myResult,MAILLAGE,TYPE_ENTITE,NOM_CHAMP,NUM_INST)
           res.SetScalarMode(i)
           resu.append(res)



#%=============== Affichage ============================%

myView1 = myViewManager.GetCurrentView()
if CHOIX=='COURBE':
    myView1 = myViewManager.CreateXYPlot()
    myView1.Display(myContainer)
    session = naming_service.Resolve('/Kernel/Session')
    session.emitMessageOneWay("updateObjBrowser")
else :
    if myView1 is None :
        myView1 = myViewManager.Create3DView()
    else :
        if myView1.GetType() != VISU.TVIEW3D :
             myView1 = myViewManager.Create3DView()
             if myView1 is None : raise "Erreur de vue VTK"

    myView1.DisplayOnly(resu[nCMP-1])
    myView1.FitAll()

#%==================FIN ================================%
