#@ MODIF salomeScriptMac Templates  DATE 06/11/2012   AUTEUR BODEL C.BODEL 
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


INPUTFILE1  = '/home/bodel/resu.med'
OUTPUTFILE1 = ''
STUDY       = 'Study1'
CHOIX       = ''

import os
import sys
root=os.path.normpath(os.path.dirname(os.path.abspath(os.path.realpath(sys.argv[0]))))
INPUTFILE2 = os.path.join(root,INPUTFILE1)
if not os.path.isfile(INPUTFILE2): raise Exception("Fichier %s non present!" % INPUTFILE1)

#%====================Initialisation Salome================================%
import sys
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
    import visu
    myVisu = visu_gui.myVisu
    myVisu.SetCurrentStudy(study)

else:
    # Sinon on choisit etude courante
    salome.salome_init()
    import visu_gui
    import VISU
    import visu
    myVisu = visu_gui.myVisu
    myVisu.SetCurrentStudy(salome.myStudy)
    # ou la premiere detectee ?
    #Liste_Study = salome.myStudyManager.GetOpenStudies()
    #NOM = Liste_Study[0]
    #myVisu.SetCurrentStudy(salome.myStudyManager.GetStudyByName(NOM))


myViewManager = myVisu.GetViewManager()
if myViewManager is None : raise Exception("Erreur de creation de study")


aVisu = visu.Initialize(orb,naming_service,lcc,myStudyManager,study,0)
aSComponent = visu.PublishComponent(study)
aMed = lcc.FindOrLoadComponent('FactoryServer','MED')
aBuilder = study.NewBuilder()
aName2ObjectMap = {}


resu_med = aVisu.ImportFile(INPUTFILE2)
resu_med.SetBuildGroups(True)
resu_med.SetBuildFields(True, True)
resu_med.Build(False, True)
if resu_med.IsDone() :
  MAILLAGE = resu_med.GetMeshNames()[0]
  LISTE_CHAMP_CELL = resu_med.GetFields(MAILLAGE,VISU.CELL)
  nom_cham_cell = LISTE_CHAMP_CELL[0]
  L_INST = resu_med.GetTimeStampNumbers(MAILLAGE,VISU.CELL,nom_cham_cell)
  inst = L_INST[0]
  aVisu.RenameEntityInStudy(resu_med,nom_cham_cell, VISU.CELL, 'CARRE')
  MAILLAGE = resu_med.GetMeshNames()[0]
  Gauss_Points = aVisu.GaussPointsOnField(resu_med,MAILLAGE,VISU.CELL,nom_cham_cell,inst)

  if Gauss_Points != None:
    visu.SetName(Gauss_Points, 'MAC')
##    Gauss_Points.SetOffset(0, 0, 0)
##    Gauss_Points.SetMarkerStd(VISU.MT_POINT, VISU.MS_50)
##    Gauss_Points.SetPosition(0.1, 0.01)
##    Gauss_Points.SetSize(0.8, 0.08)
##    Gauss_Points.SetNbColors(64)
##    Gauss_Points.SetLabels(5)
##    Gauss_Points.SetScalarMode(0)
##    Gauss_Points.SetSourceRange()
##    Gauss_Points.SetIsActiveLocalScalarBar(True)
##    Gauss_Points.SetIsDispGlobalScalarBar(True)
##    Gauss_Points.SetSpacing(0.01)
##    Gauss_Points.SetBiColor(False)
##    Gauss_Points.SetIsDeformed(False)
##    Gauss_Points.SetScaleFactor(0.017544)
##    Gauss_Points.SetPrimitiveType(VISU.GaussPoints.SPRITE)
##    Gauss_Points.SetClamp(256)
##    Gauss_Points.SetAlphaThreshold(0.5)
##    Gauss_Points.SetResolution(8)
##    Gauss_Points.SetFaceLimit(50000)
##    Gauss_Points.SetIsColored(True)
    Gauss_Points.SetTitle('MAC')
    Gauss_Points.SetMinSize(0.02)
    Gauss_Points.SetMaxSize(0.3)
    Gauss_Points.SetMagnification(4)
    Gauss_Points.SetIsDispGlobalScalarBar(False)
    Gauss_Points.SetScalarMode(1)
    pass

  pass

#%=============== Affichage ============================%

# On cree une nouvelle vue a chaque fois
myView1 = myViewManager.Create3DView()
##myView1 = myViewManager.GetCurrentView()
##if myView1 is None :
##    myView1 = myViewManager.Create3DView()
##else :
##    if myView1.GetType() != VISU.TVIEW3D :
##         myView1 = myViewManager.Create3DView()
##         if myView1 is None : raise Exception("Erreur de vue VTK")

myView1.DisplayOnly(Gauss_Points)
myView1.FitAll()

#%==================FIN ================================%

##import iparameters
##ipar = iparameters.IParameters(study.GetModuleParameters("Interface Applicative", "Post-Pro", 1))
##ipar.setParameter("VISU_3:1:3:1:2:1", "VTKViewer_0_Visibility", "On")
##ipar.setParameter("VISU_3:1:3:1:2:1", "VTKViewer_0_Name", "Gauss Points:1")
##ipar.setParameter("VISU_3:1:3:1:2:1", "VTKViewer_0_RepresentationMode", "0")
##ipar.setParameter("VISU_3:1:3:1:2:1", "VTKViewer_0_Quadratic2DRepresentation", "0")
##ipar.setParameter("VISU_3:1:3:1:2:1", "VTKViewer_0_Opacity", "1")
##ipar.setParameter("VISU_3:1:3:1:2:1", "VTKViewer_0_Color", "0.5:0.5:0.5")
##ipar.setParameter("VISU_3:1:3:1:2:1", "VTKViewer_0_LineWidth", "1")
##ipar.setParameter("VISU_3:1:3:1:2:1", "VTKViewer_0_ClippingPlane", "Off")
##
##
##if salome.sg.hasDesktop():
##  salome.sg.updateObjBrowser(1)
##  iparameters.getSession().restoreVisualState(1)
