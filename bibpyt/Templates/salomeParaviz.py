#@ MODIF salomeParaviz Templates  DATE 16/04/2013   AUTEUR ASSIRE A.ASSIRE 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#
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

if CHOIX not in ['DEPL','ISO','GAUSS','COURBE', 'ON_DEFORMED']: raise Exception("Erreur de type de visualisation!")
if not os.path.isfile(INPUTFILE1): raise Exception("Fichier %s non present!" % INPUTFILE1)

#%====================Initialisation Salome================================%
import sys
import salome
import SALOMEDS
import salome_kernel
orb, lcc, naming_service, cm = salome_kernel.salome_kernel_init()
obj = naming_service.Resolve('myStudyManager')
myStudyManager = obj._narrow(SALOMEDS.StudyManager)

root=os.path.normpath(os.path.dirname(os.path.abspath(os.path.realpath(sys.argv[0]))))
INPUTFILE2 = os.path.join(root,INPUTFILE1)
if not os.path.isfile(INPUTFILE2): raise Exception("Fichier %s non present!" % INPUTFILE1)

#%====================Initialisation etude================================%
if STUDY:
    # Si on a le nom de l'etude
    study=myStudyManager.GetStudyByName(STUDY)
    salome.salome_init(study._get_StudyId())

else:
    # Sinon on choisit etude courante
    salome.salome_init()

#%=================== Initialisation paravis ============================%
try: pvsimple
except:from pvsimple import*

#%===================Construction courbe======================%

# selection de la vue adequate 
view=None
try : 
     # Cas vue active OK
     v=GetActiveView()
     if CHOIX =='COURBE' :
          if v.GetProperty('AxisBehavior'):
             view=v
     else :
          if v.GetProperty('CenterAxesVisibility'):
             view=v
     SetActiveView(view)
     L=view.Representations
     for i in L :i.Visibility = 0
     pass

except :
     try :
       # Cas plusieurs vues 
       anim = GetAnimationScene()
       liste_view = anim.ViewModules
       for v in liste_view :
         if CHOIX == 'COURBE':
            if v.GetProperty('AxisBehavior'):
               view=v
         else :
            if v.GetProperty('CenterAxesVisibility'):
               view=v
       SetActiveView(view)
       L=view.Representations
       for i in L :i.Visibility = 0
       pass     

     except :pass
if view==None and CHOIX == 'COURBE' : view = CreateXYPlotView()

if CHOIX == 'COURBE' :
     CHOIXF = 'COURBE'

     # reader Table
     myResult = TableReader(FileName=INPUTFILE2)
     if myResult is None : raise "Erreur de fichier"
     myResult.FirstStringAsTitles = 0

     display=Show()
     display.AttributeType = 'Row Data'
     display.UseIndexForXAxis = 0
     #NAME_X = display.GetProperty('SeriesNamesInfo')[0]
     # On n'arrive pas a recuperer le label de l'axe X
     NAME_X = 'ABSC_CURV'
     display.XArrayName = NAME_X
     display.UseIndexForXAxis = 0
     display.SeriesVisibility = ['vtkOriginalIndices', '0', NAME_X, '0']
     Render()

#%====================Construction isovaleurs====================%
    
if CHOIX != 'COURBE' :

     myResult = MEDReader(FileName=INPUTFILE2)
     if myResult is None : raise Exception("Erreur de fichier MED")


     # Recuperation du nom des champs
     L_POIN = myResult.GetProperty('PointFieldsArrayInfo')
     L_CELL = myResult.GetProperty('CellFieldsArrayInfo')
     L_ELNO = myResult.GetProperty('ElnoFieldsArrayInfo')
     L_ELGA = myResult.GetProperty('QuadratureFieldsArrayInfo')
     #print 'L_POIN',L_POIN,'L_CELL',L_CELL,'L_ELNO',L_ELNO,'L_ELGA',L_ELGA
     NB_CHAMP = len(L_POIN)+len(L_CELL)+len(L_ELNO)+len(L_ELGA)
     # On ne considere pas ELNO et ON_DEFORMED (comme Gauss)
     if len(L_ELNO) > 0 :
         CHOIXF ='ELNO'
     else :
         CHOIXF = CHOIX  
     #print 'NB_CHAMP=',NB_CHAMP

     # Champ variable ou non

     NB_ORDRE = len(myResult.GetPropertyValue('TimestepValues'))

if CHOIXF == 'ISO' :

     # Recuperation du champ et application filtre ELNO si necessaire
     myResult.GenerateVectors = 0

     resu = myResult
     
     if len (L_POIN) > 0 : 
          NOM_CHAMP = L_POIN[0]
          pd = resu.PointData
     if len (L_CELL) > 0 : 
          NOM_CHAMP = L_CELL[0]
          pd = resu.CellData

     # Recuperation des informations du champ
     for i in range(len(pd)):
          if pd.values()[i-1].GetName()== NOM_CHAMP :
               NB_CMP = pd.values()[i-1].GetNumberOfComponents()
               NOM_CMP = pd.values()[i-1].GetComponentName(0)
               RANGE_CMP = pd.values()[i-1].GetRange()

     # Attributs de visualisation
     CMP = 'Component'
     TYPE = 'Surface With Edges'


if CHOIXF == 'ELNO' :

     # Recuperation du champ et application filtre ELNO si necessaire
     # choix de visualiser les ELNO en ELGA
     myResult.GenerateVectors = 0
     
     resu = ELNOPoints()
     resu.SelectSourceArray = ['POINTS', 'ELNO']
     
     
     if len (L_ELNO) > 0 : 
          NOM_CHAMP = L_ELNO[0]
          pd = resu.PointData


     # Recuperation des informations du champ
     for i in range(len(pd)):
          if pd.values()[i-1].GetName()== NOM_CHAMP :
               NB_CMP = pd.values()[i-1].GetNumberOfComponents()
               NOM_CMP = pd.values()[i-1].GetComponentName(0)
               RANGE_CMP = pd.values()[i-1].GetRange()

     # Attributs de visualisation
     CMP = 'Component'     
     TYPE = 'Point Sprite'


if CHOIXF == 'GAUSS' :

     # Application filtre ELGA
     myResult.GenerateVectors = 0
     resu = GaussPoints()
     resu.SelectSourceArray = ['CELLS', 'ELGA']
     if len (L_ELGA) > 0 :
          NOM_CHAMP = L_ELGA[0]
          pd = resu.PointData

     # Recuperation des informations du champ
     for i in range(len(pd)):
          if pd.values()[i-1].GetName()== NOM_CHAMP :
               NB_CMP = pd.values()[i-1].GetNumberOfComponents()
               NOM_CMP = pd.values()[i-1].GetComponentName(0)
               RANGE_CMP = pd.values()[i-1].GetRange()
               #print NB_CMP, NOM_CMP, RANGE_CMP

     # Attributs de visualisation
     CMP = 'Component'
     TYPE = 'Point Sprite'


if CHOIXF == 'DEPL' :

     # Generation des vecteurs et filtre WarpByVector
     myResult.GenerateVectors = 1
     resu = WarpByVector()
     pd = resu.PointData

     # Recuperation des informations du champ
     for i in range(len(pd)):
          if pd.values()[i-1].GetNumberOfComponents() == 3 :
               NOM_CHAMP = pd.values()[i-1].GetName()
               NB_CMP = pd.values()[i-1].GetNumberOfComponents()
               RANGE_CMP = pd.values()[i-1].GetRange()
               #print NB_CMP, RANGE_CMP, 
               #print NOM_CHAMP

     NOM_CHAMP_DEF = NOM_CHAMP
     MAX_CMP = max(abs(RANGE_CMP[0]),abs(RANGE_CMP[1]))
     if MAX_CMP == 0. : MAX_CMP = 1.
     SCALE_FACTOR = 1. / MAX_CMP

     # Attributs de visualisation
     CMP = 'Magnitude'
     TYPE = 'Surface With Edges'
     NOM_CMP =''
     


if CHOIXF =='ON_DEFORMED' :

     # Generation des vecteurs et filtre WarpByVector
     myResult.GenerateVectors = 1
     resu = myResult
     
     #resu = WarpByVector()
     #pd = resu.PointData
     
     # Recuperation des informations du champ
     if len(L_CELL) > 0 : 
        NOM_CHAMP = L_CELL[0]
        pd = resu.CellData
     else : 
        NOM_CHAMP = L_POIN[2]
        pd = resu.PointData

     for i in range(len(pd)) :
         #print i, pd.values()[i-1].GetName(),pd.values()[i-1].GetNumberOfComponents()
         if pd.values()[i-1].GetName() == NOM_CHAMP :
             #print pd.values()[i-1].GetName(),'COLOR'
             NOM_CMP = pd.values()[i-1].GetComponentName(0)
             NB_CMP = pd.values()[i-1].GetNumberOfComponents()
             RANGE_CMP = pd.values()[i-1].GetRange()
  
     # Recuperation des informations du champ DEPL
     resu = WarpByVector()
     pd1 = resu.PointData
     for i in range(len(pd1)) :
         # On ne considere pas le champ vecteur autre que DEPL
         if NOM_CHAMP not in pd1.values()[i-1].GetName() and pd1.values()[i-1].GetNumberOfComponents() == 3 :
              NOM_CHAMP_DEF = pd1.values()[i-1].GetName()
              RANGE_CMP_DEF = pd1.values()[i-1].GetRange()       
     
     MAX_CMP = max(abs(RANGE_CMP_DEF[0]),abs(RANGE_CMP_DEF[1]))
     if MAX_CMP == 0. : MAX_CMP = 1.
     SCALE_FACTOR = 1. / MAX_CMP
     
     # Attributs de visualisation
     CMP = 'Component'
     TYPE = 'Surface With Edges'


#%=============== Affichage ============================%
     
if CHOIXF != 'COURBE' :

     # Visualisation
     if NB_ORDRE > 1 :
          anim = GetAnimationScene()
          anim.Loop=1
          #anim.PlayMode = 'Sequence'
          #anim.NumberOfFrames = 50


     if CHOIXF == 'DEPL' or CHOIXF == 'ON_DEFORMED' :
          resu.Vectors = ['POINTS', NOM_CHAMP_DEF]
          resu.ScaleFactor = SCALE_FACTOR

     display = Show()
     display.ColorArrayName = NOM_CHAMP
     display.Representation = TYPE
     CH_PVLookupTable = GetLookupTableForArray( NOM_CHAMP, NB_CMP, VectorMode = CMP, RGBPoints=[RANGE_CMP[0], 0.0, 0.0, 1.0, RANGE_CMP[1], 1.0, 0.0, 0.0], ScalarRangeInitialized=1.0)
     display.LookupTable = CH_PVLookupTable
     display.LookupTable = CH_PVLookupTable

     if CHOIXF == 'GAUSS' or CHOIXF =='ELNO' :
          display.RadiusArray = [None, NOM_CHAMP]
          display.RadiusMode = 'Scalar'
          display.RadiusScalarRange = RANGE_CMP   

     scalarbar = CreateScalarBar(Title =NOM_CHAMP, ComponentTitle = NOM_CMP, LookupTable=CH_PVLookupTable, TitleFontSize=12 , LabelFontSize=12)
     view.Representations.append(scalarbar)


     Render()
     ResetCamera()
     if NB_ORDRE > 1 :
          anim.Play()

#%==================FIN ================================%
