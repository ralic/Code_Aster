# coding=utf-8
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

INPUTFILE1 = '/tmp/mon-fichier.med'
OUTPUTFILE1 = ''
STUDY = ''
CHOIX = 'DEPL'

#%=================================================================%

if CHOIX not in ['DEPL', 'ISO', 'GAUSS', 'COURBE', 'ON_DEFORMED']:
    raise Exception("Erreur de type de visualisation!")
if not os.path.isfile(INPUTFILE1):
    raise Exception("Fichier %s non present!" % INPUTFILE1)

#%====================Initialisation Salome================================%
import re
import sys
import salome
import SALOMEDS
import salome_kernel

orb, lcc, naming_service, cm = salome_kernel.salome_kernel_init()
obj = naming_service.Resolve('myStudyManager')
myStudyManager = obj._narrow(SALOMEDS.StudyManager)

root = os.path.normpath(
    os.path.dirname(os.path.abspath(os.path.realpath(sys.argv[0]))))
INPUTFILE2 = os.path.join(root, INPUTFILE1)
if not os.path.isfile(INPUTFILE2):
    raise Exception("Fichier %s non present!" % INPUTFILE1)

#%====================Initialisation etude================================%

salome.salome_init()

#%=================== Initialisation paravis ============================%
try:
    pvsimple
except:
    from pvsimple import *

#%===================Construction courbe======================%

# selection de la vue adequate
view = None
try:
    # Cas vue active OK
    v = GetActiveView()
    if CHOIX == 'COURBE':
        if 'LineChartView' in str(type(v)):
            view = v
    else:
        if 'RenderView' in str(type(v)):
            view = v
except:
    try:
        # Cas plusieurs vues
        anim = GetAnimationScene()
        liste_view = anim.ViewModules
        for v in liste_view:
            if CHOIX == 'COURBE':
                if 'LineChartView' in str(type(v)):
                    view = v
            else:
                if 'RenderView' in str(type(v)):
                    view = v

    except:
        pass
if view == None and CHOIX == 'COURBE':
    view = CreateXYPlotView()
if view == None and CHOIX != 'COURBE':
    view = CreateRenderView()
SetActiveView(view)
L = view.Representations
for i in L:
    i.Visibility = 0


nocomment = re.compile('^[^#].*', re.M)
title = re.compile('^#COLUMN_TITLES: *(.*)$', re.M)

def convert(fname):
    """Convert in place an output file from Stanley for Paravis"""
    txt = open(fname, 'r').read()
    mat = title.search(txt)
    assert mat, "COLUMN_TITLES not found"
    label = mat.group(1).split('|')
    values = nocomment.findall(txt)
    cont = [' '.join(label)]
    cont.extend(values)
    open(fname, 'w').write(os.linesep.join(cont))


if CHOIX == 'COURBE':
    CHOIXF = 'COURBE'

    # reader Table
    convert(INPUTFILE2)
    myResult = CSVReader(FileName=INPUTFILE2)
    if myResult is None:
        raise "Erreur de fichier"
    myResult.FieldDelimiterCharacters = ' '
    myResult.MergeConsecutiveDelimiters = 1
    Render()

    courbe = PlotData()

    display = Show()
    display.AttributeType = 'Row Data'
    display.UseIndexForXAxis = 0

    labels = display.GetProperty('SeriesLabel')

    display.XArrayName = labels[0]

    display.SeriesVisibility = labels[2:]

    Render()

#%====================Construction isovaleurs====================%

if CHOIX != 'COURBE':

    myResult = MEDReader(FileName=INPUTFILE2)
    if myResult is None:
        raise Exception("Erreur de fichier MED")

    keys = myResult.GetProperty('FieldsTreeInfo')[::2]
    arrs_with_dis = [elt.split()[-1] for elt in keys]
    arrs = [elt.split(myResult.GetProperty('Separator').GetData())
            for elt in arrs_with_dis]
    id_champ = [[nom.split('/')[3], type] for nom, type in arrs]

    CHOIXF = CHOIX

    # Champ variable ou non

    NB_ORDRE = len(myResult.GetProperty('TimestepValues'))

if CHOIXF == 'ISO':

    # Recuperation du champ et application filtre ELNO si necessaire

    resu = myResult

    NOM_CHAMP = id_champ[0][0]
    TYPE_CHAMP = id_champ[0][1]
    if TYPE_CHAMP == "P0":
        pd = resu.CellData
    if TYPE_CHAMP == "P1":
        pd = resu.PointData
    if TYPE_CHAMP == "GSSNE":
        resu = ELNOMesh()
        pd = resu.PointData

    # Recuperation des informations du champ
    for i in range(len(pd)):
        if pd.values()[i - 1].GetName() == NOM_CHAMP:
            NB_CMP = pd.values()[i - 1].GetNumberOfComponents()
            NOM_CMP = pd.values()[i - 1].GetComponentName(0)
            RANGE_CMP = pd.values()[i - 1].GetRange()

    # Attributs de visualisation
    CMP = 'Component'
    TYPE = 'Surface With Edges'


if CHOIXF == 'GAUSS':

    # Application filtre ELGA

    for nom, type in id_champ:
        if type == "GAUSS":
            NOM_CHAMP = nom
            break

    resu = GaussPoints()

    nom = "ELGA@" + "0"
    resu.SelectSourceArray = ['CELLS', nom]

    pd = resu.PointData

    # Recuperation des informations du champ
    for i in range(len(pd)):
        if pd.values()[i - 1].GetName() == NOM_CHAMP:
            NB_CMP = pd.values()[i - 1].GetNumberOfComponents()
            NOM_CMP = pd.values()[i - 1].GetComponentName(0)
            RANGE_CMP = pd.values()[i - 1].GetRange()

    # Attributs de visualisation
    CMP = 'Component'
    TYPE = 'Point Sprite'


if CHOIXF == 'DEPL':

    resu = myResult

    NOM_CHAMP = id_champ[0][0]
    NOM_CHAMP_DEF = NOM_CHAMP

    # Recuperation des informations du champ DEPL
    pd = resu.PointData

    for i in range(len(pd)):
        if pd.values()[i - 1].GetName() == NOM_CHAMP:
            RANGE_CMP = pd.values()[i - 1].GetRange()
            NB_CMP = pd.values()[i - 1].GetNumberOfComponents()

    # Filtre calculator si NB_CMP different de 3

    if NB_CMP == 2:
        resu = Calculator()
        resu.Function = NOM_CHAMP_DEF + \
            "_DX*iHat+" + NOM_CHAMP_DEF + "_DY*jHat+0*kHat"
        resu.ResultArrayName = NOM_CHAMP_DEF + "_Vector"
        NOM_CHAMP_DEF = NOM_CHAMP_DEF + "_Vector"
    if NB_CMP > 3:
        resu = Calculator()
        resu.Function = NOM_CHAMP_DEF + "_DX*iHat+" + \
            NOM_CHAMP_DEF + "_DY*jHat+" + NOM_CHAMP_DEF + "_DZ*kHat"
        resu.ResultArrayName = NOM_CHAMP_DEF + "_Vector"
        NOM_CHAMP_DEF = NOM_CHAMP_DEF + "_Vector"

    # Filtre Warp by Vector
    resu = WarpByVector()
    resu.Vectors = ['POINTS', NOM_CHAMP_DEF]
    pd = resu.PointData

    MAX_CMP = max(abs(RANGE_CMP[0]), abs(RANGE_CMP[1]))
    if MAX_CMP == 0.:
        MAX_CMP = 1.
    SCALE_FACTOR = 1. / MAX_CMP

    # Attributs de visualisation
    CMP = 'Magnitude'
    TYPE = 'Surface With Edges'
    NOM_CMP = ''
    NB_CMP = 3


if CHOIXF == 'ON_DEFORMED':

    resu = myResult

    # Recuperation des informations du champ
    # Initialisation DEPL premier champ
    NOM_CHAMP = id_champ[1][0]
    TYPE_CHAMP = id_champ[1][1]
    NOM_CHAMP_DEF = id_champ[0][0]
    if TYPE_CHAMP != "P1":
        NOM_CHAMP = id_champ[0][0]
        TYPE_CHAMP = id_champ[0][1]
        NOM_CHAMP_DEF = id_champ[1][0]
    else:
        if ("DEPL" not in NOM_CHAMP_DEF) and (id_champ[1][1] == "P1"):
            NOM_CHAMP = id_champ[0][0]
            TYPE_CHAMP = id_champ[0][1]
            NOM_CHAMP_DEF = id_champ[1][0]

    # Traitement selon TYPE_CHAMP
    if TYPE_CHAMP == "P0":
        pd = resu.CellData
    if TYPE_CHAMP == "P1":
        pd = resu.PointData
    if TYPE_CHAMP == "GSSNE":
        resu = ELNOMesh()
        pd = resu.PointData

    for i in range(len(pd)):
        if pd.values()[i - 1].GetName() == NOM_CHAMP:
            NOM_CMP = pd.values()[i - 1].GetComponentName(0)
            NB_CMP = pd.values()[i - 1].GetNumberOfComponents()
            RANGE_CMP = pd.values()[i - 1].GetRange()

    # Recuperation des informations du champ DEPL
    pd1 = resu.PointData
    for i in range(len(pd1)):
        if pd1.values()[i - 1].GetName() == NOM_CHAMP_DEF:
            NB_CMP_DEF = pd1.values()[i - 1].GetNumberOfComponents()
            RANGE_CMP_DEF = pd1.values()[i - 1].GetRange()

    if NB_CMP_DEF == 2:
        resu = Calculator()
        resu.Function = NOM_CHAMP_DEF + \
            "_DX*iHat+" + NOM_CHAMP_DEF + "_DY*jHat+0*kHat"
        resu.ResultArrayName = NOM_CHAMP_DEF + "_Vector"
        NOM_CHAMP_DEF = NOM_CHAMP_DEF + "_Vector"
    if NB_CMP_DEF > 3:
        resu = Calculator()
        resu.Function = NOM_CHAMP_DEF + "_DX*iHat+" + \
            NOM_CHAMP_DEF + "_DY*jHat+" + NOM_CHAMP_DEF + "_DZ*kHat"
        resu.ResultArrayName = NOM_CHAMP_DEF + "_Vector"
        NOM_CHAMP_DEF = NOM_CHAMP_DEF + "_Vector"

    resu = WarpByVector()
    resu.Vectors = ['POINTS', NOM_CHAMP_DEF]

    MAX_CMP = max(abs(RANGE_CMP_DEF[0]), abs(RANGE_CMP_DEF[1]))
    if MAX_CMP == 0.:
        MAX_CMP = 1.
    SCALE_FACTOR = 1. / MAX_CMP

    # Attributs de visualisation
    CMP = 'Component'
    TYPE = 'Surface With Edges'


#%=============== Affichage ============================%

if CHOIXF != 'COURBE':

    # Visualisation
    if NB_ORDRE > 1:
        anim = GetAnimationScene()
        anim.Loop = 1
        # anim.PlayMode = 'Sequence'
        # anim.NumberOfFrames = 50

    if CHOIXF == 'DEPL' or CHOIXF == 'ON_DEFORMED':
        resu.ScaleFactor = SCALE_FACTOR

    display = Show()
    display.ColorArrayName = NOM_CHAMP
    display.Representation = TYPE
    if RANGE_CMP[0] == RANGE_CMP[1]:
        max_scalar = 1.01 * RANGE_CMP[1]
    else:
        max_scalar = RANGE_CMP[1]
    CH_PVLookupTable = GetLookupTableForArray(NOM_CHAMP, NB_CMP,
                                              VectorMode=CMP,
                                              RGBPoints=[
                                                  RANGE_CMP[
                                                      0], 0.0, 0.0, 1.0, max_scalar, 1.0, 0.0, 0.0],
                                              ScalarRangeInitialized=1.0)
    display.LookupTable = CH_PVLookupTable

    if CHOIXF == 'GAUSS':
        display.RadiusArray = [None, NOM_CHAMP]
        display.RadiusMode = 'Scalar'
        display.RadiusScalarRange = RANGE_CMP

    scalarbar = CreateScalarBar(Title=NOM_CHAMP, ComponentTitle=NOM_CMP,
                                LookupTable=CH_PVLookupTable, TitleFontSize=12, LabelFontSize=12)
    view.Representations.append(scalarbar)

    Render()
    ResetCamera()
    if NB_ORDRE > 1:
        anim.Play()

#%==================FIN ================================%
