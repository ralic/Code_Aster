# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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

from cataelem.Tools.base_objects import MeshType, Elrefe, objects_from_context

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#      ATTENTION   ATTENTION   ATTENTION   ATTENTION   ATTENTION       %
#                                                                      %
#   IL NE FAUT PAS MODIFIER LE NOMBRE OU L'ORDRE DES TYPE_MAILLE       %
#   SANS METTRE A JOUR LES ROUTINES LRMTYP, CMLQLQ, IRMMMA, IRMMFA,    %
#   OP0150, LRMMMA, LRMMFA, LRMMDI, LRMHDF, LRCAME, IRMHDF, IRCMPR,    %
#   IRCMPE, IRCAME                                                     %
#                                                                      %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   


#------------------------------------------------------------
POI1 = MeshType(nbno=1, dim=0, code='POI')

PO1 = Elrefe()
PO1.addLocation('NOEU', 1)
PO1.addLocation('NOEU_S', 1)
PO1.addLocation('FPG1', 1)
POI1.addElrefe(PO1)


#------------------------------------------------------------
SEG2 = MeshType(nbno=2, dim=1, code='SE2')

SE2 = Elrefe()
SE2.addLocation('NOEU', 2)
SE2.addLocation('NOEU_S', 2)
SE2.addLocation('FPG1', 1)
SE2.addLocation('FPG2', 2)
SE2.addLocation('FPG2NOS', 4)
SE2.addLocation('FPG3', 3)
SE2.addLocation('FPG3NOS', 5)
SE2.addLocation('FPG4', 4)
SE2.addLocation('SIMP', 3)
SE2.addLocation('SIMP1', 5)
SE2.addLocation('COTES', 4)
SE2.addLocation('COTES1', 5)
SE2.addLocation('COTES2', 10)
SEG2.addElrefe(SE2)

CABPOU = Elrefe()
CABPOU.addLocation('FPG1', 1)
SEG2.addElrefe(CABPOU)

THCOSE2 = Elrefe()
THCOSE2.addLocation('FPG1', 3)
SEG2.addElrefe(THCOSE2)


#------------------------------------------------------------
SEG22 = MeshType(nbno=4, dim=1, code='S22')


#------------------------------------------------------------
SEG3 = MeshType(nbno=3, dim=1, code='SE3')

SE3 = Elrefe()
SE3.addLocation('NOEU', 3)
SE3.addLocation('NOEU_S', 2)
SE3.addLocation('FPG1', 1)
SE3.addLocation('FPG2', 2)
SE3.addLocation('FPG2NOS', 4)
SE3.addLocation('FPG3', 3)
SE3.addLocation('FPG3NOS', 5)
SE3.addLocation('FPG4', 4)
SE3.addLocation('SIMP', 3)
SE3.addLocation('SIMP1', 5)
SE3.addLocation('COTES', 4)
SE3.addLocation('COTES1', 5)
SE3.addLocation('COTES2', 10)
SEG3.addElrefe(SE3)

THCOSE3 = Elrefe()
THCOSE3.addLocation('FPG1', 3)
SEG3.addElrefe(THCOSE3)


#------------------------------------------------------------
SEG33 = MeshType(nbno=6, dim=1, code='S33')


#------------------------------------------------------------
SEG4 = MeshType(nbno=4, dim=1, code='SE4')

SE4 = Elrefe()
SE4.addLocation('NOEU', 4)
SE4.addLocation('NOEU_S', 2)
SE4.addLocation('FPG1', 1)
SE4.addLocation('FPG2', 2)
SE4.addLocation('FPG3', 3)
SE4.addLocation('FPG4', 4)
SEG4.addElrefe(SE4)


#------------------------------------------------------------
TRIA3 = MeshType(nbno=3, dim=2, code='TR3')

TR3 = Elrefe()
TR3.addLocation('NOEU', 3)
TR3.addLocation('NOEU_S', 3)
TR3.addLocation('FPG1', 1)
TR3.addLocation('FPG3', 3)
TR3.addLocation('FPG3NOS', 6)
TR3.addLocation('COT3', 3)
TR3.addLocation('FPG4', 4)
TR3.addLocation('FPG6', 6)
TR3.addLocation('FPG7', 7)
TR3.addLocation('FPG12', 12)
TR3.addLocation('FPG13', 13)
TR3.addLocation('FPG16', 16)
TR3.addLocation('XFEM36', 36)
TR3.addLocation('SIMP', 6)
TRIA3.addElrefe(TR3)


#------------------------------------------------------------
TRIA33 = MeshType(nbno=6, dim=2, code='T33')


#------------------------------------------------------------
TRIA6 = MeshType(nbno=6, dim=2, code='TR6')

TR6 = Elrefe()
TR6.addLocation('NOEU', 6)
TR6.addLocation('NOEU_S', 3)
TR6.addLocation('FPG1', 1)
TR6.addLocation('FPG3', 3)
TR6.addLocation('FPG3NOS', 6)
TR6.addLocation('FPG4', 4)
TR6.addLocation('FPG6', 6)
TR6.addLocation('FPG7', 7)
TR6.addLocation('FPG12', 12)
TR6.addLocation('FPG13', 13)
TR6.addLocation('FPG16', 16)
TR6.addLocation('XFEM72', 72)
TR6.addLocation('SIMP', 6)
TRIA6.addElrefe(TR6)


#------------------------------------------------------------
TRIA66 = MeshType(nbno=12, dim=2, code='T66')


#------------------------------------------------------------
TRIA7 = MeshType(nbno=7, dim=2, code='TR7')

MEC3TR7H = Elrefe()
MEC3TR7H.addLocation('FPG1', 7)
TRIA7.addElrefe(MEC3TR7H)

TR7 = Elrefe()
TR7.addLocation('NOEU', 7)
TR7.addLocation('NOEU_S', 3)
TR7.addLocation('FPG1', 1)
TR7.addLocation('FPG3', 3)
TR7.addLocation('FPG4', 4)
TR7.addLocation('FPG6', 6)
TR7.addLocation('FPG7', 7)
TR7.addLocation('FPG13', 13)
TR7.addLocation('FPG16', 16)
TRIA7.addElrefe(TR7)


#------------------------------------------------------------
QUAD4 = MeshType(nbno=4, dim=2, code='QU4')

QU4 = Elrefe()
QU4.addLocation('NOEU', 4)
QU4.addLocation('NOEU_S', 4)
QU4.addLocation('FPG1', 1)
QU4.addLocation('FIS2', 2)
QU4.addLocation('FPG4', 4)
QU4.addLocation('FPG4NOS', 8)
QU4.addLocation('FPG9', 9)
QU4.addLocation('FPG9COQ', 9)
QU4.addLocation('XFEM72', 72)
QU4.addLocation('SIMP', 9)
QUAD4.addElrefe(QU4)


#------------------------------------------------------------
QUAD44 = MeshType(nbno=8, dim=2, code='Q44')


#------------------------------------------------------------
QUAD8 = MeshType(nbno=8, dim=2, code='QU8')

QU8 = Elrefe()
QU8.addLocation('NOEU', 8)
QU8.addLocation('NOEU_S', 4)
QU8.addLocation('FPG1', 1)
QU8.addLocation('FPG4', 4)
QU8.addLocation('FPG4NOS', 8)
QU8.addLocation('FPG9', 9)
QU8.addLocation('FPG9COQ', 9)
QU8.addLocation('XFEM144', 144)
QUAD8.addElrefe(QU8)


#------------------------------------------------------------
QUAD88 = MeshType(nbno=16, dim=2, code='Q88')


#------------------------------------------------------------
QUAD9 = MeshType(nbno=9, dim=2, code='QU9')

QU9 = Elrefe()
QU9.addLocation('NOEU', 9)
QU9.addLocation('NOEU_S', 4)
QU9.addLocation('FPG1', 1)
QU9.addLocation('FPG4', 4)
QU9.addLocation('FPG5', 5)
QU9.addLocation('FPG9', 9)
QU9.addLocation('FPG9COQ', 9)
QUAD9.addElrefe(QU9)

MEC3QU9H = Elrefe()
MEC3QU9H.addLocation('FPG1', 9)
QUAD9.addElrefe(MEC3QU9H)


#------------------------------------------------------------
QUAD99 = MeshType(nbno=18, dim=2, code='Q99')


#------------------------------------------------------------
TETRA4 = MeshType(nbno=4, dim=3, code='TE4')

TE4 = Elrefe()
TE4.addLocation('NOEU', 4)
TE4.addLocation('NOEU_S', 4)
TE4.addLocation('FPG1', 1)
TE4.addLocation('FPG4', 4)
TE4.addLocation('FPG4NOS', 8)
TE4.addLocation('FPG5', 5)
TE4.addLocation('FPG15', 15)
TE4.addLocation('XFEM90', 90)
TE4.addLocation('XFEM180', 180)
TETRA4.addElrefe(TE4)


#------------------------------------------------------------
TETRA10 = MeshType(nbno=10, dim=3, code='T10')

T10 = Elrefe()
T10.addLocation('NOEU', 10)
T10.addLocation('NOEU_S', 4)
T10.addLocation('FPG1', 1)
T10.addLocation('FPG4', 4)
T10.addLocation('FPG4NOS', 8)
T10.addLocation('FPG5', 5)
T10.addLocation('FPG15', 15)
T10.addLocation('XFEM90', 90)
TETRA10.addElrefe(T10)


#------------------------------------------------------------
PENTA6 = MeshType(nbno=6, dim=3, code='PE6')

PE6 = Elrefe()
PE6.addLocation('NOEU', 6)
PE6.addLocation('NOEU_S', 6)
PE6.addLocation('FPG1', 1)
PE6.addLocation('FPG6', 6)
PE6.addLocation('FPG6NOS', 12)
PE6.addLocation('FPG8', 8)
PE6.addLocation('FPG21', 21)
PE6.addLocation('XFEM72', 72)
PE6.addLocation('XFEM240', 240)
PE6.addLocation('XFEM480', 480)
PE6.addLocation('SHB6', 5)
PENTA6.addElrefe(PE6)


#------------------------------------------------------------
PENTA15 = MeshType(nbno=15, dim=3, code='P15')

P15 = Elrefe()
P15.addLocation('NOEU', 15)
P15.addLocation('NOEU_S', 6)
P15.addLocation('FPG1', 1)
P15.addLocation('FPG6', 6)
P15.addLocation('FPG6NOS', 12)
P15.addLocation('FPG8', 8)
P15.addLocation('FPG21', 21)
P15.addLocation('XFEM240', 240)
P15.addLocation('SHB15', 15)
PENTA15.addElrefe(P15)


#------------------------------------------------------------
PENTA18 = MeshType(nbno=18, dim=3, code='P18')

P18 = Elrefe()
P18.addLocation('NOEU', 18)
P18.addLocation('NOEU_S', 6)
P18.addLocation('FPG1', 1)
P18.addLocation('FPG6', 6)
P18.addLocation('FPG6NOS', 12)
P18.addLocation('FPG8', 8)
P18.addLocation('FPG21', 21)
PENTA18.addElrefe(P18)


#------------------------------------------------------------
PYRAM5 = MeshType(nbno=5, dim=3, code='PY5')

PY5 = Elrefe()
PY5.addLocation('NOEU', 5)
PY5.addLocation('NOEU_S', 5)
PY5.addLocation('FPG1', 1)
PY5.addLocation('FPG5', 5)
PY5.addLocation('FPG6', 6)
PY5.addLocation('FPG27', 27)
PY5.addLocation('XFEM180', 180)
PY5.addLocation('XFEM360', 360)
PYRAM5.addElrefe(PY5)


#------------------------------------------------------------
PYRAM13 = MeshType(nbno=13, dim=3, code='P13')

P13 = Elrefe()
P13.addLocation('NOEU', 13)
P13.addLocation('NOEU_S', 5)
P13.addLocation('FPG1', 1)
P13.addLocation('FPG5', 5)
P13.addLocation('FPG6', 6)
P13.addLocation('FPG27', 27)
P13.addLocation('XFEM180', 180)
PYRAM13.addElrefe(P13)


#------------------------------------------------------------
HEXA8 = MeshType(nbno=8, dim=3, code='HE8')

HE8 = Elrefe()
HE8.addLocation('NOEU', 8)
HE8.addLocation('NOEU_S', 8)
HE8.addLocation('FPG1', 1)
HE8.addLocation('FPG8', 8)
HE8.addLocation('FPG8NOS', 16)
HE8.addLocation('FPG27', 27)
HE8.addLocation('FPG64', 64)
HE8.addLocation('SHB5', 5)
HE8.addLocation('XFEM480', 480)
HE8.addLocation('XFEM960', 960)
HEXA8.addElrefe(HE8)

POHOH8 = Elrefe()
POHOH8.addLocation('FPG8', 8)
HEXA8.addElrefe(POHOH8)


#------------------------------------------------------------
HEXA20 = MeshType(nbno=20, dim=3, code='H20')

H20 = Elrefe()
H20.addLocation('NOEU', 20)
H20.addLocation('NOEU_S', 8)
H20.addLocation('FPG1', 1)
H20.addLocation('FPG8', 8)
H20.addLocation('FPG8NOS', 16)
H20.addLocation('FPG27', 27)
H20.addLocation('FPG64', 64)
H20.addLocation('XFEM480', 480)
H20.addLocation('SHB20', 20)
HEXA20.addElrefe(H20)

POHOH20 = Elrefe()
POHOH20.addLocation('FPG27', 27)
HEXA20.addElrefe(POHOH20)


#------------------------------------------------------------
HEXA27 = MeshType(nbno=27, dim=3, code='H27')

H27 = Elrefe()
H27.addLocation('NOEU', 27)
H27.addLocation('NOEU_S', 8)
H27.addLocation('FPG1', 1)
H27.addLocation('FPG8', 8)
H27.addLocation('FPG7', 7)
H27.addLocation('FPG27', 27)
H27.addLocation('FPG64', 64)
HEXA27.addElrefe(H27)


#------------------------------------------------------------
TR3QU4 = MeshType(nbno=7, dim=2, code='TQ7')


#------------------------------------------------------------
QU4TR3 = MeshType(nbno=7, dim=2, code='QT7')


#------------------------------------------------------------
TR6TR3 = MeshType(nbno=9, dim=2, code='TT1')


#------------------------------------------------------------
TR3TR6 = MeshType(nbno=9, dim=2, code='TT2')


#------------------------------------------------------------
TR6QU4 = MeshType(nbno=10, dim=2, code='TQ1')


#------------------------------------------------------------
QU4TR6 = MeshType(nbno=10, dim=2, code='QT1')


#------------------------------------------------------------
TR6QU8 = MeshType(nbno=14, dim=2, code='TQ2')


#------------------------------------------------------------
QU8TR6 = MeshType(nbno=14, dim=2, code='QT2')


#------------------------------------------------------------
TR6QU9 = MeshType(nbno=15, dim=2, code='TQ3')


#------------------------------------------------------------
QU9TR6 = MeshType(nbno=15, dim=2, code='QT3')


#------------------------------------------------------------
QU8TR3 = MeshType(nbno=11, dim=2, code='QT4')


#------------------------------------------------------------
TR3QU8 = MeshType(nbno=11, dim=2, code='TQ4')


#------------------------------------------------------------
QU8QU4 = MeshType(nbno=12, dim=2, code='QQ1')


#------------------------------------------------------------
QU4QU8 = MeshType(nbno=12, dim=2, code='QQ2')


#------------------------------------------------------------
QU8QU9 = MeshType(nbno=17, dim=2, code='QQ3')


#------------------------------------------------------------
QU9QU8 = MeshType(nbno=17, dim=2, code='QQ4')


#------------------------------------------------------------
QU9QU4 = MeshType(nbno=13, dim=2, code='QQ5')


#------------------------------------------------------------
QU4QU9 = MeshType(nbno=13, dim=2, code='QQ6')


#------------------------------------------------------------
QU9TR3 = MeshType(nbno=12, dim=2, code='QT5')


#------------------------------------------------------------
TR3QU9 = MeshType(nbno=12, dim=2, code='TQ5')


#------------------------------------------------------------
SEG32 = MeshType(nbno=5, dim=1, code='S32')


#------------------------------------------------------------
SEG23 = MeshType(nbno=5, dim=1, code='S23')


#------------------------------------------------------------
QU4QU4 = MeshType(nbno=8, dim=2, code='QQ7')


#------------------------------------------------------------
TR3TR3 = MeshType(nbno=6, dim=2, code='TT3')


#------------------------------------------------------------
HE8HE8 = MeshType(nbno=16, dim=3, code='HH1')


#------------------------------------------------------------
PE6PE6 = MeshType(nbno=12, dim=3, code='PP1')


#------------------------------------------------------------
TE4TE4 = MeshType(nbno=8, dim=3, code='TT4')


#------------------------------------------------------------
QU8QU8 = MeshType(nbno=16, dim=2, code='QQ8')


#------------------------------------------------------------
TR6TR6 = MeshType(nbno=12, dim=2, code='TT5')


#------------------------------------------------------------
SE2TR3 = MeshType(nbno=5, dim=2, code='ST1')


#------------------------------------------------------------
SE2TR6 = MeshType(nbno=8, dim=2, code='ST2')


#------------------------------------------------------------
SE2QU4 = MeshType(nbno=6, dim=2, code='SQ1')


#------------------------------------------------------------
SE2QU8 = MeshType(nbno=10, dim=2, code='SQ2')


#------------------------------------------------------------
SE2QU9 = MeshType(nbno=11, dim=2, code='SQ3')


#------------------------------------------------------------
SE3TR3 = MeshType(nbno=6, dim=2, code='ST3')


#------------------------------------------------------------
SE3TR6 = MeshType(nbno=9, dim=2, code='ST4')


#------------------------------------------------------------
SE3QU4 = MeshType(nbno=7, dim=2, code='SQ4')


#------------------------------------------------------------
SE3QU8 = MeshType(nbno=11, dim=2, code='SQ5')


#------------------------------------------------------------
SE3QU9 = MeshType(nbno=12, dim=2, code='SQ6')


#------------------------------------------------------------
H20H20 = MeshType(nbno=40, dim=3, code='HHV')


#------------------------------------------------------------
P15P15 = MeshType(nbno=30, dim=3, code='PPQ')


#------------------------------------------------------------
T10T10 = MeshType(nbno=20, dim=3, code='TTD')


# store all MeshType objects
ELREFS = objects_from_context(globals(), Elrefe)
MESHTYPES = objects_from_context(globals(), MeshType)
