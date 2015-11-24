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

from cataelem.Tools.base_objects import Attribute, objects_from_context

#--------------------------------------------------------------------------------------
# Remarques importantes :
#------------------------
# Les 9 attributs suivants (qui ont "auto=True") sont "automatiques".
# Ils se déduisent d'informations donnees dans le catalogue phenomenons_modelisations.py
# et dans le catalogue mesh_types.py
# Il NE FAUT PAS les redefinir dans les catalogues.

# Pour certains de ces attributs : ALIAS8, MODELI,
# la liste des valeurs autorisees (donnée ci-dessous) n'est pas complete car cette liste serait
# trop fastidieuse a maintenir.
#
#--------------------------------------------------------------------------------------

DIM_TOPO_MAILLE = Attribute(auto=True, value=('0', '1', '2', '3'),
                 comment="""
  DIM_TOPO_MAILLE : dimension topologique de la maille.
                     POI1 : 0 ; SEG: 1 ; TRIA/QUAD : 2 ; HEXA/PENTA/TETRA/PYRA:3
  Cet attribut est obtenu a partir du parametre "dim" du catalogue mesh_types

""")


PHENO = Attribute(auto=True,value=(
                  'AC',
                  'ME',
                  'PR',
                  'TH',
                  ),
                  comment="""
PHENO  =  'code' du phenomene
Si un element appartient a plusieurs phenomenes  : PHENO='##'
""")

MODELI = Attribute(auto=True, value=(
                   '2FA',
                   '2FL',
                   '2FP',
                   '3FA',
                   '3FI',
                   '3FL',
                   'AFI',
                   'AXF',
                   'CL1',
                   'CL2',
                   'D2D',
                   'D3D',
                   'DIT',
                   'FLS',
                   'FS2',
                   'FSA',
                   'PFI',
                   ),
                   comment="""
  MODELI :  'code' de la modelisaton
  Si un element appartient a plusieurs modelisations : MODELI='###'
""")

ALIAS8 = Attribute(auto=True, value=( 'MEDTRSE2',),
                   comment="""
  ALIAS8 : chaine formee par concatenation de 3 codes  :
           ALIAS(1:2) : code du phenomene
           ALIAS(3:5) : code de la modelisation
           ALIAS(6:8) : code du type_maille

           exemple :   MEFL_HEXA8 => ALIAS8=ME3FLHE8
""")

DIM_COOR_MODELI = Attribute(auto=True, value=(
                            '2',
                            '3',
                            ),
                            comment="""
  DIM_COOR_MODELI : dimension de l'espace du maillage
                    (nombre de coordonnees des points de l'espace) : 2 ou 3
  Cet attribut est obtenu a partir du parametre dim(2) de la modelisation
  remarque : DIM_COOR_MODELI >=  DIM_TOPO_MODELI
""")


DIM_TOPO_MODELI = Attribute(auto=True, value=(
                            '0',
                            '1',
                            '2',
                            '3',
                            ),
                            comment="""
  DIM_TOPO_MODELI : dimension "topologique" de la modelisation a laquelle appartient
  l'element. Exemples :
    - 3 pour les elements isoparametriques 3D
    - 2 pour les elements isoparametriques 2D
    - 2 pour plaques et coques en 3D
    - 1 pour les poutres ou pour les coques en 2D
    - ...
  Cet attribut est obtenu a partir du parametre dim(1) de la modelisation
  Convention importante : Pour les modelisations "discretes" ([2D_]DIS_xxx),
   on choisit DIM_TOPO_MODELI=-1
""")


PRINCIPAL = Attribute(auto=True, value=('OUI',),
                            comment="""
  PRINCIPAL = 'OUI' :
  L'element est "principal" pour la modelisation (i.e. ce n'est pas un element de "bord").
  La dimension toplogique de sa maille (DIM_TOPO_MAILLE) est identique a celle de sa
  modelisation (DIM_TOPO_MODELI)
  Pour les modelisations "discretes" ([2D_]DIS_xxx), TOUS les elements sont principaux.
""")


BORD = Attribute(auto=True, value=(
                 '+1',
                 '0',
                 '-1',
                 '-2',
                 ),
                 comment="""
  / '0'  : l'element est principal : DIM_TOPO_MAILLE = DIM_TOPO_MODELI
  / '-1' : l'element est tel que   : DIM_TOPO_MAILLE = DIM_TOPO_MODELI - 1
  / '-2' : l'element est tel que   : DIM_TOPO_MAILLE = DIM_TOPO_MODELI - 2
  / '-3' : l'element est tel que   : DIM_TOPO_MAILLE = DIM_TOPO_MODELI - 3
  / '+1' : l'element est tel que   : DIM_TOPO_MAILLE = DIM_TOPO_MODELI + 1

  Un element de "bord" ('-1' et '-2') sert en general a appliquer des efforts sur l'element principal qu'il
  borde. Il fait partie de la meme modelisation que l'element principal.
  Il n'a pas de sens sans un element principal a cote de lui.
  Ses noeuds sont en principe ceux d'une face (ou d'une arrete) de son element principal.
  Il ne calcule pas de "rigidite".

  Un element de coque "plaque" sur un element de volume n'est pas un element de bord.
""")


DISCRET = Attribute(auto=True, value=(
                    'OUI',
                    ),
                    comment="""
  DISCRET = 'OUI'
  La modelisation de l'element est "discrete" ([2D_]DIS_xxx)
  On reconnait les modelisations "discretes" au fait que DIM__(1)=-1
  Les elements d'une modelisations discrete sont TOUS "principaux" (voir PRINCIPAL).
""")
#--------------------------------------------------------------------------------------


ABSO = Attribute(value=(
                 'OUI',
                 ),
                 comment="""
  ABSO = 'OUI' : l'element est un element de frontiere absorbante (R4.02.05)
""")

AXIS = Attribute(value=(
                 'OUI',
                 ),
                 comment="""
  AXIS =  'OUI' : l'element est axisymetrique.
          C'est a dire qu'il est maille en 2D mais qu'il represente en realite un "anneau" de
          revolution autour de l'axe OY.
          Le champ de deplacement dans un plan radial est independant de l'azimut (theta).

""")


BORD_ISO = Attribute(value=(
                     'OUI',
                     ),
                     comment="""
  BORD_ISO = 'OUI' => l'element est l'element de bord "standard" partage par les elements isoparametriques 2D ou 3D.
""")


CL_DUAL = Attribute(value=(
                    'OUI',
                    ),
                    comment="""
CL_DUAL='OUI' : l'element est un element utilise pour dualiser les conditions aux limites.
""")


CONTACT = Attribute(value=(
                    'OUI',
                    ),
                    comment="""
  CONTACT = 'OUI' : l'element est utilise (en sous-terrain) pour la mise en oeuvre du contact
""")


COQUE = Attribute(value=(
                  'OUI',
                  ),
                  comment="""
  COQUE  =  'OUI' :  l'element est un element de coque, de plaque, de grille, ... (element de structure surfacique).
""")


C_PLAN = Attribute(value=(
                   'OUI',
                   ),
                   comment="""
  C_PLAN =  'OUI' : l'element est en "contraintes planes".
          Le maillage est 2D (OXY). L'etat de contraintes est "plan" (SIZZ=SIZX=SIYZ=0)
          L'hypothese des contraintes planes est a priori verifiee pour les structure minces selon OZ (plaque).

""")



D_PLAN = Attribute(value=(
                   'OUI',
                   ),
                   comment="""
  D_PLAN =  'OUI' : l'element est en "deformations planes".
          Le maillage est 2D (OXY). Le deplacement est independant de Z et le deplacement suivant OZ est nul.
          Un element 2D represente en realite un solide cylindrique infini selon OZ.

""")


EFGE = Attribute(value=(
                 'OUI',
                 ),
                 comment="""
  EFGE = 'OUI' : l'element mecanique est un element de structure connaissant les efforts generalises.
""")


EULER = Attribute(value=(
                  'OUI',
                  ),
                  comment="""
  EULER =  'OUI' : l'element est un element de poutre (theorie d'Euler-Bernoulli).
""")


FOURIER = Attribute(value=(
                    'OUI',
                    ),
                    comment="""
  FOURIER =  'OUI' : l'element est destine a une etude par decomposition en modes de fourier.
          Le maillage est 2D (OXY) (comme pour AXIS='OUI').

""")


FROTTEMENT = Attribute(value=(
                       'OUI',
                       ),
                       comment="""
  FROTTEMENT = 'OUI' => l'element est utilise pour traiter le frottement.
""")


GRAND_GLIS = Attribute(value=(
                       'OUI',
                       ),
                       comment="""
    ...
""")


GRILLE = Attribute(value=(
                   'OUI',
                   ),
                   comment="""
  GRILLE =  'OUI' :  l'element est un element de "grille".
""")


INCO = Attribute(value=(
                 'C2',
                 'C2O',
                 'C3',
                 'C3B',
                 ),
                 comment="""
  INCO   : Type d'elements "incompressibles" (R3.06.08)
           - Le volume de l'element reste constant (trace(epsilon)=0)
           - Il existe des ddls qui "dualisent" la condition precedente (PRES,GONF)
  INCO =  Cxk
          x  = nombre de champ : 3 champs UPG ou 2 champs UP
          k  = stabilisation de l'element : O (OSGS)
               version ne respectant pas la LBB : B
""")


INTERFACE = Attribute(value=(
                      'OUI',
                      ),
                      comment="""
  INTERFACE = 'OUI' : elements "ecrases" (pour representer un joint par exemple)
           En 3D ce sont des HEXA ou des PENTA
           En 2D ce sont des QUAD
""")


INTTHM = Attribute(value=(
                   'LUM',
                   'RED',
                   ),
                   comment="""
  INTTHM =  "type" d'integration pour les elements "principaux" des modelisations "THM" :
       /  'RED'      modelisation selective (termes capacitifs integres aux sommets, diffusifs aux points de Gauss)
       /  'LUM'      modelisation lumpee (integration aux sommets)
       Par defaut : INTTHM='CLA' ("classique")
""")


LUMPE = Attribute(value=(
                  'OUI',
                  ),
                  comment="""
  LUMPE = 'OUI' : elements "lumpes" (R3.06.07):
           - Ce sont des elements de thermique
           - Leur matrice de "masse" (MASS_THER) est diagonale
""")


LXFEM = Attribute(value=(
                  'OUI',
                  ),
                  comment="""
  LXFEM = 'OUI' : modelisations "XFEM"
          Ce sont des modelisations pour lesquelles on ajoute des ddls d'enrichissement.
          Ce sont des modelisations pour lesquelles, la notion de bord est modifiee puisque
          des fissures peuvent traverser des elements "volumiques".
""")


METH_CONTINUE = Attribute(value=(
                          'OUI',
                          ),
                          comment="""
    ...
""")


MODTHM = Attribute(value=(
                   'H',
                   'HH',
                   'HH2',
                   'HH2M',
                   'HHM',
                   'HM',
                   'SUSHI',
                   'THH',
                   'THH2',
                   'THH2M',
                   'THHM',
                   'THM',
                   'THV',
                   ),
                   comment="""
  MODTHM =  "type" pour les modelisations "THM" (thermo-hydro-mecanique) :
       /  'H'      modelisation hydraulique saturee (monophasique)
       /  'HH'     modelisation hydraulique insaturee (diphasique)
       /  'HH2'    modelisation hydraulique insaturee avec 2 composants miscibles pour chaque phase
       /  'HH2M'   modelisation hydro-mecanique insaturee avec 2 composants miscibles pour chaque phase
       /  'HHM'    modelisation hydro-mecanique insaturee
       /  'HM'     modelisation hydro-mecanique saturee
       /  'THH'    modelisation thermo-hydraulique insaturee (diphasique)
       /  'THH2'   modelisation thermo-hydraulique insaturee avec 2 composants miscibles pour chaque phase
       /  'THH2M'  modelisation thermo-hydro-mecanique insaturee avec 2 composants miscibles pour chaque phase
       /  'THHM'   modelisation thermo-hydro-mecanique insaturee
       /  'THM'    modelisation thermo-hydro-mecanique saturee
       /  'THV'    modelisation thermo-hydraulique liquide vapeur (eau sous deux phases)
       /  'SUSHI'  : elements relevant de R7.01.34

""")


NBSIGM = Attribute(value=(
                   '4',
                   '6',
                   ),
                   comment="""
  NBSIGM =  /'4'/'6'/...
        Pour les elements de mecanique, c'est le nombre de composantes du tenseur des contraintes.
        En general : '6' pour les elements 3D, '4' pour certains elements 2D
""")


POUTRE = Attribute(value=(
                   'OUI',
                   ),
                   comment="""
  POUTRE =  'OUI' :  l'element est un element de poutre, de barre, de cable, de tuyau, ... (element de structure lineique).
""")


SIGM = Attribute(value=(
                 'NON',
                 ),
                 comment="""
  SIGM = 'NON' : l'element mecanique est un element de structure connaissant les contraintes (SIXX, SIYY, ...)
          Remarque importante : l'attribut SIGM='NON' ne doit etre renseigne que pour les elements ayant
                                l'attribut EFGE='OUI'
""")


SOUS_POINT = Attribute(value=(
                       'OUI',
                       ),
                       comment="""
  SOUS_POINT = 'OUI' => l'element peut definir des sous-points dans AFFE_CARA_ELEM
""")


THM = Attribute(value=(
                'OUI',
                ),
                comment="""
    utilise ?
""")


TUYAU = Attribute(value=(
                  'OUI',
                  ),
                  comment="""
  TUYAU  =  'OUI' :  l'element est un element de "tuyau".
""")


TYPE_VOISIN = Attribute(value=(
                        'A2',
                        'F3',
                        ),
                        comment="""
  TYPE_VOISIN = typvoi : type du voisinage des elements de type "volume_fini"
                la chaine de caracteres typvoi est formee d'une suite de K2 tels que :
                  'F3' : voisin 3D par une face
                  'A2' : voisin 2D par une arrete
                   ...   (voir routine voiuti.F90)
""")


TYPMOD = Attribute(value=(
                   'AXIS',
                   'COMP1D',
                   'COMP3D',
                   'C_PLAN',
                   'D_PLAN',
                   'PLAN',
                   ),
                   comment="""
  TYPMOD : Type de  modelisation utilise pour integrer les lois de comportement (utilise dans NMCOMP)
           Les valeurs possibles de cet attribut sont definies dans le catalogue de chaque comportement
           (bibpyt/Comportement/*.py)
""")


TYPMOD2 = Attribute(value=(
                    'EJ_HYME',
                    'ELEMDISC',
                    'ELEMJOIN',
                    'GRADEPSI',
                    'INTERFAC',
                    'PMF',
                    ),
                    comment="""
  TYPMOD2 : Complement au type de  modelisation utilise pour integrer les lois de comportement TYPMOD
            (TYPMOD2 est utilise dans NMCOMP et certaines routines LCxxxx)
           Les valeurs possibles de cet attribut sont :
  TYPMOD2= GRADEPSI l'element utilise des comportements non locaux (TYPMOD(2)='GRADEPSI' pour NMCOMP)
           GRADVARI l'element utilise des comportements non locaux (TYPMOD(2)='GRADVARI' pour NMCOMP)
           ELEMJOIN l'element utilise des comportements d'elements de joints (CZM sur des modelisations *_JOINT)
           INTERFAC l'element utilise des comportements d'elements d'interface (CZM sur des modelisations *_INTERFACE)
           ELEMDISC l'element utilise des comportements d'elements a discontinuite interne
           PMF      l'element fait appel a des comportements 1D PMF (GROT_GDEP PERMIS)
""")


XFEM = Attribute(value=(
                 'XH',
                 'XH1',
                 'XH2',
                 'XH2C',
                 'XH3',
                 'XH3C',
                 'XH4',
                 'XH4C',
                 'XHC',
                 'XHC3',
                 'XHT',
                 'XHTC',
                 'XT',
                 'XTC',
                 ),
                 comment="""
  XFEM = 'XH','XT','XHT'     : element XFEM de type Heaviside, cracktip, ou mixte
         'XHC',XTC','XHTC'   : element XFEM de type Heaviside, cracktip, ou mixte, avec du contact
         'XHC3'              : element XFEM de type Heaviside, cracktip, ou mixte, avec du contact  ???
         'XH4'               : element XFEM de type Heaviside, cracktip, ou mixte, avec du contact  ???
""")


XFEM_E = Attribute(value=(
                   'C',
                   'H',
                   'H2',
                   'H3',
                   'H4',
                   'T',
                   ),
                   comment="""
  XFEM_E = 'H','T','C','H2','H3','H4' : element XFEM maille "esclave" de type Heaviside, cracktip, ou mixte

""")


XFEM_M = Attribute(value=(
                   'C',
                   'H',
                   'H2',
                   'H3',
                   'H4',
                   'T',
                   ),
                   comment="""
  XFEM_M = 'H','T','C','H2','H3','H4' : element XFEM maille "maitre" de type Heaviside, cracktip, ou mixte
""")


XLAG = Attribute(value=(
                 'NOEUD',
                 ),
                 comment="""
  XLAG = 'NOEUD'       : element XFEM ????
""")


# store all Attribute objects
ATTRS = objects_from_context(globals(), Attribute)

