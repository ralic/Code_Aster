# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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

from cataelem.Tools.base_objects import PhysicalQuantity, ArrayOfQuantities
from cataelem.Tools.base_objects import objects_from_context


# PhysicalQuantities

ABSC_R   = PhysicalQuantity(type='R',
    components=(
       'ABSC[4]',
    ),
    comment="""  ABSC_R  Type:R  Abscisse curviligne le long d'un maillage filaire
       ABSC1 : abscisse curviligne du 1er noeud d'un SEG ou d'un POI1
       ABSC2 : abscisse curviligne du 2eme noeud d'un SEG
       ABSC3 : abscisse curviligne du 3eme noeud d'un SEG (s'il existe)
       ABSC4 : abscisse curviligne du 4eme noeud d'un SEG (s'il existe)
""")


ADRSJEVE = PhysicalQuantity(type='I',
    components=(
       'I1',
    ),
    comment="""  ADRSJEVE  Type:I  Grandeur reservee au materiau code
              Attention : ne pas lui ajouter de composante
       I1 : adresse du materiau code
""")


ADRSJEVN = PhysicalQuantity(type='I',
    components=(
       'I[5]',
    ),
    comment="""  ADRSJEVN  Type:I  Grandeur dont les CMPS sont des adresses d'objets
                      JEVEUX
       I1 : adresse du 1er objet
       I2 : adresse du 2eme objet
       I3 : adresse du 3eme objet
       I4 : adresse du 4eme objet
       I5 : adresse du 5eme objet
""")


CAARPO   = PhysicalQuantity(type='R',
    components=(
       'C_FLEX_Y',
       'I_SIGM_Y',
       'C_FLEX_Z',
       'I_SIGM_Z',
    ),
    comment="""  CAARPO  Type:R  Caracteristiques des poutres courbes
        Coefficient de flexibilite anisotrope
            C_FLEX_Y : dans le plan (X,Y)
            C_FLEX_Z : dans le plan (X,Z)
                Iy = Iy/C_FLEX_Y et Iz = Iz/C_FLEX_Z
        Coefficient d'intensification des contraintes anisotrope
            I_SIGM_Y : dans le plan (X,Y)
            I_SIGM_Z : dans le plan (X,Z)
                sigmaz = I_SIGM_Z*sigmaz  et sigmay = I_SIGM_Y*sigmay
""")


CACABL   = PhysicalQuantity(type='R',
    components=(
       'SECT',
       'TENS',
    ),
    comment="""  CACABL  Type:R  Caracteristiques des cables
       SECT : section du cable
       TENS : tension initiale
""")


CACO3D   = PhysicalQuantity(type='R',
    components=(
       'CRF',
    ),
    comment="""  CACO3D  Type:R  Une "CARACTERISTIQUE" des elements de COQUE_3D
       CRF : coefficient de rotation fictive
""")


CACOQU   = PhysicalQuantity(type='R',
    components=(
       'EP',
       'SECT_L',
       'ALPHA',
       'BETA',
       'KAPPA',
       'C_METR',
       'DIST_N',
       'CTOR',
       'EXCENT',
       'INERTIE',
       'TENS',
    ),
    comment="""  CACOQU  Type:R  Caracteristiques des coques
       EP      : epaisseur de la coque
       SECT_L  : somme des sections d'armatures dans la direction L
       ALPHA   : 1er angle de l'axe de reference dans le plan transverse
       BETA    : 2eme angle de l'axe de reference dans le plan transverse
                 Ces deux angles definissent par rapport au repere de reference
                 le vecteur a projeter sur le plan tangent de l'element afin
                 d'y definir le repere (T,N,L).
                 La direction L est perpendiculaire au plan tangent.
       KAPPA   : coefficient de cisaillement transverse
       C_METR  : coefficient de modification metrique pour l'element coque
       DIST_N  : excentrement de la nappe d'armatures vs la maille support
       CTOR    : constante multiplicative de la rigidite en rotation associee a
                 la direction perpendiculaire au plan tangent de l'element
       EXCENT  : excentrement du feuillet moyen
       INERTIE : prise en compte de l'inertie de rotation
       TENS    : (réservé aux membranes) application d'une contrainte 
                 initiale pour faire converger à la première iteration. 
                 Elle disparait aux increments suivants
""")


CACOQUF  = PhysicalQuantity(type='K8',
    components=(
       'EP',
       'SECT_L',
       'DIST_N',
       'EXCENT',
    ),
    comment="""  CACOQUF  Type:K8  Caracteristiques des coques qui dependent d'une fonction
       EP      : epaisseur de la coque
       SECT_L  : somme des sections d'armatures dans la direction L
       DIST_N  : excentrement de la nappe d'armatures vs la maille support
       EXCENT  : excentrement du feuillet moyen
""")


CADISA   = PhysicalQuantity(type='R',
    components=(
       'A[144]',
    ),
    comment="""  CADISA  Type:R  Matrice d'amortissement des elements discrets
        A[144] : coefficients de la matrice d'amortissement
""")


CADISK   = PhysicalQuantity(type='R',
    components=(
       'K[144]',
    ),
    comment="""  CADISK Type:R  Matrice de rigidite des elements discrets
       K[144] : coefficients de la matrice de rigidite
""")


CADISM   = PhysicalQuantity(type='R',
    components=(
       'M[144]',
    ),
    comment="""  CADISM Type:R Matrice de masse des elements discrets 12 x 12
                  (MODELISATION='DIS_XXXX')
       M[144] : coefficients de la matrice masse des elements

""")


CAFI_R   = PhysicalQuantity(type='R',
    components=(
       'YG',
       'ZG',
       'AIRE',
       'YP',
       'ZP',
       'GX',
       'NUMGR',
    ),
    comment="""  CAFI_R  Type:R Caracteristiques des fibres dans le repere local de la poutre
       YG    : coordonnee du centre de gravite de la fibre
       ZG    : coordonnee du centre de gravite de la fibre
       AIRE  : aire de lafibre
       YP    : coordonnee du centre de gravite du groupe de fibre
       ZP    : coordonnee du centre de gravite du groupe de fibre
       GX    : Constante de torsion pour le groupe de fibre
       NUMGR : numero du groupe de fibre
""")


CAGEPO   = PhysicalQuantity(type='R',
    components=(
       'HY1',
       'HZ1',
       'EPY1',
       'EPZ1',
       'HY2',
       'HZ2',
       'EPY2',
       'EPZ2',
       'R1',
       'EP1',
       'R2',
       'EP2',
       'TSEC',
    ),
    comment="""  CAGEPO Type:R Caracteristiques geometriques des poutres a section
    rectangulaire ou circulaire
       HY1 : dimension suivant GY du rectangle (noeud 1)
       HZ1 : dimension suivant GZ du rectangle (noeud 1)
       EPY1 : epaisseur suivant GY d'un rectangle creux (noeud 1)
       EPZ1 : epaisseur suivant GZ d'un rectangle creux (noeud 1)
       HY2 : dimension suivant GY du rectangle (noeud 2)
       HZ2 : dimension suivant GZ du rectangle (noeud 2)
       EPY2 : epaisseur suivant GY d'un rectangle creux (noeud 2)
       EPZ2 : epaisseur suivant GZ d'un rectangle creux (noeud 2)
       R1 : rayon du cercle (noeud 1)
       EP1 : epaisseur du cercle creux (noeud 1)
       R2 : rayon du cercle (noeud 2)
       EP2 : epaisseur du cercle creux (noeud 2)
       TSEC : type de la section (generale, rectangle, cercle)
""")


CAGNBA   = PhysicalQuantity(type='R',
    components=(
       'A1',
    ),
    comment="""  CAGNBA Type:R Caracteristiques geometriques d'une section de barre
       A1 : aire de la section transversale
""")


CAGNPO   = PhysicalQuantity(type='R',
    components=(
       'A1',
       'IY1',
       'IZ1',
       'AY1',
       'AZ1',
       'EY1',
       'EZ1',
       'JX1',
       'RY1',
       'RZ1',
       'RT1',
       'AI1',
       'JG1',
       'IYR21',
       'IZR21',
       'A2',
       'IY2',
       'IZ2',
       'AY2',
       'AZ2',
       'EY2',
       'EZ2',
       'JX2',
       'RY2',
       'RZ2',
       'RT2',
       'AI2',
       'JG2',
       'IYR22',
       'IZR22',
       'TVAR',
    ),
    comment="""  CAGNPO Type:R Caracteristiques mecaniques d'une section de poutre
      A1 : aire de la section (noeud 1)
      IY1 : moment d'inertie principal par rapport a GZ (noeud 1)
      IZ1 : moment d'inertie principal par rapport a GY (noeud 1)
      AY1 : coefficient de cisaillement dans la direction Gy (noeud 1)
      AZ1 : coefficient de cisaillement dans la direction Gz (noeud 1)
      EY1 : excentrement du centre de torsion, composante de CG
      suivant GY (noeud 1)
      EZ1 : excentrement du centre de torsion, composante de CG suivant
      GZ (noeud 1)
      JX1 : constante de torsion (noeud 1)
      RY1 : distance d'une fibre externe mesuree suivant Y (noeud 1)
      RZ1 : distance d'une fibre externe mesuree suivant Z (noeud 1)
      RT1 : rayon de torsion efficace (noeud 1)
      AI1 : aire de la section interieure, cas des tubes par exemple
      (noeud 1) = 0 pour une section pleine
      JG1 : constante de gauchissement (noeud 1)
      IYR21 : Int(y(y*y+z*z)ds sur le domaine S (noeud 1)
      IZR21 : Int(z(y*y+z*z)ds sur le domaine S (noeud 1)
      A2 : aire de la section (noeud 2)
      IY2 : moment d'inertie principal par rapport a GZ (noeud 2)
      IZ2 : moment d'inertie principal par rapport a GY (noeud 2)
      AY2 : coefficient de cisaillement dans la direction Gy (noeud 2)
      AZ2 : coefficient de cisaillement dans la direction Gz (noeud 2)
      EY2 : excentrement du centre de torsion, composante de CG suivant
      GY (noeud 2)
      EZ2 : excentrement du centre de torsion, composante de CG suivant
      GZ (noeud 2)
      JX2 : constante de torsion (noeud 2)
      RY2 : distance d'une fibre externe mesuree suivant Y (noeud 2)
      RZ2 : distance d'une fibre externe mesuree suivant Z (noeud 2)
      RT2 : rayon de torsion efficace (noeud 2)
      AI2 : aire de la section interieure, cas des tubes
      par exemple (noeud 2) = 0 pour une section pleine
      JG2 constante de gauchissement (noeud 2)
      IYR22 : INT(y(y*y+z*z)ds sur le domaine S(noeud 2)
      IZR22 : INT(z(y*y+z*z)ds sur le domaine S(noeud 2)
      TVAR : type de section (constante, variable)
""")


CAMASS   = PhysicalQuantity(type='R',
    components=(
       'C',
       'ALPHA',
       'BETA',
       'KAPPA',
       'X',
       'Y',
       'Z',
    ),
    comment="""  CAMASS Type:R Caracteristiques geometriques des elements massifs
      C : indice de definition du repere d'orthotropie (=1 definition par 3)
      angles nautiques, = -1 definition par un axe et un point sur cet axe)
      ALPHA : 1er angle nautique
      BETA :  2eme angle nautique
      KAPPA : 3eme angle nautique
      X : nul si C=1, sinon 1ere coordonnee du point de l'axe
      Y : nul si C=1, sinon 2eme coordonnee du point de l'axe
      Z : nul si C=1, sinon 3eme coordonnee du point de l'axe
""")


CAORIE   = PhysicalQuantity(type='R',
    components=(
       'ALPHA',
       'BETA',
       'GAMMA',
       'ALPHA2',
       'BETA2',
       'GAMMA2',
       'ALPHA3',
       'BETA3',
       'GAMMA3',
       'ALPHA4',
       'BETA4',
       'GAMMA4',
       'ICOUDE',
       'DN1N2',
       'RCOURB',
       'ANGCOU',
       'ANGZZK',
       'REP',
       'AXE_X',
       'AXE_Y',
       'AXE_Z',
       'O_X',
       'O_Y',
       'O_Z',
    ),
    comment="""  CAORIE Type:R Orientation d'un segment en 3D. Angles nautiques
       ALPHA : 1er angle nautique pour la maille SEG2 (ou le 1er noeud
       d'une maille SEG3 ou SEG4)
       BETA : 2eme angle nautique pour la maille SEG2 (ou le 1er noeud
       d'une maille SEG3 ou SEG4)
       GAMMA : 3eme angle nautique pour la maille SEG2 (ou le 1er noeud
       d'une maille SEG3 ou SEG4)
       ALPHA2 : 1er angle nautique pour le 2eme noeud d'une maille SEG3
       ou SEG4
       BETA2 :  2eme angle nautique pour le 2eme noeud d'une maille SEG3
       ou SEG4
       GAMMA2 : 3eme angle nautique pour le 2eme noeud d'une maille SEG3
       ou SEG4
       ALPHA3 : 1er angle nautique pour le 3eme noeud d'une maille SEG3
       ou SEG4
       BETA3 :  2eme angle nautique pour le 3eme noeud d'une maille SEG3
       ou SEG4
       GAMMA3 : 3eme angle nautique pour le 3eme noeud d'une maille SEG3
       ou SEG4
       ALPHA4 : 1er  angle nautique pour le 4eme noeud d'une maille SEG4
       BETA4 :  2eme angle nautique pour le 4eme noeud d'une maille SEG4
       GAMMA4 : 3eme angle nautique pour le 4eme noeud d'une maille SEG4
       ICOUDE :
       DN1N2 :
       RCOURB :
       ANGCOU :
       ANGZZK :
       REP    : definition du repere
                = 1 : COQUE_INTR_UTIL permet le passage du repere
                      intrinseque de l'element au repere local de l'element
                = 2 : COQUE_UTIL_INTR permet le passage du repere
                      local de l'element au repere intrinseque de l'element
       AXE_X : composante X de l'axe du repere cylindrique
       AXE_Y : composante Y de l'axe du repere cylindrique
       AXE_Z : composante Z de l'axe du repere cylindrique
       O_X   : composante X de O (origine du repere cylindrique)
       O_Y   : composante Y de O (origine du repere cylindrique)
       O_Z   : composante Z de O (origine du repere cylindrique)
""")


CAPOUF   = PhysicalQuantity(type='R',
    components=(
       'B_T',
       'B_N',
       'B_TN',
       'A_FLUI',
       'A_CELL',
       'COEF_ECH',
    ),
    comment="""  CAPOUF Type:R Caracteristiques geometriques des elements poutre fluide
       B_T : terme correcteur transverse
       B_N : terme correcteur normal
       B_TN : terme correcteur couple
       A_FLUI : aire fluide associe au terme correcteur
       A_CELL : aire de la cellule de reference
       COEF_ECH:  par rapport a la periode reelle de la cellule de reference
""")


CARCRI   = PhysicalQuantity(type='R',
    components=(
       'ITECREL',
       'MACOMP',
       'RESCREL',
       'THETA',
       'ITEDEC',
       'INTLOC',
       'PERTURB',
       'TOLDEBO',
       'ITEDEBO',
       'TSSEUIL',
       'TSAMPL',
       'TSRETOUR',
       'POSTITER',
       'LC_EXT[3]',
       'MODECALC',
       'ALPHA',
       'LC_EXT2[2]',
       'POSTINCR',
    ),
    comment="""  CARCRI Type :R Critere de convergence d'un probleme non-lineaire materiel
    (pour 1 point de Gauss)
       ITECREL  : nombre max d'iteration (ITER_INTE_MAXI)
       MACOMP   : type de matrice tangente : VERIFICATION / PERTURBATION
       RESCREL  : tolerance pour la convergence ( RESI_INTE_RELA )
       THETA    : parametre de la theta_methode
       ITEDEC   : indicateur pour le redecoupage ( ITER_INTE_PAS )
       INTLOC   : ALGO_INTE ( NEWTON, NEWTON_PERT,... RUNGE_KUTTA )
       PERTURB  : perturbation pour matrice tangente par perturbation
       TOLDEBO  : tolerance pour la verification de DEBORST
       ITEDEBO  : nb iterations maxi pour DEBORST (ITER_MAXI_DEBORST == ITEDEBO)
       TSSEUIL  : seuil d'activation matrice tangente/secante (TANGSEC_SEUIL)
       TSAMPL   : perturbation de la matrice TANGENTE/SECANTE (TANGSEC_AMPL)
       TSRETOUR : taux de retour vers la matrice tangente (TANGSEC_RETOUR)
       POSTITER : type de critere POST_ITER : 0=rien, 1=CRIT_RUPT
       LC_EXT[3]: pointeurs vers routines externes (UMAT / MFRONT)
       MODECALC : 1 (CALC_POINT_MAT) ou 0 (ELEMENTS FINIS)
       ALPHA    : PARM_ALPHA pour les volumes finis
       POSTINCR : type de critere POST_INCR : 0=rien, 1=REST_ECRO
""")


CASECT   = PhysicalQuantity(type='K8',
    components=(
       'NOM',
    ),
    comment="""  CASECT Type:K8 Nom d'un objet de type cara-poutre contenant le
    caracteristiques d'une section de poutre
       NOM : nom d'un objet cara-poutre
""")


CHGREPER = PhysicalQuantity(type='R',
    components=(
       'NATCHG',
       'CMAT[9]',
    ),
    comment=""" CHGREPER  Type:R  Sert a definir les parametres des changements de repere
    NATCHG  : Nature du changement de repere
                0.5 : passage GLOBAL vers LOCAL, matrice constante par elements
    CMAT[9] : Matrice de passage 3x3 pour le changement de repere.
    En sortie de l'option REPERE_LOCAL
    En entree de l'option MODI_REPERE
""")


CHLI_R   = PhysicalQuantity(type='R',
    components=(
       'CHLI[3]',
       'CHAR0',
    ),
    comment="""  CHLI_R Type:R Charge limite
       CHLI1 : 1er terme elementaire
       CHLI2 : 2eme terme elementaire
       CHLI3 : 3eme terme elementaire
       CHAR0 : terme elementaire du au chargement permanent
""")


CINFDI   = PhysicalQuantity(type='R',
    components=(
       'REPK',
       'REPM',
       'REPA',
       'SYMK',
       'SYMM',
       'SYMA',
       'DISK',
       'DISM',
       'DISA',
       'ETAK',
       'TYDI',
    ),
    comment="""  CINFDI Type:R : Informations pour les elements discrets
      REP(K;M;A) si =1 repere global,  si =2 repere local
         REPK  : Type de repere pour les raideurs
         REPM  : Type de repere pour les masses
         REPA  : Type de repere pour les amortissements
      SYM(K;M;A) si =1 symetrique, si =2 non-symetrique
         SYMK  : Type de matrice de raideur
         SYMM  : Type de matrice de MASSE
         SYMA  : Type de matrice de d'amortissement
      DIS(K;M;A) si =0, pas de caracteristiques, si =1 caracteristiques
         DISK  : Discret avec des caracteristique de raideur
         DISM  : Discret avec des caracteristique de masse
         DISA  : Discret avec des caracteristique de d'amortissement
      ETAK  : Coefficient d'amortissement hysteretique
              uniquement sur les matrices de raideurs
      TYDI  : Type du discret

""")


CODE_I   = PhysicalQuantity(type='I',
    components=(
       'IRET',
    ),
    comment="""  CODE_I Type:I
       IRET :
""")


COEH_F   = PhysicalQuantity(type='K8',
    components=(
       'H',
       'H_INF',
       'H_SUP',
    ),
    comment="""  COEH_F Type:K8 Coefficient d'echange thermique (fonction) PHI = h*(Text - T)
       H : coefficient d'echange (probleme continu)
       H_INF : coefficient d'echange sur la face inferieure d'une coque
       H_SUP : coefficient d'echange sur la face superieure d'une coque
""")


COEH_R   = PhysicalQuantity(type='R',
    components=(
       'H',
       'H_INF',
       'H_SUP',
    ),
    comment="""  COEH_R Type:R Coefficient d'echange thermique (reel) PHI = h *(Text - T)
       H : coefficient d'echange (probleme continu)
       H_INF : coefficient d'echange sur la face inferieure d'une coque
       H_SUP : coefficient d'echange sur la face superieure d'une coque
""")


COMPOR   = PhysicalQuantity(type='K16',
    components=(
       'RELCOM',
       'NBVARI',
       'DEFORM',
       'INCELA',
       'C_PLAN',
       'NUME_LC',
       'SD_COMP',
       'KIT[9]',
       'NVI_C',
       'NVI_T',
       'NVI_H',
       'NVI_M',
    ),
    comment="""  COMPOR Type:K16 Comportement materiel
       RELCOM : relation de comportement : 'ELAS' , 'VMIS_ISOT_LINE' , ...
       NBVARI : nombre de variables internes pour le comportement
       DEFORM : type de deformations : 'PETIT' , ...
       INCELA : comp_incr ou comp_elas
       C_PLAN : type de contraintes planes : DEBORST ou analytique
       NUME_LC : numero de routine LC
       SD_COMP : nom de la SD issue de DEFI_COMPOR (PMF, MONOCRISTAL,..)
       KIT1 : pour la relation kit ....
       KIT2 : pour la relation kit ....
       KIT3 : pour la relation kit ....
       KIT4 : pour la relation kit ....
       KIT5 : pour la relation kit ....
       KIT6 : pour la relation kit ....
       KIT7 : pour la relation kit ....
       KIT8 : pour la relation kit ....
       KIT9 : pour la relation kit ....
       NVI_C : nombre de variables internes associees a la relation de couplage
       NVI_T : nombre de variables internes associees a la relation thermique
       NVI_H : nombre de variables internes associees a la relation hydraulique
       NVI_M : nombre de variables internes associees a la relation mecanique
""")


CORR_R   = PhysicalQuantity(type='R',
    components=(
       'CORR',
    ),
    comment="""  CORR_R Type:R Corrosion
       COOR :
""")


DCEL_I   = PhysicalQuantity(type='I',
    components=(
       'NPG_DYN',
       'NCMP_DYN',
    ),
    comment="""  DCEL_I Type:I
       NPG_DYN  : Nombre de sous-points pour un cham_elem a sous-points
       NCMP_DYN : Nombre reel de CMPS pour la grandeur VARI_R
""")


DDLI_C   = PhysicalQuantity(type='C',
    components=(
       'C',
    ),
    comment="""  DDLI_C Type:C Valeur (complexe) imposee a 1 ddl (ou a 1 relation
    lineaire) : Somme des alpha_i*ui = C
       C : Second membre de type complexe de la relation
""")


DDLI_F   = PhysicalQuantity(type='K24',
    components=(
       'C',
    ),
    comment="""  DDLI_F Type:K8 Valeur (fonction) imposee a 1 ddl (ou a 1 relation
    lineaire) : Somme des alpha_i*ui = C
       C : Second membre de type fonction de la relation
""")


DDLI_R   = PhysicalQuantity(type='R',
    components=(
       'C',
    ),
    comment="""  DDLI_R Type:R Valeur (reelle) imposee a 1 ddl (ou a 1 relation
    lineaire) : Somme des alpha_i*ui = C
       C : Second membre de type reel de la relation
""")


DDLM_C   = PhysicalQuantity(type='C',
    components=(
       'A[3]',
       'B[3]',
    ),
    comment="""  DDLM_C Type:C Coefficients (complexe) pour un noeud d'une relation
    lineaire : mecanique : A1.UX + A2.UY + ... + B3.DRZ = C
       A(1) : coefficient complexe pour le ddl Ux
       A(2) : coefficient complexe pour le ddl Uy
       A(3) : coefficient complexe pour le ddl Uz
       B(1) : coefficient complexe pour le ddl DRx
       B(2) : coefficient complexe pour le ddl DRy
       B(3) : coefficient complexe pour le ddl DRz
""")


DDLM_R   = PhysicalQuantity(type='R',
    components=(
       'A[3]',
       'B[3]',
    ),
    comment="""  DDLM_R Type:R Coefficients (reels) pour un noeud d'une relation
    lineaire : mecanique : A1.UX + A2.UY+...+B3.DRZ = C
    thermique : A1.T + A2 Tinf + A3 Tsup = C pour les elements de
    coque thermique
       A(1) Mecanique: coefficient reel pour le ddl UX
            Thermique: coefficient reel pour le ddl Tmoyen
       A(2) Mecanique: coefficient reel pour le ddl UY
            Thermique: coefficient reel pour le ddl Tinf
       A(3) Mecanique: coefficient reel pour le ddl UZ
       Thermique: coefficient reel pour le ddl Tsup
       B(1) Mecanique: coefficient reel pour le ddl DRX
       B(2) Mecanique: coefficient reel pour le ddl DRY
       B(3) Mecanique: coefficient reel pour le ddl DRZ
""")

#------------------------------------------------------------------------------------------
# Il est tres important que les 3 grandeurs DEPL_R, DEPL_C et DEPL_F aient les memes
# composantes (et dans le meme ordre) :
list_cmp_depl=(
  'DX',         'DY',         'DZ',         'DRX',        'DRY',        'DRZ',
  'GRX',        'GLIS',       'PRES',       'PRE[2]',     'TEMP',       'PHI',
  'DH',         'H1X',        'H1Y',        'H1Z',        'H1PRE1',     'H2X',
  'H2Y',        'H2Z',        'H2PRE1',     'H3X',        'H3Y',        'H3Z',
  'H3PRE1',     'H4X',        'H4Y',        'H4Z',        'LAGR',       
  'K1',         'K2',          'K3',        'LAGS_C',
  'LAGS_F[2]',  'LAG2_C',     'LAG2_F[2]',  'LAG3_C',     'LAG3_F[2]',  'LAG4_C',
  'LAG4_F[2]',  'UI2',        'VI2',        'WI2',        'UI3',        'VI3',
  'WI3',        'UI4',        'VI4',        'WI4',        'UI5',        'VI5',
  'WI5',        'UI6',        'VI6',        'WI6',        'UO2',        'VO2',
  'WO2',        'UO3',        'VO3',        'WO3',        'UO4',        'VO4',
  'WO4',        'UO5',        'VO5',        'WO5',        'UO6',        'VO6',
  'WO6',        'WO',         'WI1',        'WO1',        'GONF',       'EPXX',
  'EPYY',       'EPZZ',       'EPXY',       'EPXZ',       'EPYZ',       'D1',
  'D1X',        'D1Y',        'D1Z',        'D2',         'D2X',        'D2Y',
  'D2Z',        'D3',         'D3X',        'D3Y',        'D3Z',        'VARI',
  'LAG_GV',     'V1[3]',      'V2[3]',      'V3[3]',      'PRES1[3]',   'PRES2[3]',
  'PRES3[3]',   'SIGN',       'SITX',       'SITY',       'LH1',        'SIXX',
  'SIYY',       'SIZZ',       'SIXY',       'DAMG',       'PTOT',       'PIX',
  'PIY',        'PIZ',
)
comment_depl= """  DEPL_R/_C/_F  Deplacement reel, complexe ou fonction
       DX, DY, DZ : translation suivant X, Y ET Z (repere global)
       DRX, DRY, DRZ : rotation autour de X, Y ET Z (repere global)
       GLIS    : translation relative suivante tangente (repere local)
       PRE1, PRE2 : DDL de pression
       TEMP : DDL de temperature
       PHI : angle de fissuration
       DH : diametre hydraulique
       Pour X-FEM: H1X, H1Y, H1Z
                   H2X, H2Y, H2Z
                   H3X, H3Y, H3Z
                   H4X, H4Y, H4Z : DDLS HEAVYSIDE X-FEM
       Pour HM-XFEM: H1PRE1
                     H2PRE1
                     H3PRE1 : DDLS HEAVYSIDE HM-XFEM
       LAGR : parametre de lagrange du a la dualisation des conditions aux
       limites
       Pour X-FEM: K1, K2, K3 : DDLS enrichis vectoriels (CRACKTIP) X-FEM
       LAGS_C, LAG2_C, LAG3_C, LAG4_C :
       LAGS_F1, LAGS_F2, LAG2_F1, LAG2_F2, LAG3_F1, LAG3_F2, LAG4_F1, LAG4_F2 :
       HPRE1 : DDL de pression Heaviside pour la pression du massif (HM_XFEM)
       UI2, VI2, WI2, ...,UO6, VO6, WO6, WO, WI1, WO1 : DDL des elements de tube
       GONF :
       EPXX, EPYY, EPZZ, EPXY, EPXZ, EPYZ :
       D1 : projection du champ suivant le vecteur directeur (D1X,D1Y,D1Z)
       D1X, D1Y, D1Z :
       D2 : projection du champ suivant le vecteur directeur (D2X,D2Y,D2Z)
       D2X, D2Y, D2Z :
       D3 : projection du champ suivant le vecteur directeur (D3X,D3Y,D3Z)
       D3X, D3Y, D3Z :
       VARI :
       LAG_GV :
       V11     : composante DU1/DX1 de la modelisation mixte second gradient
       V12     : composante DU1/DX2 de la modelisation mixte second gradient
       V13     : composante DU1/DX3 de la modelisation mixte second gradient
       V21     : composante DU2/DX1 de la modelisation mixte second gradient
       V22     : composante DU2/DX2 de la modelisation mixte second gradient
       V23     : composante DU2/DX3 de la modelisation mixte second gradient
       V31     : composante DU3/DX1 de la modelisation mixte second gradient
       V32     : composante DU3/DX2 de la modelisation mixte second gradient
       V33     : composante DU3/DX3 de la modelisation mixte second gradient
       PRES11  : multiplicateur assurant DU1/DX1=V11 de la modelisation 2ND GRAD
       PRES12  : multiplicateur assurant DU1/DX2=V12 de la modelisation 2ND GRAD
       PRES13  : multiplicateur assurant DU1/DX3=V13 de la modelisation 2ND GRAD
       PRES21  : multiplicateur assurant DU2/DX1=V21 de la modelisation 2ND GRAD
       PRES22  : multiplicateur assurant DU2/DX2=V22 de la modelisation 2ND GRAD
       PRES23  : multiplicateur assurant DU2/DX3=V23 de la modelisation 2ND GRAD
       PRES31  : multiplicateur assurant DU3/DX1=V31 de la modelisation 2ND GRAD
       PRES32  : multiplicateur assurant DU3/DX2=V32 de la modelisation 2ND GRAD
       PRES33  : multiplicateur assurant DU3/DX3=V33 de la modelisation 2ND GRAD
       SIGN    : effort de contact normal
       SITX    : effort de contact normal
       SITY    : effort de contact tangent direction 2
       LH1     : multiplicateur de Lagrange pression a travers fissure HM
       SIXX,SIYY,SIXY    : contraintes regularisees pour modele GRAD_SIGM
       DAMG :
       PTOT    : pression totale utilisee pour le chainage vers la mecanique
                 en HM
       PIX     : projete du gradient de pression suivantx (methode OSGS)
       PIY     : projete du gradient de pression suivanty (methode OSGS)
       PIZ     : projete du gradient de pression suivantz (methode OSGS)
"""

DEPL_R   = PhysicalQuantity(type='R',
    components= list_cmp_depl,
    comment=comment_depl,)

DEPL_C   = PhysicalQuantity(type='C',
    components= list_cmp_depl,
    comment=comment_depl,)

DEPL_F   = PhysicalQuantity(type='K8',
    components= list_cmp_depl,
    comment=comment_depl,)
#------------------------------------------------------------------------------------------


DERA_R   = PhysicalQuantity(type='R',
    components=(
       'DCHA_V',
       'DCHA_T',
       'IND_DCHA',
       'VAL_DCHA',
       'X11',
       'X22',
       'X33',
       'X12',
       'X13',
       'X23',
       'RADI_V',
       'ERR_RADI',
    ),
    comment="""  DERA_R Type:R indicateurs locaux de decharge et de perte de radialite
       DCHA_V   : indicateur de decharge sur le deviateur des contraintes
       DCHA_T   : indicateur de decharge sur les contraintes totales
       IND_DCHA : indic. de charge (1,2) ou decharge (-1 elas, -2 plas abusive)
       X11      : tenseur cinematique utilise pour IND_DCHA et VAL_DCHA, CMP 11
       X22      : tenseur cinematique utilise pour IND_DCHA et VAL_DCHA, CMP 22
       X33      : tenseur cinematique utilise pour IND_DCHA et VAL_DCHA, CMP 33
       X12      : tenseur cinematique utilise pour IND_DCHA et VAL_DCHA, CMP 12
       X13      : tenseur cinematique utilise pour IND_DCHA et VAL_DCHA, CMP 13
       X23      : tenseur cinematique utilise pour IND_DCHA et VAL_DCHA, CMP 23
       RADI_V   : indic. de perte de radialite sur le deviateur des contraintes
       ERR_RADI : indicateur d'erreur d'integration due a la non radialite
""")


DISS_R   = PhysicalQuantity(type='R',
    components=(
       'ENDO',
    ),
    comment="""  DISS_R Type:R energie dissipee
       ENDO   : dissipation due a l'endommagement
""")


DOMA_R   = PhysicalQuantity(type='R',
    components=(
       'DOMA',
    ),
    comment="""  DOMA_R Type:R champ de dommage sur une structure
       DOMA     : Valeur du dommage
""")


DOMMAG   = PhysicalQuantity(type='R',
    components=(
       'DOMA',
    ),
    comment="""  DOMMAG Type:R A SUPPRIMER
       DOMA     : Valeur du dommage
""")


DURT_R   = PhysicalQuantity(type='R',
    components=(
       'HV',
    ),
    comment="""  DURT_R Type:R Initialisation du calcul de la durete associe a la metallurgie
       HV : valeur
""")


ENDO_R   = PhysicalQuantity(type='R',
    components=(
       'TRIAX',
       'SI_ENDO',
       'COENDO',
       'DOM_LEM',
    ),
    comment="""  ENDO_R Type:R champ d'endommagement sur une structure
       TRIAX    : Taux de triaxialite
       SI_ENDO  : Contrainte equivalente d'endommagement
       COENDO   : Contrainte equivalente d'endommagement normalisee
       DOM_LEM  : Endommagement de Lemaitre
""")


ENER_R   = PhysicalQuantity(type='R',
    components=(
       'TOTALE',
       'TRAC_COM',
       'TORSION',
       'MEMBRANE',
       'FLEXION',
       'FLEX_Y',
       'FLEX_Z',
       'PLAN_XY',
       'PLAN_XZ',
       'DX',
       'DY',
       'DZ',
       'DRX',
       'DRY',
       'DRZ',
       'CISAILLE',
       'COUPL_MF',
    ),
    comment="""  ENER_R Type:R Energie
       TOTALE : energie totale de l'element
       TRAC_COM : energie en traction-compression
       TORSION : energie en torsion
       MEMBRANE : energie en membrane
       FLEXION : energie en flexion
       FLEX_Y : energie en flexion Y
       FLEX_Z : energie en flexion Z
       PLAN_XY : energie dans le plan XY
       PLAN_XZ : energie dans le plan XZ
       DX : energie suivant DX
       DY : energie suivant DY
       DZ : energie suivant DZ
       DRX : energie suivant DRX
       DRY : energie suivant DRY
       DRZ : energie suivant DRZ
       CISAILLE : energie en cisaillement
       COUPL_MF : energie en couplage membrane-flexion
""")


EPSI_C   = PhysicalQuantity(type='C',
    components=(
       'EPXX',
       'EPYY',
       'EPZZ',
       'EPXY',
       'EPXZ',
       'EPYZ',
       'EXX',
       'EYY',
       'EXY',
       'KXX',
       'KYY',
       'KXY',
       'EPX',
       'GAXY',
       'GAXZ',
       'GAT',
       'KY',
       'KZ',
       'GAX',
       'GAY',
       'INVA_2',
       'PRIN_[3]',
       'INVA_2SG',
       'PRE1',
       'P1DX',
       'P1DY',
       'P1DZ',
       'PRE2',
       'P2DX',
       'P2DY',
       'P2DZ',
       'TEMP',
       'TEDX',
       'TEDY',
       'TEDZ',
       'DX',
       'DY',
       'DZ',
       'VECT_1_X',
       'VECT_1_Y',
       'VECT_1_Z',
       'VECT_2_X',
       'VECT_2_Y',
       'VECT_2_Z',
       'VECT_3_X',
       'VECT_3_Y',
       'VECT_3_Z',
       'EPTHER_L',
       'EPTHER_T',
       'EPTHER_N',
       'EPSECH',
       'EPHYDR',
       'DEPV',
       'DGONFX[3]',
       'DEPV1[3]',
       'DEPV2[3]',
       'DEPV3[3]',
       'DV11X[3]',
       'DV12X[3]',
       'DV13X[3]',
       'DV21X[3]',
       'DV22X[3]',
       'DV23X[3]',
       'DV31X[3]',
       'DV32X[3]',
       'DV33X[3]',
       'PRES',
       'PRES1[3]',
       'PRES2[3]',
       'PRES3[3]',
       'EPPTOT',
       'DIVU',
    ),
    comment=""" EPSI_C Type:C
""")


EPSI_F   = PhysicalQuantity(type='K8',
    components=(
       'EPXX',
       'EPYY',
       'EPZZ',
       'EPXY',
       'EPXZ',
       'EPYZ',
       'DIVU',
       'EXX',
       'EYY',
       'EXY',
       'KXX',
       'KYY',
       'KXY',
       'EPX',
       'KY',
       'KZ',
    ),
    comment="""  EPSI_F Type:K8 Deformation (fonction)
       EPXX : epsilon_xx deformation d'un milieu continu
       EPYY : epsilon_yy deformation d'un milieu continu
       EPZZ : epsilon_zz deformation d'un milieu continu
       EPXY : epsilon_xy deformation d'un milieu continu
       EPXZ : epsilon_xz deformation d'un milieu continu
       EPYZ : epsilon_yz deformation d'un milieu continu
       DIVU    : dilatation volumique (chainage HM)
       EXX coque : deformations generalisees
       EYY coque : deformations generalisees
       EXY coque : deformations generalisees
       KXX coque : deformations generalisees
       KYY coque : deformations generalisees
       KXY coque : deformations generalisees
       EPX Poutre: elongation selon l'axe de la poutre
       KY Poutre: courbure selon l'axe Y
       KZ Poutre: courbure selon l'axe Z
""")


EPSI_R   = PhysicalQuantity(type='R',
    components=(
       'EPXX',
       'EPYY',
       'EPZZ',
       'EPXY',
       'EPXZ',
       'EPYZ',
       'EXX',
       'EYY',
       'EXY',
       'KXX',
       'KYY',
       'KXY',
       'EPX',
       'GAXY',
       'GAXZ',
       'GAT',
       'KY',
       'KZ',
       'GAX',
       'GAY',
       'INVA_2',
       'PRIN_[3]',
       'INVA_2SG',
       'PRE1',
       'P1DX',
       'P1DY',
       'P1DZ',
       'PRE2',
       'P2DX',
       'P2DY',
       'P2DZ',
       'TEMP',
       'TEDX',
       'TEDY',
       'TEDZ',
       'DX',
       'DY',
       'DZ',
       'VECT_1_X',
       'VECT_1_Y',
       'VECT_1_Z',
       'VECT_2_X',
       'VECT_2_Y',
       'VECT_2_Z',
       'VECT_3_X',
       'VECT_3_Y',
       'VECT_3_Z',
       'EPTHER_L',
       'EPTHER_T',
       'EPTHER_N',
       'EPSECH',
       'EPHYDR',
       'DEPV',
       'DGONFX[3]',
       'DEPV1[3]',
       'DEPV2[3]',
       'DEPV3[3]',
       'DV11X[3]',
       'DV12X[3]',
       'DV13X[3]',
       'DV21X[3]',
       'DV22X[3]',
       'DV23X[3]',
       'DV31X[3]',
       'DV32X[3]',
       'DV33X[3]',
       'PRES',
       'PRES1[3]',
       'PRES2[3]',
       'PRES3[3]',
       'EPPTOT',
       'DIVU',
    ),
    comment="""  EPSI_R Type:R Deformation
       EPXX : epsilon_xx deformation d'un milieu continu
       EPYY : epsilon_yy deformation d'un milieu continu
       EPZZ : epsilon_zz deformation d'un milieu continu
       EPXY : epsilon_xy deformation d'un milieu continu
       EPXZ : epsilon_xz deformation d'un milieu continu
       EPYZ : epsilon_yz deformation d'un milieu continu
       EXX coque : deformations generalisees
       EYY coque : deformations generalisees
       EXY coque : deformations generalisees
       KXX coque : deformations generalisees
       KYY coque : deformations generalisees
       KXY coque : deformations generalisees
       GAX coque : deformations generalisees
       GAY coque : deformations generalisees
       EPX Poutre: elongation selon l'axe de la poutre
       KY Poutre: courbure selon l'axe Y
       KZ Poutre: courbure selon l'axe Z
       INVA_2 : second invariant du tenseur de deformation
       PRIN_1 : deformation principale du tenseur direction 1
       PRIN_2 : deformation principale du tenseur direction 1
       PRIN_3 : deformation principale du tenseur direction 1
       INVA_2SG : second invariant signe du tenseur de deformation
       VECT_1_X : Composante selon Ox du 1er vecteur principal (EQUI_XXXX_XXXX )
       VECT_1_Y : Composante selon Oy du 1er vecteur principal (EQUI_XXXX_XXXX )
       ...
       VECT_3_Z : Composante selon Oz du 3eme vecteur principal (EQUI_XXXX_XXXX)
       EPTHER_L :
       EPTHER_T :
       EPTHER_N :
       EPSECH :
       EPHYDR :
       DEPV :
       DGONFX1 :
       DGONFX2 :
       DGONFX3 :
       DEPV11 :
       DEPV12 :
       DEPV13 :
       DEPV21 :
       DEPV22 :
       DEPV23 :
       DEPV31 :
       DEPV32 :
       DEPV33 :
       DV11X1 :
       DV11X2 :
       DV11X3 :
       DV12X1 :
       DV12X2 :
       DV12X3 :
       DV13X1 :
       DV13X2 :
       DV13X3 :
       DV21X1 :
       DV21X2 :
       DV21X3 :
       DV22X1 :
       DV22X2 :
       DV22X3 :
       DV23X1 :
       DV23X2 :
       DV23X3 :
       DV31X1 :
       DV31X2 :
       DV31X3 :
       DV32X1 :
       DV32X2 :
       DV32X3 :
       DV33X1 :
       DV33X2 :
       DV33X3 :
       PRES :
       PRES11 :
       PRES12 :
       PRES13 :
       PRES21 :
       PRES22 :
       PRES23 :
       PRES31 :
       PRES32 :
       PRES33 :
       EPPTOT : deformation liee a la variable de commande ptot en chainage HM
       DIVU   : dilatation volumique (chainage HM mecanique vers hydraulique)
""")


ERRE_R   = PhysicalQuantity(type='R',
    components=(
       'ERREST',
       'NUEST',
       'SIGCAL',
       'TERMRE',
       'TERMR2',
       'ERTABS',
       'ERTREL',
       'TERMNO',
       'TERMN2',
       'TERMVO',
       'TERMV2',
       'TERMV1',
       'TERMSA',
       'TERMS2',
       'TERMS1',
       'TERMFL',
       'TERMF2',
       'TERMF1',
       'TERMEC',
       'TERME2',
       'TERME1',
       'ESTERG[2]',
       'ERHMME_L',
       'ERHMMEDL',
       'ERHMHY_L',
       'ERHMME_G',
       'ERHMMEDG',
       'ERHMHY_G',
       'ERRETPS',
       'TAILLE',
    ),
    comment="""  EERREUR Type:R Calcul de l'erreur avec la methode des residus
       ERREST : erreur absolue estimee sur l'element
       NUEST : erreur relative estimee sur l'element
       SIGCAL : norme de l'energie des contraintes sur l'element
       TERMRE   : erreur absolue du residu de l'equation d'equilibre
       TERMR2   : erreur relative du residu de l'equation d'equilibre
       ERTABS   : erreur absolue pour la thermique
       ERTREL   : erreur relative pour la thermique
       TERMNO   : erreur absolue des sauts entre les contraintes normales
                  et la force de Neumann
       TERMN2   : erreur relative des sauts entre les contraintes normales
                  et la force de Neumann
       TERMVO   : xx
       TERMV2   : xx
       TERMV1   : xx
       TERMSA   : erreur absolue des sauts de contraintes sur chaque bord
       TERMS2   : erreur relative des sauts de contraintes sur chaque bord
       TERMS1   : xx
       TERMFL   : xx
       TERMF2   : xx
       TERMF1   : xx
       TERMEC   : xx
       TERME2   : xx
       TERME1   : xx
       ESTERG1  : 1ere ESTimation de l'ERreur Globale (stabilite)
       ESTERG2  : 2eme ESTimation de l'ERreur Globale (dualite)
       ERHMME_L : Erreur en Residu en espace pour le HM
                  - equation MEcanique - Locale en temps
       ERHMMEDL : Erreur en Residu en espace pour le HM
                  - equation MEcanique Derivee - Locale en temps
       ERHMHY_L : Erreur en Residu en espace pour le HM
                  - equation HYdraulique - Locale en temps
                  Pour le stationnaire - indicateur non booste
       ERHMME_G : Erreur en Residu en espace pour le HM
                  - equation MEcanique - Globale en temps
       ERHMMEDG : Erreur en Residu en espace pour le HM
                  - equation MEcanique Derivee - Globale en temps
       ERHMHY_G : Erreur en Residu en espace pour le HM
                  - equation HYdraulique - Globale en temps
                  Pour le stationnaire - indicateur booste
       ERRETPS  : ERreur en REsidu en TemPS
       TAILLE   : taille des mailles selon la definition de la routine uthk.f
""")


FACY_R   = PhysicalQuantity(type='R',
    components=(
       'DTAUM1',
       'VNM1X',
       'VNM1Y',
       'VNM1Z',
       'SINMAX1',
       'SINMOY1',
       'EPNMAX1',
       'EPNMOY1',
       'SIGEQ1',
       'NBRUP1',
       'ENDO1',
       'DTAUM2',
       'VNM2X',
       'VNM2Y',
       'VNM2Z',
       'SINMAX2',
       'SINMOY2',
       'EPNMAX2',
       'EPNMOY2',
       'SIGEQ2',
       'NBRUP2',
       'ENDO2',
       'VMIS',
       'TRESCA',
    ),
    comment="""  FACY_R  Type:R Grandeur liee a la fatigue a grand nombre de cycles sous
       chargement multiaxial
       DTAUM1 : Premiere valeur de la demi amplitude max du cisaillement dans
       le plan critique
       VNM1X : composante X du vecteur normal au plan critique
       correspondant a DTAUM1
       VNM1Y : composante Y du vecteur normal au plan critique
       correspondant a DTAUM1
       VNM1Z : composante Z du vecteur normal au plan critique
       correspondant a DTAUM1
       SINMAX1 : contrainte maximale normale au plan critique
       correspondant a DTAUM1
       SINMOY1 : contrainte moyenne normale au plan critique
       correspondant a DTAUM1
       EPNMAX1 : deformation maximale normale au plan critique
       correspondant a DTAUM1
       EPNMOY1 : deformation moyenne normale au plan critique
       correspondant a DTAUM1
       SIGEQ1 : contrainte equivalente associee a DTAUM1,
       NBRUP1 : nombre de cycles avant rupture (fonction de SIGEQ1 et
       d'une courbe de WOHLER)
       ENDO1 : endommagement associe a NBRUP1 (ENDO1=1/NBRUP1)
       DTAUM2 : seconde valeur de la demi amplitude max du cisaillement dans
       le plan critique
       VNM2X : composante X du vecteur normal au plan critique
       correspondant a DTAUM2
       VNM2Y : composante Y du vecteur normal au plan critique
       correspondant a DTAUM2
       VNM2Z : composante Z du vecteur normal au plan critique
       correspondant a DTAUM2
       SINMAX2 : contrainte maximale normale au plan critique
       correspondant a DTAUM2
       SINMOY2 : contrainte moyenne normale au plan critique
       correspondant a DTAUM2
       EPNMAX2 : deformation maximale normale au plan critique
       correspondant a DTAUM2
       EPNMOY2 : deformation moyenne normale au plan critique
       correspondant a DTAUM2
       SIGEQ2 : contrainte equivalente associee a DTAUM2,
       NBRUP2 : nombre de cycles avant rupture
       (fonction de SIGEQ2 et d'une courbe de WOHLER)
       ENDO2 : Endommagement associe a NBRUP2 (ENDO2=1/NBRUP2)
       VMIS : mesure d'amplitude de contrainte selon le critere de VON MISES
       TRESCA : mesure d'amplitude de contrainte selon le critere de TRESCA
""")


FELECR   = PhysicalQuantity(type='R',
    components=(
       'X1',
       'Y1',
       'Z1',
       'X2',
       'Y2',
       'Z2',
       'CODE',
    ),
    comment="""  FELECR Type:R Application de la force de Laplace ( FORCE_ELEC )
       X1 : coordonnee X d'un point du conducteur 1
       Y1 : coordonnee Y d'un point du conducteur 1
       Z1 : coordonnee Z d'un point du conducteur 1
       X2 : coordonnee X d'un point du conducteur 2
       Y2 : coordonnee Y d'un point du conducteur 2
       Z2 : coordonnee Z d'un point du conducteur 2
       CODE = 10 : composantes de la force Laplace (X1, Y1, Z1 et X2=Y2=Y3=0)
            = 11 : TRANS
            = 12 : DIST
            = 2 : INFI
            = 3 : FINI
""")


FER1_R   = PhysicalQuantity(type='R',
    components=(
       'TYPCOMB',
       'ENROBG',
       'CEQUI',
       'SIGACI',
       'SIGBET',
       'PIVA',
       'PIVB',
       'ES',
    ),
    comment=""" FER1_R et FER2_R : 2 grandeurs utilisees par CALC_FERRAILLAGE
""")


FER2_R   = PhysicalQuantity(type='R',
    components=(
       'DNSXI',
       'DNSXS',
       'DNSYI',
       'DNSYS',
       'DNST',
       'SIGMBE',
       'EPSIBE',
    ),)


FISS_R   = PhysicalQuantity(type='R',
    components=(
       'XA',
       'YA',
       'XTAN',
       'YTAN',
       'XNORM',
       'YNORM',
    ),
    comment="""  FISS_R Type:R Noeud du fond de fissure et sa normale
    (calcul du K1, K2 en 2D)
       XA : coordonnee 1 du noeud du fond de fissure
       YA : coordonnee 2 du noeud du fond de fissure
       XTAN : composante 1 de la tangente a la fissure
       YTAN : composante 2 de la tangente a la fissure
       XNORM : composante 1 de la normale a la fissure
       YNORM : composante 2 de la normale a la fissure
""")


FLAPLA   = PhysicalQuantity(type='K24',
    components=(
       'NOMAIL',
       'NOGEOM',
    ),
    comment="""  FLAPLA Type:K24 Inductance mutuelle entre 2 circuits filaires
       NOMAIL : nom du maillage
       NOGEOM : champ de geometrie
""")


FLHN_R   = PhysicalQuantity(type='R',
    components=(
       'FH1[2]',
       'FH2[2]',
    ),
    comment="""  FLHN_R Type:R Flux hydraulique sortant par unite de surface
       FH11 : valeur du flux sortant
       FH12 : valeur du flux sortant
       FH21 : valeur du flux sortant
       FH22 : valeur du flux sortant
""")


FLUN_F   = PhysicalQuantity(type='K8',
    components=(
       'FLUN',
       'FLUN_INF',
       'FLUN_SUP',
    ),
    comment="""  FLUN_F Type:K8 Flux de chaleur sortant par unite de surface
    PHI = (-lambda.laplacien(T)).n (c'est un scalaire)
       FLUN : valeur du flux sortant
       FLUN_INF : valeur du flux sortant par la face inferieure d'une coque
       FLUN_SUP : valeur du flux sortant par la face superieure d'une coque
""")


FLUN_R   = PhysicalQuantity(type='R',
    components=(
       'FLUN',
       'FLUN_INF',
       'FLUN_SUP',
    ),
    comment="""  FLUN_R Type:R Flux de chaleur sortant par unite de surface
    PHI = (-lambda.laplacien(T)).n  (c'est un scalaire)
       FLUN : valeur du flux sortant
       FLUN_INF : valeur du flux sortant par la face inferieure d'une coque
       FLUN_SUP : valeur du flux sortant par la face superieure d'une coque
""")


FLUX_F   = PhysicalQuantity(type='K8',
    components=(
       'FLUX',
       'FLUY',
       'FLUZ',
    ),
    comment="""  FLUX_F Type:K8 Flux vectoriel de chaleur en un point materiel du
    domaine continu : PHI = -lambda.laplacien(T)
       FLUX : composante suivante OX de PHI
       FLUY : composante suivante OY de PHI
       FLUZ : composante suivante OZ de PHI
""")


FLUX_R   = PhysicalQuantity(type='R',
    components=(
       'FLUX',
       'FLUY',
       'FLUZ',
       'FLUX_INF',
       'FLUY_INF',
       'FLUZ_INF',
       'FLUX_SUP',
       'FLUY_SUP',
       'FLUZ_SUP',
    ),
    comment=""" FLUX_R Type:R Flux vectoriel de chaleur en un point materiel du
   domaine continu : PHI = -lambda.laplacien(T).n
      FLUX : composante suivante OX de f
      FLUY : composante suivante OY de f
      FLUZ : composante suivante OZ de f
      FLUX_INF : flux sur un point de la face inferieure des coques
      FLUY_INF : flux sur un point de la face inferieure des coques
      FLUZ_INF : flux sur un point de la face inferieure des coques
      FLUX_SUP : flux sur un point de la face superieure des coques
      FLUY_SUP : flux sur un point de la face superieure des coques
      FLUZ_SUP : flux sur un point de la face superieure des coques
""")


FORC_C   = PhysicalQuantity(type='C',
    components=(
       'FX',
       'FY',
       'FZ',
       'MX',
       'MY',
       'MZ',
       'BX',
       'REP',
       'ALPHA',
       'BETA',
       'GAMMA',
       'PLAN',
    ),
    comment="""  FORC_C Type:C Force complexe (ponctuelle, lineique, surfacique ou
    volumique) appliquee sur un modele mecanique
       FX : composante suivant OX de la force
       FY : composante suivant OY de la force
       FZ : composante suivant OZ de la force
       MX : moment suivant OX
       MY : moment suivant OY
       MZ : moment suivant OZ
       BX : bi-moment pour les elements de poutre avec gauchissement
       REP : indicateur de repere : si REP = le repere OXYZ est le repere
       global sinon le repere est le repere local lie a l'element
       ALPHA : angles nautiques
       BETA : angles nautiques
       GAMMA : angles nautiques
       PLAN :
""")


FORC_F   = PhysicalQuantity(type='K8',
    components=(
       'FX',
       'FY',
       'FZ',
       'MX',
       'MY',
       'MZ',
       'BX',
       'REP',
       'ALPHA',
       'BETA',
       'GAMMA',
       'PLAN',
    ),
    comment="""  FORC_F Type:K8 Force (ponctuelle, lineique, surfacique ou volumique)
    appliquee sur un modele mecanique
       FX composante suivant OX de la force
       FY composante suivant OY de la force
       FZ composante suivant OZ de la force
       MX moment suivant OX
       MY moment suivant OY
       MZ moment suivant OZ
       BX bi-moment pour les elements de poutre avec gauchissement
       REP indicateur de repere : si REP = le repere OXYZ est le repere
       global sinon le repere est le repere local lie a l'element
       ALPHA angles nautiques
       BETA angles nautiques
       GAMMA angles nautiques
       PLAN :
""")


FORC_R   = PhysicalQuantity(type='R',
    components=(
       'FX',
       'FY',
       'FZ',
       'MX',
       'MY',
       'MZ',
       'BX',
       'REP',
       'ALPHA',
       'BETA',
       'GAMMA',
       'PLAN',
    ),
    comment="""  FORC_R Type:R Force (ponctuelle, lineique, surfacique ou volumique)
    appliquee sur un modele mecanique
       FX : composante suivant OX de la force
       FY : composante suivant OY de la force
       FZ : composante suivant OZ de la force
       MX : moment suivant OX
       MY : moment suivant OY
       MZ : moment suivant OZ
       BX : bi-moment pour les elements de poutre avec gauchissement
       REP : indicateur de repere : si REP = le repere OXYZ est le repere
       global sinon le repere est le repere local lie a l'element
       ALPHA : angles nautiques
       BETA : angles nautiques
       GAMMA : angles nautiques
       PLAN :
""")


FREQ_R   = PhysicalQuantity(type='R',
    components=(
       'FREQ',
    ),
    comment="""  FREQ_R Type:R Frequence (reelle)
       FREQ : valeur de la frequence
""")


FTHM_F   = PhysicalQuantity(type='K8',
    components=(
       'PFLU[2]',
       'PTHER',
    ),
    comment="""  FTHM_F Type:K8 Flux thermo-hydraulique (modelisation THM)
       PFLU1 :
       PFLU2 :
       PTHER : fonction du flux de chaleur

""")


FTHM_R   = PhysicalQuantity(type='R',
    components=(
       'PFLU[2]',
       'PTHER',
    ),
    comment="""  FTHM_R Type:R Flux thermo-hydraulique (modelisation THM)
       PFLU1 :
       PFLU2 :
       PTHER : fonction du flux de chaleur
""")


G        = PhysicalQuantity(type='R',
    components=(
       'GTHETA',
       'FIC[3]',
       'K[3]',
       'BETA',
    ),
    comment="""  G Type:R Taux de restitution de l'energie et coefficient d'intensite
    de contraintes
       GTHETA : Taux de restitution d'energie
       FIC1 : . . .
       FIC2 : . . .
       FIC3 : . . .
       K1 : Coefficient de contraintes K1
       K2 : Coefficient de contraintes K2
       K3 : Coefficient de contraintes K3
       BETA :
""")


GEOM_R   = PhysicalQuantity(type='R',
    components=(
       'X',
       'Y',
       'Z',
       'W',
    ),
    comment="""  GEOM_R Type:R Geometrie (d'un noeud)
       X : coordonnee suivant OX
       Y : coordonnee suivant OY
       Z : coordonnee suivant OZ (0. Si le modele est 2D)
       W :
""")


G_DEPL_R = PhysicalQuantity(type='R',
    components=(
       'D_DX_X',
       'D_DX_Y',
       'D_DX_Z',
       'D_DY_X',
       'D_DY_Y',
       'D_DY_Z',
       'D_DZ_X',
       'D_DZ_Y',
       'D_DZ_Z',
    ),
    comment="""  G_DEPL_R Type:R
        D_DX_X :
        D_DX_Y :
        D_DX_Z :
        D_DY_X :
        D_DY_Y :
        D_DY_Z :
        D_DZ_X :
        D_DZ_Y :
        D_DZ_Z :
""")


HARMON   = PhysicalQuantity(type='I',
    components=(
       'NH',
    ),
    comment="""  HARMON Type:I Harmonique de Fourier
       NH numero d'harmonique de Fourier
""")


HYDR_R   = PhysicalQuantity(type='R',
    components=(
       'HYDR',
    ),
    comment="""  HYDR_R Type:R
       HYDR :
""")


IMPE_C   = PhysicalQuantity(type='C',
    components=(
       'IMPE',
    ),
    comment="""  IMPE_C Type:C Impedance
       IMPE : valeur complexe de l'impedance
""")


IMPE_F   = PhysicalQuantity(type='K8',
    components=(
       'IMPE',
    ),
    comment="""  IMPE_F Type:K8 Impedance
       IMPE : fonction de l'impedance
""")


IMPE_R   = PhysicalQuantity(type='R',
    components=(
       'IMPE',
    ),
    comment="""  IMPE_R Type:R Impedance
       IMPE : valeur reelle de l'impedance
""")


INDL_R   = PhysicalQuantity(type='R',
    components=(
       'INDICE',
       'DIR[4]',
    ),
    comment="""  INDL_R type:R Indicateur de localisation
        INDICE : indicateur de contact
        DIR1   : premiere direction de localisation
        DIR2   : deuxieme direction de localisation
        DIR3   : troisieme direction de localisation
        DIR4   : quatrieme direction de localisation
""")


INFC_R   = PhysicalQuantity(type='R',
    components=(
       'CONT',
       'JEU',
       'RN',
       'RNX',
       'RNY',
       'RNZ',
       'GLIX',
       'GLIY',
       'GLI',
       'RTAX',
       'RTAY',
       'RTAZ',
       'RTGX',
       'RTGY',
       'RTGZ',
       'RX',
       'RY',
       'RZ',
       'R',
       'HN',
       'I',
       'IX',
       'IY',
       'IZ',
       'PT_X',
       'PT_Y',
       'PT_Z',
       'PROJ_X',
       'PROJ_Y',
       'PROJ_Z',
    ),
    comment="""  INFC_R Type:R Informations relatives au contact
       CONT   : indicateur de contact
       JEU    : jeu entre le noeud esclave et la maille maitre associee
       RN     : multiplicateur de lagrange et norme de RN
       RNX    : composante x du vecteur de force dues au contact
       RNY    : composante y du vecteur de force dues au contact
       RNZ    : composante z du vecteur de force dues au contact
       GLIX   : norme du deplacement tangent en x pour chaque liaison
       GLIY   : norme du deplacement tangent en y pour chaque liaison
       GLI    : norme du deplacement tangent pour chaque liaison
       RTAX   : composante x des forces des noeuds adherents
       RTAY   : composante y des forces des noeuds adherents
       RTAZ   : composante z des forces des noeuds adherents
       RTGX   : composante x des forces des noeuds glissants
       RTGY   : composante y des forces des noeuds glissants
       RTGZ   : composante z des forces des noeuds glissants
       RX     : composante x de la somme RN RTG ET RTA
       RY     : composante y de la somme RN RTG et RTA
       RZ     : composante z de la somme RN RTG et RTA
       R      : norme de R_TOT
       HN     : profondeur usure
       I      : percussion pour DYNA_NON_LINE
       IX     : Composante x de la percussion
       IY     : Composante y de la percussion
       IZ     : Composante z de la percussion
       PT_X   : coordonnee suivant x du point de contact
       PT_Y   : coordonnee suivant y du point de contact
       PT_Z   : coordonnee suivant z du point de contact
       PROJ_X   : coordonnee suivant x du projete du point de contact
       PROJ_Y   : coordonnee suivant y du projete du point de contact
       PROJ_Z   : coordonnee suivant z du projete du point de contact
""")


INST_R   = PhysicalQuantity(type='R',
    components=(
       'INST',
       'DELTAT',
       'THETA',
       'KHI',
       'R',
       'RHO',
    ),
    comment="""  INST_R Type:R Instant de calcul pour une evolution temporelle
       INST : valeur du temps (instant)
       DELTAT : increment de temps pour un calcul par ''pas de temps''
       THETA : parametre de THETA -methode (0 : explicite, 1 : implicite,
       THETA appartenant a )0,1( (semi-implicite)
       KHI : indicateur pour le calcul stationnaire ou transitoire
       R : parametre du lagrangien augmente
       RHO : parametre du lagrangien augmente
""")


INTE_R   = PhysicalQuantity(type='R',
    components=(
       'INTX_R',
       'INTY_R',
       'INTZ_R',
       'INTX_I',
       'INTY_I',
       'INTZ_I',
    ),
    comment="""  INTE_R Type:R Intensite acoustique
       INTX_R : partie reelle selon OX
       INTY_R : partie imaginaire selon OY
       ...
       INTZ_I : partie imaginaire selon OZ
""")


IRRA_R   = PhysicalQuantity(type='R',
    components=(
       'IRRA',
    ),
    comment="""  IRRA_R Type:R
       IRRA :
""")


J        = PhysicalQuantity(type='R',
    components=(
       'JINT',
       'JEXT',
       'JTH',
       'JAX',
       'JPR',
    ),
    comment="""  J Type:R
       JINT :
       JEXT :
       JTH :
       JAX :
       JPR :
""")


LISTMA   = PhysicalQuantity(type='K16',
    components=(
       'LISTMA',
       'TRANS',
    ),
    comment="""  LISTMA Type:K16 Definition des charges reparties sur une zone du maillage
                    (Force de LAPLACE)
       LISTMA : Liste des mailles affectees par le chargement
       TRANS : Nom d'un vecteur de travail decrivant le chargement
""")


MASS_R   = PhysicalQuantity(type='R',
    components=(
       'M',
       'CDGX',
       'CDGY',
       'CDGZ',
       'IXX',
       'IYY',
       'IZZ',
       'IXY',
       'IXZ',
       'IYZ',
       'IXR2',
       'IYR2',
    ),
    comment="""MASS_R Type:R Caracteristiques
     M : masse de l'element
     CDGX : coordonnee suivant X du C.D.G
     CDGY : coordonnee suivant Y
     CDGZ : coordonnee suivant Z
     IXX : inertie INT(y*y)
     IYY : inertie INT(x*x)
     IZZ : inertie INT(x*x+y*y) inertie polaire
     IXY : inertie INT(xy)
     IXZ : inertie INT(xz)
     IYZ : inertie INT(yz)
     EX : moment d'inertie principal X
     EY : moment d'inertie principal Y
     EZ : moment d'inertie principal Z
""")

MATE_R   = PhysicalQuantity(type='R',
    components=(
       'E',
       'NU',
       'RHO',
    ),
    comment="""  MATE_R Type:R Parmètres matériaux
       E   : module d'Young
       NU  : Coef. de Poisson
       RHO : Masse volumique
""")



MATE_F   = PhysicalQuantity(type='K8',
    components=(
       'MATE',
    ),
    comment="""  MATE_F Type:K8 Materiau
       MATE : nom du materiau affecte a une maille
""")

MULTCOMP = PhysicalQuantity(type='K16',
    components=(
       'SD_COMP',
    ),
    comment=""" Comportement pour *CRISTAL
       SD_COMP : nom de la SD issue de DEFI_COMPOR (MONOCRISTAL,..)
""")


N120_I   = PhysicalQuantity(type='I',
    components=(
       'X[120]',
    ),
    comment="""  N120_I Type:I Grandeur ''neutre'' de type I
       La signification des composantes varie d'une option a l'autre. Cette
       grandeur ''passe-partout'' ne sert qu'a eviter l'introduction de
       nombreuses grandeurs sans grand interet.
       X(1)   : composante 1
       ...
       X(120) : composante 120
""")


N120_R   = PhysicalQuantity(type='R',
    components=(
       'X[120]',
    ),
    comment="""  N120_R Type:R Grandeur ''neutre'' de type reel
       La signification des composantes varie d'une option a l'autre. Cette
       grandeur ''passe-partout'' ne sert qu'a eviter l'introduction de
       nombreuses grandeurs sans grand interet.
       X(1)  : composante 1
       ...
       X(120) : composante 120
""")


N1280I   = PhysicalQuantity(type='I',
    components=(
       'X[1280]',
    ),
    comment="""  N1280I Type:I Grandeur ''neutre'' de type I
       La signification des composantes varie d'une option a l'autre. Cette
       grandeur ''passe-partout'' ne sert qu'a eviter l'introduction de
       nombreuses grandeurs sans grand interet.
       X(1)   : composante 1
       ...
       X(1280) : composante 1280
""")


N132_R   = PhysicalQuantity(type='R',
    components=(
       'X[132]',
    ),
    comment="""  N132_R Type:R Grandeur ''neutre'' de type reel
       La signification des composantes varie d'une option a l'autre. Cette
       grandeur ''passe-partout'' ne sert qu'a eviter l'introduction de
       nombreuses grandeurs sans grand interet.
       X(1)  : composante 1
       ...
       X(132) : composante 132
""")


N1360R   = PhysicalQuantity(type='R',
    components=(
       'X[1280]',
    ),)


N1920R   = PhysicalQuantity(type='R',
    components=(
       'X[1920]',
    ),)


N240_I   = PhysicalQuantity(type='I',
    components=(
       'X[240]',
    ),)


N2448R   = PhysicalQuantity(type='R',
    components=(
       'X[2304]',
    ),)


N480_I   = PhysicalQuantity(type='I',
    components=(
       'X[480]',
    ),
    comment="""  N480_I Type:I Grandeur ''neutre'' de type I
       La signification des composantes varie d'une option a l'autre. Cette
       grandeur ''passe-partout'' ne sert qu'a eviter l'introduction de
       nombreuses grandeurs sans grand interet.
       X(1)   : composante 1
       ...
       X(480) : composante 480
""")


N480_R   = PhysicalQuantity(type='R',
    components=(
       'X[480]',
    ),
    comment="""  N480_R Type:R Grandeur ''neutre'' de type reel
       La signification des composantes varie d'une option a l'autre. Cette
       grandeur ''passe-partout'' ne sert qu'a eviter l'introduction de
       nombreuses grandeurs sans grand interet.
       X(1)  : composante 1
       ...
       X(480) : composante 480
""")


N512_I   = PhysicalQuantity(type='I',
    components=(
       'X[512]',
    ),
    comment="""  N512_I Type:I Grandeur ''neutre'' de type I
       La signification des composantes varie d'une option a l'autre. Cette
       grandeur ''passe-partout'' ne sert qu'a eviter l'introduction de
       nombreuses grandeurs sans grand interet.
       X(1)   : composante 1
       ...
       X(512) : composante 512
""")


N720_I   = PhysicalQuantity(type='I',
    components=(
       'X[720]',
    ),)


N792_R   = PhysicalQuantity(type='R',
    components=(
       'X[792]',
    ),)


N816_R   = PhysicalQuantity(type='R',
    components=(
       'X[816]',
    ),)


N960_I   = PhysicalQuantity(type='I',
    components=(
       'X[960]',
    ),
    comment="""  N1080I Type:I Grandeur ''neutre'' de type I
       La signification des composantes varie d'une option a l'autre. Cette
       grandeur ''passe-partout'' ne sert qu'a eviter l'introduction de
       nombreuses grandeurs sans grand interet.
       X(1)   : composante 1
       ...
       X(1080) : composante 1080
""")


NBSP_I   = PhysicalQuantity(type='I',
    components=(
       'NBFIBR',
       'COQ_NCOU',
       'TUY_NCOU',
       'TUY_NSEC',
       'NBGRFI',
       'TYGRFI',
       'NBCARMAX',
       'NUG[10]',
    ),
    comment="""  NBSP_I Type:I Caracteristiques des elements de structure a sous-points
        COQ_NCOU : nombre de couches (COQUE)
        TUY_NCOU : nombre de couches (TUYAU)
        TUY_NSEC : nombre de secteurs (TUYAU)
        NBFIBR   : nombre de fibres au total sur la section
        NBGRFI   : nombre de groupes de fibres (Poutre MULTI-FIBRE)
        TYGRFI   : type du groupe de fibres
        NBCARMAX : nombre de caracteristique maximum pour une fibre
        NUG1,..  : numeros des groupes de fibres (DE 1 A 10)
""")


NEUT_F   = PhysicalQuantity(type='K8',
    components=(
       'X[30]',
    ),
    comment="""  NEUT_F Type:K8 Grandeur ''neutre'' de type K8
       La signification des composantes varie d'une option a l'autre. Cette
       grandeur ''passe-partout'' ne sert qu'a eviter l'introduction de
       nombreuses grandeurs sans grand interet.
       X(1)  : composante 1
       ...
       X(30) : composante 30   !!! meme nombre d'entiers codes que NEUT_R
""")


NEUT_I   = PhysicalQuantity(type='I',
    components=(
       'X[30]',
    ),
    comment="""  NEUT_I Type:I Grandeur ''neutre'' de type I
       La signification des composantes varie d'une option a l'autre. Cette
       grandeur ''passe-partout'' ne sert qu'a eviter l'introduction de
       nombreuses grandeurs sans grand interet.
       X(1)   : composante 1
       ...
       X(30) : composante 30
""")


NEUT_K16 = PhysicalQuantity(type='K16',
    components=(
       'Z[30]',
    ),
    comment="""  NEUT_K16 Type:K16 Grandeur ''neutre'' de type K16
       La signification des composantes varie d'une option a l'autre. Cette
       grandeur ''passe-partout'' ne sert qu'a eviter l'introduction de
       nombreuses grandeurs sans grand interet.
       Z(1)  : composante 1
       ...
       Z(30) : composante 30
""")


NEUT_K24 = PhysicalQuantity(type='K24',
    components=(
       'Z[30]',
    ),
    comment="""  NEUT_K24 Type:K24 Grandeur ''neutre'' de type K24.
       La signification des composantes varie d'une option a l'autre. Cette
       grandeur ''passe-partout'' ne sert qu'a eviter l'introduction de
       nombreuses grandeurs sans grand interet.
       Z(1)  : composante 1
       ...
       Z(30) : composante 30
""")


NEUT_K8  = PhysicalQuantity(type='K8',
    components=(
       'Z[30]',
    ),
    comment="""  NEUT_K8 Type:K8 Grandeur ''neutre'' de type K8
       La signification des composantes varie d'une option a l'autre. Cette
       grandeur ''passe-partout'' ne sert qu'a eviter l'introduction de
       nombreuses grandeurs sans grand interet.
       Z(1)  : composante 1
       ...
       Z(30) : composante 30
""")


NEUT_R   = PhysicalQuantity(type='R',
    components=(
       'X[30]',
    ),
    comment="""  NEUT_R Type:R Grandeur ''neutre'' de type reel
       La signification des composantes varie d'une option a l'autre. Cette
       grandeur ''passe-partout'' ne sert qu'a eviter l'introduction de
       nombreuses grandeurs sans grand interet.
       X(1)  : composante 1
       ...
       X(30) : composante 30
""")


NOMMATER = PhysicalQuantity(type='K8',
    components=(
       'MAT[26]',
       'LREF',
       'VREF[3]',
    ),
    comment="""  NOMMATER Type:K8 Nom des materiaux
       MAT1  : nom du materiau 1
       ...
       MAT28 : nom du materiau 28
       LREF  : Toujours egal a 'TREF=>'
       VREF1, VREF2, VREF3 : 3 K8 qui, concatenes, donne la valeur de TREF
                             (reel code sur 3 K8 au format '1PE22.15')
""")


NUMC_I   = PhysicalQuantity(type='I',
    components=(
       'NUMC',
       'ORDO',
       'ANGL',
    ),
    comment="""  NUMC_I Type:I Pour les coques multi-couches
       NUMC : Numero de la couche
       ORDO : Niveau de la couche (SUP=1, MOY=0, INF=-1)
       ANGL :
""")


NUMMOD   = PhysicalQuantity(type='I',
    components=(
       'NUM',
    ),
    comment="""  NUMMOD Type:I
       NUM : numero du mode
""")


OME2_R   = PhysicalQuantity(type='R',
    components=(
       'OMEG2',
    ),
    comment="""  OME2_R Type:R Pulsation (reelle)
       OMEG2 : valeur de la pulsation au carre (Rappel : omega = 2*Pi*freq)
""")


ONDE_F   = PhysicalQuantity(type='K8',
    components=(
       'PRES',
    ),
    comment="""  ONDE_F Type:K8 Terme d'amortissement en onde incidente imposee sur une face
    d'element
       PRES : Valeur de l'onde de type fonction
""")


ONDE_R   = PhysicalQuantity(type='R',
    components=(
       'PRES',
    ),
    comment="""  ONDE_R Type:R Terme d'amortissement en onde incidente imposee sur une face
    d'element
       PRES Valeur de l'onde de type reel
""")


PDIL_R   = PhysicalQuantity(type='R',
    components=(
       'A1_LC2',
    ),
    comment="""  PDIL_R type:R module de Rigidite de micro-dilatation
        A1_LC2 : Module de rigidite de micro-dilatation
""")


PESA_R   = PhysicalQuantity(type='R',
    components=(
       'G',
       'AG',
       'BG',
       'CG',
    ),
    comment="""  PESA_R Type:R Caracteristiques d'un chargement de pesanteur : intensite et
    direction
       G : valeur de l'acceleration de la pesanteur
       AG : composante suivant OX du vecteur unitaire donnant la direction de la
       pesanteur
       BG : composante suivant OY du vecteur unitaire donnant la direction de la
       pesanteur
       CG : composante suivant OZ du vecteur unitaire donnant la direction de la
       pesanteur
""")


PILO_K   = PhysicalQuantity(type='K16',
    components=(
       'TYPE',
    ),
    comment="""  PILO_K Type:K16
       TYPE :
""")


PILO_R   = PhysicalQuantity(type='R',
    components=(
       'A0',
       'A[3]',
       'ETA',
    ),
    comment="""  PILO_R Type:R
       A0 :
       A1 :
       A2 :
       A3 :
       ETA :
""")


POSI     = PhysicalQuantity(type='I',
    components=(
       'POS',
    ),
    comment="""  POSI Type:I Option de calcul de la matrice de Masse
       POS = 1 , option MASS_MECA
           = 0 , option MASS_MECA_DIAG
""")


PRAC_R   = PhysicalQuantity(type='R',
    components=(
       'PRES_R',
       'PRES_I',
       'DB',
    ),
    comment="""  PRAC_R Type:R Pression acoustique (phenomene Acoustique)
       PRES_R  partie reelle de la pression
       PRES_I  partie imaginaire de la pression
       DB      decibel
""")


PREC     = PhysicalQuantity(type='R',
    components=(
       'PREC',
       'SIGM',
       'EPSI',
       'FHYDR[2]',
       'FTHERM',
       'VARI',
       'EFFORT',
       'MOMENT',
       'DEPL',
       'LAG_GV',
       'DAMG',
       'PI',
    ),
    comment="""  PREC Type:R precision
       PREC : valeur de la precision
       SIGM :
       EPSI :
       FHYDR1 :
       FHYDR2 :
       FTHERM :
       VARI :
       EFFORT :
       MOMENT :
       DEPL :
       LAG_GV :
       DAMG
       PI     :
""")


PRES_C   = PhysicalQuantity(type='C',
    components=(
       'PRES',
       'CISA',
       'VX',
       'VY',
       'VZ',
       'LAGR',
    ),
    comment="""  PRES_C Type:C Chargement surfacique applique a un modele mecanique
   (PRES,CISA)
    Inconnue d'un probleme d'acoustique : (pression, vitesse du fluide)
       PRES : valeur de la pression
       CISA : cisaillement applique sur le bord d'un modele 2D
       VX : vitesse du fluide suivant OX
       VY: vitesse du fluide suivant OY
       VZ : vitesse du fluide suivant OZ
       LAGR : parametre de lagrange du a la dualisation des conditions aux
       limites
""")


PRES_F   = PhysicalQuantity(type='K8',
    components=(
       'PRES',
       'CISA',
    ),
    comment="""  PRES_F Type:K8 Chargement surfacique applique a un modele mecanique
    (PRES,CISA)
     inconnue d'un probleme d'acoustique : (pression, vitesse du fluide)
       PRES : valeur de la pression
       CISA : cisaillement applique sur le bord d'un modele 2D
""")


PRES_R   = PhysicalQuantity(type='R',
    components=(
       'PRES',
       'CISA',
       'VX',
       'VY',
       'VZ',
       'LAGR',
    ),
    comment="""  PRES_R Type:R Chargement surfacique applique a un modele mecanique
   (PRES,CISA)
    Inconnue d'un probleme d'acoustique : (pression, vitesse du fluide)
       PRES : valeur de la pression
       CISA : cisaillement applique sur le bord d'un modele 2D
       VX : vitesse du fluide suivant OX
       VY : vitesse du fluide suivant OY
       VZ : vitesse du fluide suivant OZ
       LAGR : parametre de lagrange du a la dualisation des conditions aux
       limites
""")


PRME_R   = PhysicalQuantity(type='R',
    components=(
       'DB',
    ),
    comment="""  PRME_R Type:R Pression acoustique (phenomene Mecanique)
       DB decibel
""")


RAYO_F   = PhysicalQuantity(type='K8',
    components=(
       'SIGMA',
       'EPSIL',
       'TPINF',
    ),
    comment="""  RAYO_F Type:K8
       SIGMA :
       EPSIL :
       TPINF :
""")


RAYO_R   = PhysicalQuantity(type='R',
    components=(
       'SIGMA',
       'EPSIL',
       'TPINF',
    ),
    comment="""  RAYO_R Type:R
       SIGMA :
       EPSIL :
       TPINF :
""")


RCCM_K   = PhysicalQuantity(type='K24',
    components=(
       'TB_TEMP',
       'TB_MOYE',
    ),
    comment="""  RCCM_K Type:K24 Grandeur pour le RCCM B3600
       TB_TEMP : table de releve de temperature
       TB_MOYE : table de releve de la moyenne
""")


RCCM_R   = PhysicalQuantity(type='R',
    components=(
       'C[3]',
       'K[3]',
       'TYPE',
       'E',
       'E_AMBI',
       'NU',
       'ALPHA',
       'E_REFE',
       'SM',
       'M_KE',
       'N_KE',
       'IY',
       'IZ',
       'D',
       'EP',
       'SN',
       'SN_3SM',
       'SALT',
       'U_TOTAL',
       'TYPEKE',
    ),
    comment="""  RCCM_R Type:R Grandeurs pour le RCCM B3600
       C1 : valeur indice de contraintes
       C2 : valeur indice de contraintes
       C3 : valeur indice de contraintes
       K1 : valeur indice de contraintes
       K2 : valeur indice de contraintes
       K3 : valeur indice de contraintes
       TYPE : type de maille
       E : module d'elasticite a temperature de calcul
       E_AMBI : module d'elasticite a temperature ambiante
       NU : coefficient de poisson a temperature ambiante
       ALPHA : coefficient de dilatation a temperature ambiante
       E_REFE : module d'young de reference
       SM : contrainte equivalente admissible du materiau
       M_KE : constante du materiau
       N_KE : constante du materiau
       IY : moment d'inertie principale par rapport a Y
       IZ : moment d'inertie principale par rapport a Z
       D : diametre de la tuyauterie
       EP : epaisseur de la tuyauterie
       SN : amplitude de variation des contraintes linearisees
       SN_3SM :
       SALT : amplitude de la contrainte
       U_TOTAL : facteur d'usage
       TYPEKE : type de calcul de KE: soit KE_MECA, soit KE_MIXTE
""")


RICE_TRA = PhysicalQuantity(type='R',
    components=(
       'TRIAX',
       'RSR0',
       'VOLU',
       'NUMEMA',
       'DEPSEQ',
    ),
    comment=""" RICE_TRA Type:R CHAM_ELEM de RICE_TRACEY
      TRIAX : taux de triaxialite sur la maille
      RSRO : taux de croissance sur la maille a l'instant courant
      VOLU : volume pris en compte
      NUMEMA : numero de la maille
      DEPSEQ : variation de la deformation plastique equivalente
""")


ROTA_R   = PhysicalQuantity(type='R',
    components=(
       'OME',
       'AR',
       'BR',
       'CR',
       'X',
       'Y',
       'Z',
    ),
    comment="""  ROTA_R Type:R Caracteristiques d'un chargement de rotation :
    vitesse et axe
       OME : vitesse angulaire
       AR : composante (suivant OX) de l'axe de rotation
       BR : composante (suivant OY) de l'axe de rotation
       CR : composante (suivant OZ) de l'axe de rotation
       X :
       Y :
       Z :
""")


SECTION  = PhysicalQuantity(type='R',
    components=(
       'AIRE',
       'XG',
       'YG',
       'ALPHA',
       'IX',
       'IY',
       'XC',
       'YC',
       'CF',
    ),
    comment="""  SECTION Type:R Caracteristiques d'une section de poutre
       AIRE : aire de la section
       XG : abscisse du centre de gravite (repere OXY du maillage de la section)
       YG : ordonnee du centre de gravite (repere OXY du maillage de la section)
       ALPHA : angle (OX,GX) si GX 1er axe principal d'inertie
       IX : ''inertie'' autour de GX ( INT(Y-YG )**2  sur S)
       IY : ''inertie'' autour de GY ( INT(X-YG )**2  sur S)
       XC : abscisse du centre de torsion (repere GXY d'inertie)
       YC : ordonnee du centre de torsion (repere GXY d'inertie)
       CF : ...
""")


SIEFMX_C = PhysicalQuantity(type='C',
    components=(
       'SIXXMIN',
       'SIXXMAX',
    ),
    comment="""  SIEFMX_C Type:C contraintes min et max sur section
       SIXXMIN : contrainte minimale
       SIXXMAX : contrainte maximale
""")


SIEFMX_R = PhysicalQuantity(type='R',
    components=(
       'SIXXMIN',
       'SIXXMAX',
    ),
    comment="""  SIEFMX_R Type:R contraintes min et max sur section
       SIXXMIN : contrainte minimale
       SIXXMAX : contrainte maximale
""")


SIEF_C   = PhysicalQuantity(type='C',
    components=(
       'SIXX',
       'SIYY',
       'SIZZ',
       'SIXY',
       'SIXZ',
       'SIYZ',
       'N',
       'VY',
       'VZ',
       'MT',
       'MFY',
       'MFZ',
       'BX',
       'NXX',
       'NYY',
       'NXY',
       'MXX',
       'MYY',
       'MXY',
       'QX',
       'QY',
       'FX',
       'FY',
       'FZ',
       'MX',
       'MY',
       'MZ',
       'SIGN',
       'SITX',
       'SITY',
       'SITZ',
       'VMIS',
       'TRESCA',
       'PRIN_[3]',
       'VMIS_SG',
       'SN',
       'SVY',
       'SVZ',
       'SMT',
       'SMFY',
       'SMFZ',
       'MASF',
       'ENTR',
       'DISS',
       'FLHX',
       'FLHY',
       'FLHZ',
       'FLUX',
       'FLUY',
       'FLUZ',
       'PM',
       'PMPB',
       'SIP',
       'SIPXX',
       'SIPYY',
       'SIPZZ',
       'SIPXY',
       'SIPXZ',
       'SIPYZ',
       'M11',
       'FH11',
       'FH11X',
       'FH11Y',
       'FH11Z',
       'ENT11',
       'M12',
       'FH12',
       'FH12X',
       'FH12Y',
       'FH12Z',
       'ENT12',
       'M21',
       'FH21',
       'FH21X',
       'FH21Y',
       'FH21Z',
       'ENT21',
       'M22',
       'FH22',
       'FH22X',
       'FH22Y',
       'FH22Z',
       'ENT22',
       'QPRIM',
       'FHTX',
       'FHTY',
       'FHTZ',
       'QXX',
       'QXY',
       'QYX',
       'QYY',
       'QZX',
       'QZY',
       'Q2XX',
       'Q2XY',
       'Q2YX',
       'Q2YY',
       'Q2ZX',
       'Q2ZY',
       'VECT_1_X',
       'VECT_1_Y',
       'VECT_1_Z',
       'VECT_2_X',
       'VECT_2_Y',
       'VECT_2_Z',
       'VECT_3_X',
       'VECT_3_Y',
       'VECT_3_Z',
       'PRES',
       'PRES1[3]',
       'PRES2[3]',
       'PRES3[3]',
       'SIG[3]',
       'SIG11[3]',
       'SIG12[3]',
       'SIG13[3]',
       'SIG21[3]',
       'SIG22[3]',
       'SIG23[3]',
       'SIG31[3]',
       'SIG32[3]',
       'SIG33[3]',
       'DEPV',
       'DEPV1[3]',
       'DEPV2[3]',
       'DEPV3[3]',
       'TRSIG',
       'CONT_X',
       'CONT_Y',
       'CONT_Z',
       'SIG_NX',
       'SIG_NY',
       'SIG_NZ',
       'SIG_N',
       'SIG_TX',
       'SIG_TY',
       'SIG_TZ',
       'SIG_T1X',
       'SIG_T1Y',
       'SIG_T1Z',
       'SIG_T1',
       'SIG_T2X',
       'SIG_T2Y',
       'SIG_T2Z',
       'SIG_T2',
       'LH1P',
       'LH1M',
       'DPRE1P',
       'DPRE1M',
       'TRIAX',
       'FSTAB[72]',
       'SIGV_A',
       'SIGV_L',
       'SIGV_GX',
       'SIGV_GY',
       'SIGV_GZ',
    ),
    comment="""  SIEF_C Type:C Etat de contrainte (ou d'effort interne)
       SIXX : sigma_xx contraintes dans un milieu continu
       SIYY : sigma_yy contraintes dans un milieu continu
       SIZZ : sigma_zz contraintes dans un milieu continu
       SIXY : sigma_xy contraintes dans un milieu continu
       SIXZ : sigma_xz contraintes dans un milieu continu
       SIYZ : sigma_yz contraintes dans un milieu continu
       N : effort normal
       VY : effort tranchant suivant CY efforts internes des poutres
       VZ : effort tranchant suivant CZ efforts internes des poutres
       MT : moment de torsion suivant CX
       MFY : moment de flexion suivant GY
       MFZ : moment de flexion suivant GZ
       BX : bi-moment (poutre avec gauchissement)
       NXX : efforts internes des coques
       NYY : efforts internes des coques
       NXY : efforts internes des coques
       MXX : efforts internes des coques
       MYY : efforts internes des coques
       MXY : efforts internes des coques
       QX : efforts internes des coques
       QY : efforts internes des coques
       FX : efforts pour les discrets, poutres, barres, cales, poulies
       FY : efforts pour les discrets, poutres, barres, cales, poulies
       FZ : efforts pour les discrets, poutres, barres, cales, poulies
       MX : efforts pour les discrets, poutres, barres, cales, poulies
       MY : efforts pour les discrets, poutres, barres, cales, poulies
       MZ : efforts pour les discrets, poutres, barres, cales, poulies
       SIGN : elements de contact : contrainte normale
       SITX : elements de contact : contrainte tangentielle
       SITY : elements de contact : contrainte tangentielle
       SITZ : elements de contact : contrainte tangentielle
       VMIS : contrainte de Von Mises
       TRESCA : tresca
       PRIN_1 : contrainte principale direction 1
       PRIN_2 : contrainte principale direction 2
       PRIN_3 : contrainte principale direction 3
       VMIS_SG : contrainte de Von Mises signee par la trace de sigma
       SN : contrainte dans la section de poutre due a l'effort normal
       SVY : contrainte dans la section de poutre due a l'effort tranchant VY
       SVZ : contrainte dans la section de poutre due a l'effort tranchant VZ
       SMT : contrainte dans la section de poutre due au moment de torsion MX
       SMFY : contrainte dans la section de poutre due au moment de flexion MY
       SMFZ : contrainte dans la section de poutre due au moment de flexion MZ
       MASF :
       ENTR :
       DISS :
       FLHX :
       FLHY :
       FLHZ :
       FLUX :
       FLUY :
       FLUZ :
       PM :
       PMPB :
       SIP :
       SIPXX : sigmap_xx contraintes de pression dans la massif
       SIPYY : sigmap_yy contraintes de pression dans la massif
       SIPZZ : sigmap_zz contraintes de pression dans la massif
       SIPXY : sigmap_xy contraintes de pression dans la massif
       SIPXZ : sigmap_xz contraintes de pression dans la massif
       SIPYZ : sigmap_yz contraintes de pression dans la massif
       M11:
       FH11:
       FH11X :
       FH11Y :
       FH11Z :
       ENT11 :
       M12 :
       FH12:
       FH12X :
       FH12Y :
       FH12Z :
       ENT12 :
       M21 :
       FH21:
       FH21X :
       FH21Y :
       FH21Z :
       ENT21 :
       M22 :
       FH22:
       FH22X :
       FH22Y :
       FH22Z :
       ENT22 :
       QPRIM :
       FHTX :
       FHTY :
       FHTZ :
       QXX :
       QXY :
       QYX :
       QYY :
       QZX :
       QZY :
       Q2XX :
       Q2XY :
       Q2YX :
       Q2YY :
       Q2ZX :
       Q2ZY :
       VECT_1_X :
       VECT_1_Y :
       VECT_1_Z :
       VECT_2_X :
       VECT_2_Y :
       VECT_2_Z :
       VECT_3_X :
       VECT_3_Y :
       VECT_3_Z
       PRES :
       PRES11 :
       PRES12 :
       PRES13 :
       PRES21 :
       PRES22 :
       PRES23 :
       PRES31 :
       PRES32 :
       PRES33 :
       SIG1 :
       SIG2 :
       SIG3 :
       SIG111 :
       SIG112 :
       SIG113 :
       SIG121 :
       SIG122 :
       SIG123 :
       SIG131 :
       SIG132 :
       SIG133 :
       SIG211 :
       SIG212 :
       SIG213 :
       SIG221 :
       SIG222 :
       SIG223 :
       SIG231 :
       SIG232 :
       SIG233 :
       SIG311 :
       SIG312 :
       SIG313 :
       SIG321 :
       SIG322 :
       SIG323 :
       SIG331 :
       SIG332 :
       SIG333 :
       DEPV   :
       DEPV11 :
       DEPV12 :
       DEPV13 :
       DEPV21 :
       DEPV22 :
       DEPV23 :
       DEPV31 :
       DEPV32 :
       DEPV33 :
       TRSIG  :
       CONT_X :
       CONT_Y :
       CONT_Z :
       LH1P   :
       LH1M   :
       DPRE1P :
       DPRE1M :
       TRIAX  : Taux de triaxialite
       FSTAB[72]  : Forces de stabilisation
""")


SIEF_R   = PhysicalQuantity(type='R',
    components=(
       'SIXX',
       'SIYY',
       'SIZZ',
       'SIXY',
       'SIXZ',
       'SIYZ',
       'N',
       'VY',
       'VZ',
       'MT',
       'MFY',
       'MFZ',
       'BX',
       'NXX',
       'NYY',
       'NXY',
       'MXX',
       'MYY',
       'MXY',
       'QX',
       'QY',
       'FX',
       'FY',
       'FZ',
       'MX',
       'MY',
       'MZ',
       'SIGN',
       'SITX',
       'SITY',
       'SITZ',
       'VMIS',
       'TRESCA',
       'PRIN_[3]',
       'VMIS_SG',
       'SN',
       'SVY',
       'SVZ',
       'SMT',
       'SMFY',
       'SMFZ',
       'MASF',
       'ENTR',
       'DISS',
       'FLHX',
       'FLHY',
       'FLHZ',
       'FLUX',
       'FLUY',
       'FLUZ',
       'PM',
       'PMPB',
       'SIP',
       'SIPXX',
       'SIPYY',
       'SIPZZ',
       'SIPXY',
       'SIPXZ',
       'SIPYZ',
       'M11',
       'FH11',
       'FH11X',
       'FH11Y',
       'FH11Z',
       'ENT11',
       'M12',
       'FH12',
       'FH12X',
       'FH12Y',
       'FH12Z',
       'ENT12',
       'M21',
       'FH21',
       'FH21X',
       'FH21Y',
       'FH21Z',
       'ENT21',
       'M22',
       'FH22',
       'FH22X',
       'FH22Y',
       'FH22Z',
       'ENT22',
       'QPRIM',
       'FHTX',
       'FHTY',
       'FHTZ',
       'QXX',
       'QXY',
       'QYX',
       'QYY',
       'QZX',
       'QZY',
       'Q2XX',
       'Q2XY',
       'Q2YX',
       'Q2YY',
       'Q2ZX',
       'Q2ZY',
       'VECT_1_X',
       'VECT_1_Y',
       'VECT_1_Z',
       'VECT_2_X',
       'VECT_2_Y',
       'VECT_2_Z',
       'VECT_3_X',
       'VECT_3_Y',
       'VECT_3_Z',
       'PRES',
       'PRES1[3]',
       'PRES2[3]',
       'PRES3[3]',
       'SIG[3]',
       'SIG11[3]',
       'SIG12[3]',
       'SIG13[3]',
       'SIG21[3]',
       'SIG22[3]',
       'SIG23[3]',
       'SIG31[3]',
       'SIG32[3]',
       'SIG33[3]',
       'DEPV',
       'DEPV1[3]',
       'DEPV2[3]',
       'DEPV3[3]',
       'TRSIG',
       'CONT_X',
       'CONT_Y',
       'CONT_Z',
       'SIG_NX',
       'SIG_NY',
       'SIG_NZ',
       'SIG_N',
       'SIG_TX',
       'SIG_TY',
       'SIG_TZ',
       'SIG_T1X',
       'SIG_T1Y',
       'SIG_T1Z',
       'SIG_T1',
       'SIG_T2X',
       'SIG_T2Y',
       'SIG_T2Z',
       'SIG_T2',
       'LH1P',
       'LH1M',
       'DPRE1P',
       'DPRE1M',
       'TRIAX',
       'FSTAB[72]',
       'SIGV_A',
       'SIGV_L',
       'SIGV_GX',
       'SIGV_GY',
       'SIGV_GZ',
    ),
    comment="""  SIEF_R Type:R Etat de contrainte (ou d'effort interne) (cf. (U2.01.04))
       SIXX : sigma_xx contraintes dans un milieu continu
       SIYY : sigma_yy contraintes dans un milieu continu
       SIZZ : sigma_zz contraintes dans un milieu continu
       SIXY : sigma_xy contraintes dans un milieu continu
       SIXZ : sigma_xz contraintes dans un milieu continu
       SIYZ : sigma_yz contraintes dans un milieu continu
       N : effort normal
       VY : effort tranchant suivant CY efforts internes des poutres
       VZ : effort tranchant suivant CZ efforts internes des poutres
       MT : moment de torsion suivant CX
       MFY : moment de flexion suivant GY
       MFZ : moment de flexion suivant GZ
       BX : bi-moment (poutre avec gauchissement)
       NXX : efforts internes des coques
       NYY : efforts internes des coques
       NXY : efforts internes des coques
       MXX : efforts internes des coques
       MYY : efforts internes des coques
       MXY : efforts internes des coques
       QX : efforts internes des coques
       QY : efforts internes des coques
       FX : efforts pour les discrets, poutres, barres, cales, poulies
       FY : efforts pour les discrets, poutres, barres, cales, poulies
       FZ : efforts pour les discrets, poutres, barres, cales, poulies
       MX : efforts pour les discrets, poutres, barres, cales, poulies
       MY : efforts pour les discrets, poutres, barres, cales, poulies
       MZ : efforts pour les discrets, poutres, barres, cales, poulies
       SIGN : elements de contact : contrainte normale
       SITX : elements de contact : contrainte tangentielle
       SITY : elements de contact : contrainte tangentielle
       SITZ : elements de contact : contrainte tangentielle
       VMIS : contrainte de Von Mises
       TRESCA : tresca
       PRIN_1 : contrainte principale direction 1
       PRIN_2 : contrainte principale direction 2
       PRIN_3 : contrainte principale direction 3
       VMIS_SG : contrainte de Von Mises signee par la trace de sigma
       SN : contrainte dans la section de poutre due a l'effort normal
       SVY : contrainte dans la section de poutre due a l'effort tranchant Vy
       SVZ : contrainte dans la section de poutre due a l'effort tranchant Vz
       SMT : contrainte dans la section de poutre due au moment de torsion Mx
       SMFY : contrainte dans la section de poutre due au moment de flexion My
       SMFZ : contrainte dans la section de poutre due au moment de flexion Mz
       MASF ********** composantes utilisees pour la THM ***********
       ENTR repeter : pour les elements ''THM''
       DISS repeter : pour les elements ''THM''
       FLHX repeter : pour les elements ''THM''
       FLHY repeter : pour les elements ''THM''
       FLHZ repeter : pour les elements ''THM''
       FLUX repeter : pour les elements ''THM''
       FLUY repeter : pour les elements ''THM''
       FLUZ repeter : pour les elements ''THM''
       VECT_1_X : Composante selon Ox du 1er vecteur principal (EQUI_XXXX_XXXX )
       VECT_1_Y : Composante selon Oy du 1er vecteur principal (EQUI_XXXX_XXXX )
       ... ...
       VECT_3_Z Composante selon Oz du 3eme vecteur principal ( EQUI_XXXX_XXXX )
       PRES :
       PRES11 :
       PRES12 :
       PRES13 :
       PRES21 :
       PRES22 :
       PRES23 :
       PRES31 :
       PRES32 :
       PRES33 :
       SIG1 :
       SIG2 :
       SIG3 :
       SIG111 :
       SIG112 :
       SIG113 :
       SIG121 :
       SIG122 :
       SIG123 :
       SIG131 :
       SIG132
       SIG133 :
       SIG211 :
       SIG212 :
       SIG213 :
       SIG221 :
       SIG222 :
       SIG223 :
       SIG231
       SIG232 :
       SIG233 :
       SIG311 :
       SIG312 :
       SIG313 :
       SIG321 :
       SIG322 :
       SIG323 :
       SIG331 :
       SIG332 :
       SIG333 :
       DEPV :
       DEPV11 :
       DEPV12 :
       DEPV13 :
       DEPV21 :
       DEPV22 :
       DEPV23 :
       DEPV31 :
       DEPV32 :
       DEPV33 :
       TRSIG :
       CONT_X :
       CONT_Y :
       CONT_Z :
       LH1P   :
       LH1M   :
       DPRE1P :
       DPRE1M :
       TRIAX  : Taux de triaxialite
       FSTAB[72]  : Forces de stabilisation
       SIGV_A : GRAD_VARI - COMPOSANTE ASSOCIEE A LA VARIABLE NON LOCALE
       SIGV_L : GRAD_VARI - COMPOSANTE ASSOCIEE AU LAGRANGE
       SIGV_GX: GRAD_VARI - COMPOSANTE ASSOCIEA AU GRADIENT SELON X
       SIGV_GY: GRAD_VARI - COMPOSANTE ASSOCIEA AU GRADIENT SELON Y
       SIGV_GZ: GRAD_VARI - COMPOSANTE ASSOCIEA AU GRADIENT SELON Z
       SIG_NX, SIG_NY, SIG_NZ, ...  ,SIG_T2Z, SIG_T2 : contraintes de "rosette"
""")


SING_R   = PhysicalQuantity(type='R',
    components=(
       'DEGRE',
       'RAPPORT',
       'TAILLE',
    ),
    comment="""  SING_R Type:R grandeurs liee au calcul de carte de taille
       DEGRE : ordre de la singularite
       RAPPORT : rapport de modification de taille
       TAILLE : nouvelle taille des elements
""")


SIZZ_R   = PhysicalQuantity(type='R',
    components=(
       'SIZZ',
    ),
    comment="""  SIZZ_R Type:R Grandeur simple servant a construire les grandeurs
    elementaires MSIZ_R et VSIZ_R pour le calcul de l'estimateur ZZ1
       SIZZ : coefficient de la matrice ou du vecteur
""")


SOUR_F   = PhysicalQuantity(type='K8',
    components=(
       'SOUR',
       'VNOR',
    ),
    comment="""  SOUR_F Type:K8 Source volumique de type fonction
       SOUR : valeur de la source volumique appliquee a une maille
       mot cle SOURCE de la commande AFFE_CHAR_THER_F
       VNOR : valeur de la vitesse normale appliquee a une face
       mot cle VITE_FACE de la commande AFFE_CHAR_MECA_F
""")


SOUR_R   = PhysicalQuantity(type='R',
    components=(
       'SOUR',
       'VNOR',
    ),
    comment="""  SOUR_R Type:R Source volumique de type reel
       SOUR : valeur de la source volumique appliquee a une maille
       mot cle SOURCE de la commande AFFE_CHAR_THER
       VNOR : valeur de la vitesse normale appliquee a une face
       mot cle VITE_FACE de la commande AFFE_CHAR_MECA
""")


SPMX_R   = PhysicalQuantity(type='R',
    components=(
       'VAL',
       'NUCOU',
       'NUSECT',
       'NUFIBR',
       'POSIC',
       'POSIS',
    ),
    comment="""  SPMX_R Type:R
       VAL    :   valeur "min/max" trouvee
       NUCOU  :   numero de la couche ou le "min/max" a ete atteint
       NUSECT :   numero du secteur ou le "min/max" a ete atteint
       NUFIBR :   numero de la fibre ou le "min/max" a ete atteint
       POSIC  :   position dans la couche  (-1 : INF, 0: MOY, 1: SUP)
       POSIS  :   position dans le secteur (-1 : INF, 0: MOY, 1: SUP)
""")


STAOUDYN = PhysicalQuantity(type='R',
    components=(
       'STAOUDYN',
       'ALFNMK',
       'DELNMK',
    ),
    comment="""  STAOUDYN Type:R Parametres de Newmark si calcul dynamique
       STAOUDYN = 0 : statique
                = 1 : dynamique
       ALFNMK : parametre de Newmark ALPHA
       DELNMK : parametre de Newmark DELTA
""")


STRX_R   = PhysicalQuantity(type='R',
    components=(
       'N',
       'VY',
       'VZ',
       'MT',
       'MFY',
       'MFZ',
       'BX',
       'EPXX',
       'GAXY',
       'GAXZ',
       'GAT',
       'KY',
       'KZ',
       'GAX',
       'DXINT',
       'ALPHA',
       'BETA',
       'GAMMA',
    ),
    comment="""  STRX_R  Type:R Champs special pour les elements de structure
       N : effort normal
       VY : effort tranchant suivant CY efforts internes des poutres
       VZ : effort tranchant suivant CZ efforts internes des poutres
       MT : moment de torsion suivant CX
       MFY : moment de flexion suivant GY
       MFZ : moment de flexion suivant GZ
       BX : bi-moment (poutre avec gauchissement)
       EPXX : increment de deformation axiale cumule
       GAXY : increment de deformation de cisaillement suivant Y cumule
       GAXZ : increment de deformation de cisaillement suivant  Z cumule
       GAT : increment de deformation de torsion cumule
       KY : increment de courbure autour de Y cumule
       KZ : increment de courbure autour de Z cumule
       GAX : increment de deformation de gauchissement cumule
       DXINT : deplacement axial du noeud bulle interne
       ALPHA : premier angle nautique reactualise
       BETA  : deuxieme angle nautique reactualise
       GAMMA : troisieme angle nautique reactualise

""")


TEMP_C   = PhysicalQuantity(type='C',
    components=(
       'TEMP',
       'TEMP_MIL',
       'TEMP_INF',
       'TEMP_SUP',
       'LAGR',
    ),
    comment="""  TEMP_C Type:C Temperature inconnue du phenomene thermique
       TEMP : temperature
       TEMP_MIL : temperature sur le feuillet moyen  (coques)
       TEMP_INF : temperature sur la face inferieure (coques)
       TEMP_SUP : temperature sur la face superieure (coques)
       LAGR : parametre de lagrange du a la dualisation des conditions aux
       limites
""")


TEMP_F   = PhysicalQuantity(type='K8',
    components=(
       'TEMP',
       'TEMP_MIL',
       'TEMP_INF',
       'TEMP_SUP',
       'LAGR',
    ),
    comment="""  TEMP_F Type:K8 Temperature inconnue du phenomene thermique
       TEMP : temperature
       TEMP_MIL : temperature sur le feuillet moyen  (coques)
       TEMP_INF : temperature sur la face inferieure (coques)
       TEMP_SUP : temperature sur la face superieure (coques)
       LAGR : parametre de lagrange du a la dualisation des
       conditions aux limites
""")


TEMP_R   = PhysicalQuantity(type='R',
    components=(
       'TEMP',
       'TEMP_MIL',
       'TEMP_INF',
       'TEMP_SUP',
       'LAGR',
       'H1',
       'E1',
       'DTX',
       'DTY',
       'DTZ',
    ),
    comment="""  TEMP_R Type:R Temperature inconnue du phenomene thermique
       TEMP : temperature
       TEMP_MIL : temperature sur le feuillet moyen  (coques)
       TEMP_INF : temperature sur la face inferieure (coques)
       TEMP_SUP : temperature sur la face superieure (coques)
       LAGR : parametre de lagrange du a la dualisation des
       conditions aux limites
       H1 : ddl Heaviside X-FEM
       E1 : ddl Cracktip X-FEM
       DTX : derivee de la temperature selon x (n'est pas un ddl)
       DTY : derivee de la temperature selon y (n'est pas un ddl)
       DTZ : derivee de la temperature selon z (n'est pas un ddl)
""")


VALO_R   = PhysicalQuantity(type='R',
    components=(
       'VALEUR',
       'GRAD_X',
       'GRAD_Y',
       'GRAD_Z',
    ),
    comment="""  VALO_R Type:R
       VALEUR :
       GRAD_X :
       GRAD_Y :
       GRAD_Z :
""")


VAR2_R   = PhysicalQuantity(type='R',
    components=(
       'V[200]',
    ),
    comment="""  VAR2_R Type:R Composantes du CHAM_NO de type variables internes
    (passage d'un CHAM_ELEM a un CHAM_NO )
       V(1) variable interne 1
       V(2) variable interne 2
       V(3) variable interne 3
       ...
""")


VARC_R   = PhysicalQuantity(type='R',
    components=(
       'TEMP',
       'HYDR',
       'SECH',
       'IRRA',
       'CORR',
       'PTOT',
       'DIVU',
       'NEUT[2]',
    ),
    comment="""  VARC_R Type:R Variables de commande (nommees) :
       TEMP : temperature
       HYDR : hydratation
       SECH : sechage
       IRRA : irradiation
       CORR : corrosion
       PTOT : pression totale de fluide (chainage HM)
       DIVU : dilatation volumique (chainage HM)
       NEUT1 : "neutre 1" (definie par l'utilisateur)
       NEUT2 : "neutre 2" (definie par l'utilisateur)
""")


VARI_R   = PhysicalQuantity(type='R',
    components=(
       'VARI',
    ),
    comment="""  VARI_R Type:R Variables internes pour les lois de comportement
    non-lineaires
    Attention : le nombre de variables interne pouvant varier fortement selon
    les modeles de comportement (de 1 a 50 par exemple), on ne distingue
    pas les composantes de cette grandeur. Le nombre reel de composantes
    est donne en dehors des catalogues.
       VARI : nom conventionnel des composantes : VARI_1 , VARI_2 , ...
""")


VENTCX_F = PhysicalQuantity(type='K8',
    components=(
       'FCXP',
    ),
    comment="""  VENTCX_F Type:K8
       FCXP :
""")


VNOR_C   = PhysicalQuantity(type='C',
    components=(
       'VNOR',
    ),
    comment="""  VNOR_C Type:C Vitesse normale appliquee a une face de maille (acoustique)
       VNOR : valeur de la vitesse normale
""")


VNOR_F   = PhysicalQuantity(type='K8',
    components=(
       'VNOR',
    ),
    comment="""  VNOR_F Type:K8 Vitesse normale appliquee a une face de maille (acoustique)
       VNOR : fonction de la vitesse normale
""")


VOISIN   = PhysicalQuantity(type='I',
    components=(
       'V0',
       'V[6]',
       'T0',
       'T[6]',
    ),
    comment="""  VOISIN Type:I Elements voisins et leurs types pour l'estimateur d'erreur
       V0 : numero de l'element voisin 1
       V1 : numero de l'element voisin 2
       V2 : numero de l'element voisin 3
       V3 : numero de l'element voisin 4
       V4 : numero de l'element voisin 5
       V5 : numero de l'element voisin 6
       V6 : numero de l'element voisin 7
       T0 : type de l'element voisin 1
       T1 : type de l'element voisin 2
       T2 : type de l'element voisin 3
       T3 : type de l'element voisin 4
       T4 : type de l'element voisin 5
       T5 : type de l'element voisin 6
       T6 : type de l'element voisin 7
""")


WEIBULL  = PhysicalQuantity(type='R',
    components=(
       'DSIGWB',
    ),
    comment="""  WEIBULL Type:R Contrainte de Weibull
       DSIGWB : valeur de la contrainte
""")


XCONTAC  = PhysicalQuantity(type='R',
    components=(
       'RHON',
       'MU',
       'RHOTK',
       'INTEG',
       'COECH',
       'COSTCO',
       'COSTFR',
       'COPECO',
       'COPEFR',
       'RELA',
    ),
    comment="""  XCONTAC Type:R Contact avec X-FEM
       RHON   : COEF_REGU_CONT
       MU     : Coefficient de frottement de coulomb
       RHOTK  : COEF_REGU_FROT/DELTAT
                ou DELTAT est l'increment de temps pour
                la discretisation eulerienne des vitesses
       INTEG  : schema d'integration (GAUSS OU NOEUD)
       COECH  : coefficient de mise a l'echelle des pressions de contact
       CSTCO  : coefficient de stabilisation du contact, COEF_STAB_CONT
       CSTFR  : coefficient de stabilisation du frottement, COEF_STAB_CONT
       CPECO  : coefficient de penalisation du contact, COEF_PENA_CONT
       CPEFR  : coefficient de penalisation du frottement, COEF_PENA_CONT
       RELA   : gere l''activation des lois cohesives avec X-FEM
""")


# Elementary Quantities
MDEP_C   = ArrayOfQuantities(elem='MS', phys= DEPL_C)
MDEP_R   = ArrayOfQuantities(elem='MS', phys= DEPL_R)
MDNS_R   = ArrayOfQuantities(elem='MR', phys= DEPL_R)
MPRE_C   = ArrayOfQuantities(elem='MS', phys= PRES_C)
MSIZ_R   = ArrayOfQuantities(elem='MS', phys= SIZZ_R)
MTEM_R   = ArrayOfQuantities(elem='MS', phys= TEMP_R)
MTNS_R   = ArrayOfQuantities(elem='MR', phys= TEMP_R)
MZNS_R   = ArrayOfQuantities(elem='MR', phys= NEUT_R)
VDEP_C   = ArrayOfQuantities(elem='V', phys= DEPL_C)
VDEP_R   = ArrayOfQuantities(elem='V', phys= DEPL_R)
VPRE_C   = ArrayOfQuantities(elem='V', phys= PRES_C)
VSIZ_R   = ArrayOfQuantities(elem='V', phys= SIZZ_R)
VTEM_R   = ArrayOfQuantities(elem='V', phys= TEMP_R)


# store all PhysicalQuantity & ElementaryQuantity objects
PHYSQUANTS = objects_from_context(globals(), PhysicalQuantity)
ELEMQUANTS = objects_from_context(globals(), ArrayOfQuantities)
