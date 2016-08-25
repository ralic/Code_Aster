module cara_elem_parameter_module
!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
! --------------------------------------------------------------------------------------------------
!
!   Pour l'opérateur AFFE_CARA_ELEM
!
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
    implicit none
!
    integer, parameter :: ACE_NOTHING        =  0
!
! Nombre total de mots clefs facteur
    integer, parameter :: ACE_NB_MCLEF       = 16
    character(len=16), parameter :: ACE_MCLEF(ACE_NB_MCLEF) = [ &
        'POUTRE          ','COQUE           ','DISCRET         ','ORIENTATION     ',&
        'CABLE           ','BARRE           ','MASSIF          ',&
        'POUTRE_FLUI     ','RIGI_PARASOL    ','GRILLE          ','RIGI_MISS_3D    ',&
        'DISCRET_2D      ','MEMBRANE        ','MASS_AJOU       ','MULTIFIBRE      ',&
        'MASS_REP        ' ]
!
    integer, parameter :: ACE_POUTRE         =  1
    integer, parameter :: ACE_COQUE          =  2
    integer, parameter :: ACE_DISCRET        =  3
    integer, parameter :: ACE_ORIENTATION    =  4
    integer, parameter :: ACE_CABLE          =  5
    integer, parameter :: ACE_BARRE          =  6
    integer, parameter :: ACE_MASSIF         =  7
    integer, parameter :: ACE_POUTRE_FLUI    =  8
    integer, parameter :: ACE_RIGI_PARASOL   =  9
    integer, parameter :: ACE_GRILLE         = 10
    integer, parameter :: ACE_RIGI_MISS_3D   = 11
    integer, parameter :: ACE_DISCRET_2D     = 12
    integer, parameter :: ACE_MEMBRANE       = 13
    integer, parameter :: ACE_MASS_AJOU      = 14
    integer, parameter :: ACE_MULTIFIBRE     = 15
    integer, parameter :: ACE_MASS_REP       = 16
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16), parameter :: ACE_GRMA_MA(4) =[ &
        'GROUP_MA        ','MAILLE          ','GROUP_MA_POI1   ','GROUP_MA_SEG2   ' ]
    character(len=8),  parameter :: ACE_GRMA_TY(4) =[ &
        'GROUP_MA',        'MAILLE  ',        'GROUP_MA',        'GROUP_MA' ]
    integer, parameter :: ACE_GR_MAI  = 1
    integer, parameter :: ACE_MAILLE  = 2
    integer, parameter :: ACE_GR_PO1  = 3
    integer, parameter :: ACE_GR_SE2  = 4
!
    integer, parameter :: ACE_NB_GRMA_MA = 4
    integer, parameter :: MCLEF_GRP_MA(ACE_NB_GRMA_MA*ACE_NB_MCLEF) = [ &
        ACE_GR_MAI, ACE_MAILLE, ACE_NOTHING, ACE_NOTHING , &
        ACE_GR_MAI, ACE_MAILLE, ACE_NOTHING, ACE_NOTHING , &
        ACE_GR_MAI, ACE_MAILLE, ACE_NOTHING, ACE_NOTHING , &
        ACE_GR_MAI, ACE_MAILLE, ACE_NOTHING, ACE_NOTHING , &
        ACE_GR_MAI, ACE_MAILLE, ACE_NOTHING, ACE_NOTHING , &
        ACE_GR_MAI, ACE_MAILLE, ACE_NOTHING, ACE_NOTHING , &
        ACE_GR_MAI, ACE_MAILLE, ACE_NOTHING, ACE_NOTHING , &
        ACE_GR_MAI, ACE_MAILLE, ACE_NOTHING, ACE_NOTHING , &
        ACE_GR_MAI, ACE_GR_PO1, ACE_GR_SE2 , ACE_NOTHING , &
        ACE_GR_MAI, ACE_MAILLE, ACE_NOTHING, ACE_NOTHING , &
        ACE_GR_PO1, ACE_GR_SE2, ACE_NOTHING, ACE_NOTHING , &
        ACE_GR_MAI, ACE_MAILLE, ACE_NOTHING, ACE_NOTHING , &
        ACE_GR_MAI, ACE_MAILLE, ACE_NOTHING, ACE_NOTHING , &
        ACE_GR_MAI, ACE_GR_PO1, ACE_NOTHING, ACE_NOTHING , &
        ACE_GR_MAI, ACE_MAILLE, ACE_NOTHING, ACE_NOTHING , &
        ACE_GR_MAI, ACE_GR_PO1, ACE_NOTHING, ACE_NOTHING ]
!
! --------------------------------------------------------------------------------------------------
! Toutes les cartes créées par AFFE_CARA_ELEM
    integer, parameter :: ACE_NB_CARTE     = 4
    integer, parameter :: ACE_NB_CARTE_CMP = 3
    character(len=10), parameter :: ACE_CARTE(ACE_NB_CARTE*ACE_NB_CARTE_CMP) = [ &
        '.CARDINFO ', 'CINFDI    ', 'DISCRET   ', &
        '.CARDISCK ', 'CADISK    ', 'DISCRET   ', &
        '.CARDISCM ', 'CADISM    ', 'DISCRET   ', &
        '.CARDISCA ', 'CADISA    ', 'DISCRET   ' ]
!
    integer, parameter :: ACE_CAR_DINFO = 1
    integer, parameter :: ACE_CAR_DISCK = 2
    integer, parameter :: ACE_CAR_DISCM = 3
    integer, parameter :: ACE_CAR_DISCA = 4

! #define ACE_NB_CARTE  14
!         '.CARCABLE', 'CACABL   ', 'CABLE     ',
!         '.CARGENBA', 'CAGNBA   ', 'BARRE     ',
!         '.CARMASSI', 'CAMASS   ', 'MASSIF    ',
!         '.CARCOQUE', 'CACOQU   ', 'COQUE     ',
!         '.CARCOQUF', 'CACOQUF  ', 'COQUE    ',
!         '.CARARCPO', 'CAARPO   ', 'POUTRE    ',
!         '.CARGENPO', 'CAGNPO   ', 'POUTRE    ',
!         '.CARGEOPO', 'CAGEPO   ', 'POUTRE    ',
!         '.CARPOUFL', 'CAPOUF   ', 'POUTREFLUI',
!         '.CVENTCXF', 'VENTCX_F ', 'VENT     ',
!         '.CARORIEN', 'CAORIE   ', 'ORIENT   ',
!
! --------------------------------------------------------------------------------------------------
!
!       ACE_NB_ELEMENT              : Nombre de type d'élements différents
!       ACE_NU_(el)                 : Numéro du type de l'élément (POUTRE DISCRET COQUE ...)
!       ACE_NM_(el)                 : Mon des types d'élément (POUTRE DISCRET COQUE ...)
!       ACE_NB_(el)                 : Nombre de support dans le type
!       ACE_EL_(el)[ACE_NB_(el)]    : Liste des supports dans le type
!
! Nombre de type d'éléments
    integer, parameter :: ACE_NB_ELEMENT  =  9
character(len=16),parameter :: ACE_NM_ELEMENT(ACE_NB_ELEMENT) =[ &
        'POUTRE          ','DISCRET         ','COQUE           ','CABLE           ', &
        'BARRE           ','GRILLE          ','MEMBRANE        ','MASSIF          ', &
        'MASSIF THM      ' ]
!
    integer, parameter :: ACE_NU_POUTRE    =  1
    integer, parameter :: ACE_NU_DISCRET   =  2
    integer, parameter :: ACE_NU_COQUE     =  3
    integer, parameter :: ACE_NU_CABLE     =  4
    integer, parameter :: ACE_NU_BARRE     =  5
    integer, parameter :: ACE_NU_GRILLE    =  6
    integer, parameter :: ACE_NU_MEMBRANE  =  7
    integer, parameter :: ACE_NU_MASSIF    =  8
    integer, parameter :: ACE_NU_THHMM     =  9
!
    integer, parameter :: ACE_NB_POUTRE    = 12
    character(len=16), parameter :: ACE_EL_POUTRE(ACE_NB_POUTRE) =[ &
        'MECA_POU_D_T    ','MECA_POU_D_E    ','MECA_POU_D_T_GD ','MEFS_POU_D_T    ',&
        'MECA_POU_D_TG   ','MECA_POHO_HEXA8 ','MECA_POHO_HEXA20','MET3SEG3        ',&
        'MET6SEG3        ','MET3SEG4        ','MECA_POU_D_EM   ','MECA_POU_D_TGM  ' ]
!
!     integer, parameter :: ACE_MECA_POU_D_T         1
!     integer, parameter :: ACE_MECA_POU_D_E         2
!     integer, parameter :: ACE_MECA_POU_D_T_GD      3
!     integer, parameter :: ACE_MECA_POU_C_T         4
!     integer, parameter :: ACE_MEFS_POU_D_T         5
!     integer, parameter :: ACE_MECA_POU_D_TG        6
!     integer, parameter :: ACE_MECA_POHO_HEXA8      7
!     integer, parameter :: ACE_MECA_POHO_HEXA20     8
!     integer, parameter :: ACE_MET3SEG3             9
!     integer, parameter :: ACE_MET6SEG3            10
!     integer, parameter :: ACE_MET3SEG4            11
!     integer, parameter :: ACE_MECA_POU_D_EM       12
!     integer, parameter :: ACE_MECA_POU_D_TGM      13
!
! --------------------------------------------------------------------------------------------------
    integer, parameter :: ACE_NB_DISCRET   = 8
    character(len=16), parameter :: ACE_EL_DISCRET(ACE_NB_DISCRET) =[ &
        'MECA_DIS_T_N    ','MECA_DIS_T_L    ','MECA_DIS_TR_N   ','MECA_DIS_TR_L   ',&
        'MECA_2D_DIS_T_N ','MECA_2D_DIS_T_L ','MECA_2D_DIS_TR_N','MECA_2D_DIS_TR_L' ]
!
!     integer, parameter :: ACE_MECA_DIS_T_N        1
!     integer, parameter :: ACE_MECA_DIS_T_L        2
!     integer, parameter :: ACE_MECA_DIS_TR_N       3
!     integer, parameter :: ACE_MECA_DIS_TR_L       4
!     integer, parameter :: ACE_MECA_2D_DIS_T_N     5
!     integer, parameter :: ACE_MECA_2D_DIS_T_L     6
!     integer, parameter :: ACE_MECA_2D_DIS_TR_N    7
!     integer, parameter :: ACE_MECA_2D_DIS_TR_L    8
!
! --------------------------------------------------------------------------------------------------
    integer, parameter :: ACE_NB_COQUE     = 27
    character(len=16), parameter :: ACE_EL_COQUE(ACE_NB_COQUE) =[ &
        'THCOTR3         ','THCOTR6         ','THCOQU4         ','THCOQU8         ',&
        'THCOTR7         ','THCOQU9         ','MEDKTR3         ','MEDSTR3         ',&
        'MET3TR3         ','MEDKQU4         ','MEDSQU4         ','MEQ4QU4         ',&
        'MECXSE3         ','MEDKTG3         ','MEDKQG4         ','MEQ4GG4         ',&
        'MET3GG3         ','THCASE3         ','THCPSE3         ','MEC3QU9H        ',&
        'MEC3TR7H        ','MEBODKT         ','MEBODST         ','MEBOQ4G         ',&
        'MEBOCQ3         ','THCOSE3         ','THCOSE2         ' ]
!
! --------------------------------------------------------------------------------------------------
    integer, parameter :: ACE_NB_CABLE     = 2
    character(len=16),parameter :: ACE_EL_CABLE(ACE_NB_CABLE) =[ &
        'MECABL2         ','MEPOULI         ' ]
!
! --------------------------------------------------------------------------------------------------
    integer, parameter :: ACE_NB_BARRE     = 3
    character(len=16),parameter :: ACE_EL_BARRE(ACE_NB_BARRE) =[ &
        'MECA_BARRE      ','MECA_2D_BARRE   ','MECGSEG3        ' ]
!
! --------------------------------------------------------------------------------------------------
    integer, parameter :: ACE_NB_GRILLE    = 6
    character(len=16),parameter :: ACE_EL_GRILLE(ACE_NB_GRILLE) =[ &
        'MEGCQU4         ','MEGMTR3         ','MEGMQU4         ','MEGMTR6         ',&
        'MEGMQU8         ','MEGCTR3         ' ]
!
! --------------------------------------------------------------------------------------------------
    integer, parameter :: ACE_NB_MEMBRANE = 6
    character(len=16),parameter :: ACE_EL_MEMBRANE(ACE_NB_MEMBRANE) =[ &
        'MEMBTR3         ','MEMBTR6         ','MEMBQU4         ','MEMBQU8         ',&
        'MEMBTR7         ','MEMBQU9         ']
!
! --------------------------------------------------------------------------------------------------
    integer, parameter :: ACE_NB_MASSIF    = 50
    character(len=16),parameter :: ACE_EL_MASSIF(ACE_NB_MASSIF) =[ &
        'MECA_HEXA8      ','MECA_PENTA6     ','MECA_PENTA18    ','MECA_TETRA4     ',&
        'MECA_HEXA27     ','MECA_HEXA20     ','MECA_PENTA15    ','MECA_TETRA10    ',&
        'MECA_TETRS10    ','MECA_PYRAM5     ','MECA_PYRAM13    ','MECA_HEXS8      ',&
        'MECA_HEXS20     ','MEAXTR3         ','MEAXQU4         ','MEAXTR6         ',&
        'MEAXQU8         ','MEAXQU9         ','MEDPTR3         ','MEDPQU4         ',&
        'MEDPTR6         ','MEDPQU8         ','MEDPQU9         ','MECPTR3         ',&
        'MECPQU4         ','MECPTR6         ','MECPQU8         ','MECPQU9         ',&
        'THER_HEXA8      ','THER_PENTA6     ','THER_TETRA4     ','THER_PYRAM5     ',&
        'THER_HEXA27     ','THER_HEXA20     ','THER_PENTA15    ','THER_TETRA10    ',&
        'THER_PYRAM13    ','THAXTR3         ','THAXQU4         ','THAXTR6         ',&
        'THAXQU8         ','THAXQU9         ','THPLTR3         ','THPLQU4         ',&
        'THPLTR6         ','THPLQU8         ','THPLQU9         ','MET3SEG3        ',&
        'MET6SEG3        ','MET3SEG4        ' ]
!
! --------------------------------------------------------------------------------------------------
    integer, parameter :: ACE_NB_THHMM     = 75
    character(len=16),parameter :: ACE_EL_THHMM(ACE_NB_THHMM) =[ &
        'HM_DPQ8S        ','HM_AXIS_QU8S    ','HM_DPTR6S       ','HM_AXIS_TR6S    ',&
        'HM_HEXA20S      ','HM_PENTA15S     ','HM_TETRA10S     ','THM_DPQ8S       ',&
        'THM_AXIS_QU8S   ','THM_DPTR6S      ','THM_AXIS_TR6S   ','THM_HEXA20S     ',&
        'THM_PENTA15S    ','THM_TETRA10S    ','H_DPQ8S         ','H_DPTR6S        ',&
        'H_HEXA20S       ','H_PENTA15S      ','H_TETRA10S      ','THHM_DPQ8S      ',&
        'THHM_AXIS_QU8S  ','THHM_DPTR6S     ','THHM_AXIS_TR6S  ','THHM_HEXA20S    ',&
        'THHM_PENTA15S   ','THHM_TETRA10S   ','HHM_DPQ8S       ','HHM_AXIS_QU8S   ',&
        'HHM_DPTR6S      ','HHM_AXIS_TR6S   ','HHM_HEXA20S     ','HHM_PENTA15S    ',&
        'HHM_TETRA10S    ','THH_DPQ8S       ','THH_AXIS_QU8S   ','THH_DPTR6S      ',&
        'THH_AXIS_TR6S   ','THH_HEXA20S     ','THH_PENTA15S    ','THH_TETRA10S    ',&
        'HH_DPQ8S        ','HH_AXIS_QU8S    ','HH_DPTR6S       ','HH_AXIS_TR6S    ',&
        'HH_HEXA20S      ','HH_PENTA15S     ','HH_TETRA10S     ','THH2M_DPQ8S     ',&
        'THH2M_AXIS_QU8S ','THH2M_DPTR6S    ','THH2M_AXIS_TR6S ','THH2M_HEXA20S   ',&
        'THH2M_PENTA15S  ','THH2M_TETRA10S  ','HH2M_DPQ8S      ','HH2M_AXIS_QU8S  ',&
        'HH2M_DPTR6S     ','HH2M_AXIS_TR6S  ','HH2M_HEXA20S    ','HH2M_PENTA15S   ',&
        'HH2M_TETRA10S   ','THH2_DPQ8S      ','THH2_AXIS_QU8S  ','THH2_DPTR6S     ',&
        'THH2_AXIS_TR6S  ','THH2_HEXA20S    ','THH2_PENTA15S   ','THH2_TETRA10S   ',&
        'HH2_DPQ8S       ','HH2_AXIS_QU8S   ','HH2_DPTR6S      ','HH2_AXIS_TR6S   ',&
        'HH2_HEXA20S     ','HH2_PENTA15S    ','HH2_TETRA10S    ' ]
!
! --------------------------------------------------------------------------------------------------
    integer, parameter :: ACE_NB_TYPE_ELEM = ACE_NB_POUTRE + ACE_NB_DISCRET  + ACE_NB_COQUE  + &
                                             ACE_NB_CABLE  + ACE_NB_BARRE    + ACE_NB_MASSIF + &
                                             ACE_NB_GRILLE + ACE_NB_MEMBRANE + ACE_NB_THHMM

end module
