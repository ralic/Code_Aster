subroutine typthm(axi, perman, vf, typvf, typmod,&
                  ndim)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
! OUT AXI    : .TRUE. OU .FALSE. SELON LE CARACTERE AXISYMETRIQUE DU PB
!
! OUT VF     : .TRUE. OU .FALSE. SELON LE CARACTERE VF OU PAS
!     VF EST VRAI SI LA FORMULATION CONTINUE EST UN BILAN PAR MAILLE
!               DONC VF EST VRAI POUR SUSHI OU TPFA
!               AVEC OU SANS PRISE EN COMPTE DES VOISINS
! OUT TYPVF TYPE DE VF :1 = TPFA (FLUX A DEUX POINTS - SCHEMA SUPPRIME)
!                    2  = SUSHI AVEC VOISIN DECENTRE MAILLE (SUDM - SUPPRIME)
!                    3  = SUSHI AVEC VOISIN DECENTRE ARETE (SUDA)
!                    4  = SUSHI AVEC VOISIN CENTRE  (SUC - SUPPRIME)
! OUT PERMAN : .TRUE. OU .FALSE. SELON LE CARACTERE AXISYMETRIQUE DE
!              LA PARTIE HYDRAULIQUE
! OUT TYPMOD : 1. TYPE DE MODELISATION : AXI/D_PLAN/3D
! OUT NDIM   : DIMENSION DU PROBLEME (2 OU 3)
! ----------------------------------------------------------------------
    implicit none
!
!     --- ARGUMENTS ---
#include "asterf_types.h"
#include "asterfort/lteatt.h"
#include "asterfort/lxlgut.h"
    aster_logical :: axi, perman, vf
    integer :: typvf
    integer :: ndim
    character(len=8) :: typmod(2)
!
!     --- VARIABLES LOCALES ---
!
!
! =====================================================================
! --- BUT : DETERMINER LE TYPE DE MODELISATION (AXI/D_PLAN/3D) --------
! =====================================================================
    axi = .false.
!
    if (lteatt('AXIS','OUI')) then
        axi = .true.
        typmod(1) = 'AXIS    '
        ndim = 2
!
    else if (lteatt('D_PLAN','OUI')) then
        typmod(1) = 'D_PLAN  '
        ndim = 2
!
    else
        typmod(1) = '3D      '
        ndim = 3
    endif
!
! =====================================================================
! --- BUT : LA PARTIE HM EST-ELLE TRANSITOIRE OU PERMANENTE EN TEMPS ?
! =====================================================================
!
    if (lteatt('CODMOD','DHB')) then
        perman = .true.
    else
        perman = .false.
    endif
!
!   -- MODELISATIONS SUSHI VOLUMES FINIS
    if (lteatt('CODMOD','3AD').or.lteatt('CODMOD','2DA')) then
        vf = .true.
        typvf=3
    else
        typvf =0
        vf = .false.
    endif
!
end subroutine
