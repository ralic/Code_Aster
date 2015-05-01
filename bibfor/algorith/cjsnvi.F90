subroutine cjsnvi(mod, ndt, ndi, nvi)
    implicit none
#include "asterfort/utmess.h"
!
!       ================================================================
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!       CJS        : DONNE NOMBRE CONTARINTES ET NOMBRE VARIABLES DE
!                    CJS
!       ----------------------------------------------------------------
!       IN
!           MOD    :  TYPE DE MODELISATION
!       OUT
!           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
!           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
!           NVI    :  NB DE VARIABLES INTERNES
!       ----------------------------------------------------------------
    integer :: ndt, ndi, nvi
!
! VARIABLES LOALES POUR SE PREMUNIR D APPELS DU TYPE
! CALL CJSNVI(MOD,IBID,IBID,NVI)
!
    integer :: ndtloc, ndiloc
    character(len=8) :: mod
!       ----------------------------------------------------------------
!
! -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
!
!
! - 3D
    if (mod(1:2) .eq. '3D') then
        ndtloc = 6
        ndiloc = 3
!
! - D_PLAN AXIS
    else if (mod(1:6).eq.'D_PLAN'.or.mod(1:4).eq.'AXIS') then
        ndtloc = 4
        ndiloc = 3
!
! - C_PLAN
    else if (mod(1:6).eq.'C_PLAN') then
        call utmess('F', 'ALGORITH2_15')
!
! - 1D
    else if (mod(1:2).eq.'1D') then
        call utmess('F', 'ALGORITH2_15')
! - 1D
    else
        call utmess('F', 'ALGORITH2_20')
    endif
    nvi = ndtloc+10
    ndt = ndtloc
    ndi = ndiloc
end subroutine
