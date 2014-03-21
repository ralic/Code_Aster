subroutine rslnvi(mod, ndt, ndi, nr, nvi)
    implicit none
#include "asterfort/utmess.h"
!       ================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       ROUSSELIER : NOMBRE DE COMPOSANTES DES CONTRAINTES ET
!                    NOMBRE VARIABLES
!       ----------------------------------------------------------------
!       IN  MOD    :  TYPE DE MODELISATION
!       OUT NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
!           NDI    :  NB DE COMPOSANTES DIRECTES TENSEURS
!           NR     :  NB DE COMPOSANTES SYSTEME NL
!           NVI    :  NB DE VARIABLES INTERNES
!       ----------------------------------------------------------------
    integer :: ndt, ndi, nr, nvi
    character(len=8) :: mod
!       ----------------------------------------------------------------
!
! -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
!
    if (mod(1:2) .eq. '3D') then
        ndt = 6
        ndi = 3
        nr = ndt+2
        else if ( mod(1:6) .eq. 'D_PLAN' .or. mod(1:4) .eq. 'AXIS'&
    ) then
        ndt = 4
        ndi = 3
        nr = ndt+2
    else if (mod(1:6) .eq. 'C_PLAN') then
        ndt = 4
        ndi = 3
        nr = ndt+3
        call utmess('F', 'ALGORITH10_51')
    else if (mod(1:2) .eq. '1D') then
        ndt = 3
        ndi = 3
        nr = ndt+2
    endif
    nvi = 5
!
end subroutine
