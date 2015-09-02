subroutine get_meta_type(meta_type, nb_phasis)
!
use calcul_module, only : ca_iactif_
!
implicit none
!
#include "asterfort/rcvarc.h"
!
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
!
    integer, intent(out) :: meta_type
    integer, intent(out) :: nb_phasis
!
! --------------------------------------------------------------------------------------------------
!
! Comportment utility - Metallurgy
!
! Get metallurgy type
!
! --------------------------------------------------------------------------------------------------
!
! Out meta_type    : type of metallurgy
!                       0 - No metallurgy
!                       1 - Steel
!                       2 - Zirconium
! Out nb_phasis    : total number of phasis (cold and hot)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: steel, zirc, fami
    integer :: kpg, ksp
    integer :: iret_steel, iret_zirc
    real(kind=8) :: r8dummy
!
    data steel /'PFERRITE'/
    data zirc  /'ALPHPUR'/
!
! --------------------------------------------------------------------------------------------------
!
    kpg       = 1
    ksp       = 1
    meta_type = 0
    nb_phasis = 0
!
! - Choice of integration scheme: for CALC_POINT_MAT is PMAT !
!
    if (ca_iactif_ .eq. 2) then
        fami = 'PMAT'
    else
        fami = 'RIGI'
    endif
!
    call rcvarc(' ', steel, '+', fami, kpg,&
                ksp, r8dummy, iret_steel)
    if (iret_steel .eq. 0) then
        meta_type = 1
        nb_phasis = 5
    else
        call rcvarc(' ', zirc, '+', fami, kpg,&
                    ksp, r8dummy, iret_zirc)
        if (iret_zirc .eq. 0) then
            meta_type = 2
            nb_phasis = 3
        endif
    endif

end subroutine
