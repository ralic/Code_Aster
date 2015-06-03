subroutine get_meta_mixd(poum  , fami     , kpg      , ksp   , j_mater,&
                         l_visc, meta_type, nb_phasis, zalpha,fmel    ,&
                         sy)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/rcvalb.h"
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
    character(len=1), intent(in) :: poum
    character(len=*), intent(in) :: fami
    integer, intent(in) :: kpg
    integer, intent(in) :: ksp
    integer, intent(in) :: j_mater
    integer, intent(in) :: meta_type
    integer, intent(in) :: nb_phasis
    logical, intent(in) :: l_visc
    real(kind=8), intent(in) :: zalpha
    real(kind=8), intent(out) :: fmel
    real(kind=8), optional, intent(out) :: sy(*)
!
! --------------------------------------------------------------------------------------------------
!
! Comportment utility - Metallurgy
!
! Get parameters for mixing law
!
! --------------------------------------------------------------------------------------------------
!
! In  poum         : '-' or '+' for parameters evaluation (previous or current)
! In  fami         : Gauss family for integration point rule
! In  kpg          : current point gauss
! In  ksp          : current "sous-point" gauss
! In  j_mater      : coded material address
! In  meta_type    : type of metallurgy
!                       0 - No metallurgy
!                       1 - Steel
!                       2 - Zirconium
! In  nb_phasis    : total number of phasis (cold and hot)
! In  l_visc       : .true. if visco-plasticity
! In  zalpha       : sum of "cold" phasis
! Out fmel         : mixing function
! Out sy           : elasticity yield by phasis
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_res_mx
    parameter (nb_res_mx = 6)
    real(kind=8) :: valres(nb_res_mx)
    integer :: codret(nb_res_mx)
    character(len=16) :: nomres(nb_res_mx)
    integer :: nb_res, i_res
!
! --------------------------------------------------------------------------------------------------
!
    if (meta_type.eq.1) then
        ASSERT(nb_phasis.eq.5) 
    elseif (meta_type.eq.2) then
        ASSERT(nb_phasis.eq.3)
    else
        ASSERT(.false.)
    endif
!
! - Mixing function
!
    nb_res    = 1
    nomres(1) = 'SY_MELANGE'
    if (l_visc) then
        nb_res    = 1
        nomres(1) = 'S_VP_MELANGE'
    endif
    fmel = 0.d0
    call rcvalb(fami, kpg, ksp, poum, j_mater,&
                ' ', 'ELAS_META', 1, 'META', [zalpha],&
                nb_res, nomres, valres, codret, 0)
    if (codret(1) .eq. 0) then
        fmel = valres(1)
    else
        fmel = zalpha
    endif
!
! - Elasticity yield by phasis
!
    if (present(sy)) then
        nb_res = nb_phasis
        if (meta_type.eq.1) then
            nomres(1) = 'F1_SY'
            nomres(2) = 'F2_SY'
            nomres(3) = 'F3_SY'
            nomres(4) = 'F4_SY'
            nomres(5) = 'C_SY'
            if (l_visc) then
                nomres(1) = 'F1_S_VP'
                nomres(2) = 'F2_S_VP'
                nomres(3) = 'F3_S_VP'
                nomres(4) = 'F4_S_VP'
                nomres(5) = 'C_S_VP'
            endif
            sy(1:nb_res) = 0.d0
        elseif (meta_type.eq.2) then
            nomres(1) = 'F1_SY'
            nomres(2) = 'F2_SY'
            nomres(3) = 'C_SY'
            if (l_visc) then
                nomres(1) = 'F1_S_VP'
                nomres(2) = 'F2_S_VP'
                nomres(3) = 'C_S_VP'
            endif
            sy(1:nb_res) = 0.d0
        else
            ASSERT(.false.)
        endif
        call rcvalb(fami, kpg, ksp, poum, j_mater,&
                    ' ', 'ELAS_META', 0, ' ', [0.d0],&
                    nb_res, nomres, valres, codret, 2)
        do i_res = 1, nb_res
            sy(i_res) = valres(i_res)
        end do
    endif
!
end subroutine
