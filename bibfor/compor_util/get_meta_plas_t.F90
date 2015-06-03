subroutine get_meta_plas_t(poum     , fami     , kpg      , ksp      , j_mater   ,&
                           meta_type, nb_phasis, phas_prev, phas_curr, zcold_curr,&
                           kpt      , fpt)
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
    real(kind=8), intent(in) :: phas_prev(*)
    real(kind=8), intent(in) :: phas_curr(*)
    real(kind=8), intent(in) :: zcold_curr
    real(kind=8), intent(out) :: kpt(*)
    real(kind=8), intent(out) :: fpt(*)
!
! --------------------------------------------------------------------------------------------------
!
! Comportment utility - Metallurgy
!
! Get parameters for transformation plasticity
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
! In  phas_prev    : previous phasis
! In  phas_curr    : current phasis
! In  zcold_curr   : sum of cold phasis
! Out kpt          : transformation plasticity - constant k
! Out fpt          : transformation plasticity - function f'
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_res_mx
    parameter (nb_res_mx = 4)
    real(kind=8) :: valres(nb_res_mx)
    integer :: codret(nb_res_mx)
    character(len=16) :: nomres(nb_res_mx)
    integer :: nb_res, i_phasis, nb_phasis_cold
    real(kind=8) :: deltaz
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
    nb_phasis_cold = nb_phasis - 1
!
! - Name of parameters - Constant k
!
    if (meta_type.eq.1) then
        nb_res = 4
        nomres(1) = 'F1_K'
        nomres(2) = 'F2_K'
        nomres(3) = 'F3_K'
        nomres(4) = 'F4_K'   
    elseif (meta_type.eq.2) then
        nb_res = 2
        nomres(1) = 'F1_K'
        nomres(2) = 'F2_K'
    else
        ASSERT(.false.)
    endif
!
! - Get parameters - Constant k
!
    call rcvalb(fami, kpg, ksp, poum, j_mater,&
                ' ', 'META_PT', 0, ' ', [0.d0],&
                nb_res, nomres, valres, codret, 2)
    do i_phasis = 1, nb_phasis_cold
        kpt(i_phasis) = valres(i_phasis)
    end do
!
! - Name of parameters - function f'
!
    if (meta_type.eq.1) then
        nb_res    = 4
        nomres(1) = 'F1_D_F_META'
        nomres(2) = 'F2_D_F_META'
        nomres(3) = 'F3_D_F_META'
        nomres(4) = 'F4_D_F_META'
    elseif (meta_type.eq.2) then
        nb_res    = 2
        nomres(1) = 'F1_D_F_META'
        nomres(2) = 'F2_D_F_META'
    else
        ASSERT(.false.)
    endif
!
! - Get parameters - function f'
!
    do i_phasis = 1, nb_phasis_cold
        deltaz = (phas_curr(i_phasis) - phas_prev(i_phasis))
        if (deltaz .gt. 0.d0) then
            call rcvalb(fami, kpg, ksp, poum, j_mater,&
                        ' ', 'META_PT', 1, 'META', [zcold_curr],&
                        1, nomres(i_phasis), valres(i_phasis), codret(i_phasis), 2)
            fpt(i_phasis) = valres(i_phasis)
        else
            fpt(i_phasis) = 0.d0
        endif
    end do
!
end subroutine
