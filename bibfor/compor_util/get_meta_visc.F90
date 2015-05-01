subroutine get_meta_visc(poum     , fami     , kpg, ksp, j_mater,&
                         meta_type, nb_phasis, eta, n  , unsurn ,&
                         c        , m)
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
    real(kind=8), optional, intent(out) :: eta(*)
    real(kind=8), optional, intent(out) :: n(*)
    real(kind=8), optional, intent(out) :: unsurn(*)
    real(kind=8), optional, intent(out) :: c(*)
    real(kind=8), optional, intent(out) :: m(*)
!
! --------------------------------------------------------------------------------------------------
!
! Comportment utility - Metallurgy
!
! Get parameters for viscosity
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
! In  nb_phasis    : number of phasis
! Out eta          : viscosity parameter - eta
! Out n            : viscosity parameter - n
! Out unsurn       : viscosity parameter - 1/n
! Out c            : viscosity parameter - C
! Out m            : viscosity parameter - m
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_res_mx
    parameter (nb_res_mx = 20)
    real(kind=8) :: valres(nb_res_mx)
    integer :: codret(nb_res_mx)
    character(len=8) :: nomres(nb_res_mx)
    integer :: nb_res, i_res
!
! --------------------------------------------------------------------------------------------------
!
!
! - Name of parameters
!
    if (meta_type.eq.1) then
        ASSERT(nb_phasis.eq.4)
        if (present(eta)) then
            nomres(1)     = 'F1_ETA'
            nomres(2)     = 'F2_ETA'
            nomres(3)     = 'F3_ETA'
            nomres(4)     = 'F4_ETA'
            nomres(5)     = 'C_ETA'
            nb_res        = 5
            eta(1:nb_res) = 0.d0
        endif
    elseif (meta_type.eq.2) then
        ASSERT(nb_phasis.eq.2)
        if (present(eta)) then
            nomres(1)     = 'F1_ETA'
            nomres(2)     = 'F2_ETA'
            nomres(3)     = 'C_ETA'
            nb_res        = 3
            eta(1:nb_res) = 0.d0
        endif
    else
        ASSERT(.false.)
    endif
!
! - Get parameters
!
    if (present(eta)) then
        call rcvalb(fami, kpg, ksp, poum, j_mater,&
                    ' ', 'META_VISC', 0, ' ', [0.d0],&
                    nb_res, nomres, valres, codret, 2)
        do i_res = 1, nb_res
            eta(i_res)    = valres(i_res)
        end do
    endif
!
! - Name of parameters
!
    if (meta_type.eq.1) then
        ASSERT(nb_phasis.eq.4)
        if (present(n)) then
            nomres(1)        = 'F1_N'
            nomres(2)        = 'F2_N'
            nomres(3)        = 'F3_N'
            nomres(4)        = 'F4_N'
            nomres(5)        = 'C_N'
            nb_res           = 5
            n(1:nb_res)      = 20.d0
            unsurn(1:nb_res) = 1.d0
        endif
    elseif (meta_type.eq.2) then
        ASSERT(nb_phasis.eq.2)
        if (present(n)) then
            nomres(1)        = 'F1_N'
            nomres(2)        = 'F2_N'
            nomres(3)        = 'C_N'
            nb_res           = 3
            n(1:nb_res)      = 20.d0
            unsurn(1:nb_res) = 1.d0
        endif
    endif
!
! - Get parameters
!
    call rcvalb(fami, kpg, ksp, poum, j_mater,&
                ' ', 'META_VISC', 0, ' ', [0.d0],&
                nb_res, nomres, valres, codret, 2)
    if (present(n)) then
        call rcvalb(fami, kpg, ksp, poum, j_mater,&
                    ' ', 'META_VISC', 0, ' ', [0.d0],&
                    nb_res, nomres, valres, codret, 2)
        do i_res = 1, nb_res
            n(i_res)      = valres(i_res)
            unsurn(i_res) = 1.d0/n(i_res)
        end do
    endif
!
! - Name of parameters
!
    if (meta_type.eq.1) then
        ASSERT(nb_phasis.eq.4)
        if (present(c)) then
            nomres(1)   = 'F1_C'
            nomres(2)   = 'F2_C'
            nomres(3)   = 'F3_C'
            nomres(4)   = 'F4_C'
            nomres(5)   = 'C_C'
            nb_res      = 5
            c(1:nb_res) = 0.d0
        endif
    elseif (meta_type.eq.2) then
        ASSERT(nb_phasis.eq.2)
        if (present(c)) then
            nomres(1)   = 'F1_C'
            nomres(2)   = 'F2_C'
            nomres(3)   = 'C_C'
            nb_res      = 3
            c(1:nb_res) = 0.d0
        endif
    endif
!
! - Get parameters
!
    if (present(c)) then
        call rcvalb(fami, kpg, ksp, poum, j_mater,&
                    ' ', 'META_VISC', 0, ' ', [0.d0],&
                    nb_res, nomres, valres, codret, 2)
        do i_res = 1, nb_res
            c(i_res) = valres(i_res)
        end do
    endif
!
! - Name of parameters
!
    if (meta_type.eq.1) then
        ASSERT(nb_phasis.eq.4)
        if (present(m)) then
            nomres(1)   = 'F1_M'
            nomres(2)   = 'F2_M'
            nomres(3)   = 'F3_M'
            nomres(4)   = 'F4_M'
            nomres(5)   = 'C_M'
            nb_res      = 5
            m(1:nb_res) = 20.d0
        endif
    elseif (meta_type.eq.2) then
        ASSERT(nb_phasis.eq.2)
        if (present(m)) then
            nomres(1)   = 'F1_M'
            nomres(2)   = 'F2_M'
            nomres(3)   = 'C_M'
            nb_res      = 3
            m(1:nb_res) = 20.d0
        endif
    endif
!
! - Get parameters
!
    if (present(m)) then
        call rcvalb(fami, kpg, ksp, poum, j_mater,&
                    ' ', 'META_VISC', 0, ' ', [0.d0],&
                    nb_res, nomres, valres, codret, 2)
        do i_res = 1, nb_res
            m(i_res) = valres(i_res)
        end do
    endif
!
end subroutine
