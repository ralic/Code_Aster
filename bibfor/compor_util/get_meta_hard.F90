subroutine get_meta_hard(poum       , fami     , kpg      , ksp   , j_mater,&
                         l_hard_line, meta_type, nb_phasis, l_temp, temp   ,&
                         young      , epsp     , r0)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rctype.h"
#include "asterfort/rctrac.h"
#include "asterfort/rcfonc.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    logical, intent(in) :: l_hard_line
    logical, intent(in) :: l_temp
    real(kind=8), intent(in) :: young
    real(kind=8), intent(in) :: temp
    real(kind=8), intent(in) :: epsp(*)
    real(kind=8), intent(out) :: r0(*)
!
! --------------------------------------------------------------------------------------------------
!
! Comportment utility - Metallurgy
!
! Get hardening
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
! In  l_hard_line  : .true. if linear hardening
! In  young        : Young modulus
! In  l_temp       : .true. if temperature command variable is affected
! In  temp         : temperature
! In  epsp         : cumulated plastic strain
! Out r0           : current hardening
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_res_mx
    parameter (nb_res_mx = 5)
    real(kind=8) :: valres(nb_res_mx)
    integer :: codret(nb_res_mx)
    character(len=16) :: nomres(nb_res_mx)
!
    integer :: nb_res, i_res
    integer :: i_phasis
    character(len=8) :: keyw_trac(5)
    character(len=16) :: keyw_fact
    integer :: j_prol, j_vale
    integer :: nb_vale
    character(len=8) :: para_type
    real(kind=8) :: para_vale, r8dummy, h(5)
!
! --------------------------------------------------------------------------------------------------
!
    if (l_hard_line) then
        if (meta_type.eq.1) then
            ASSERT(nb_phasis.eq.4)
            nb_res    = 5
            nomres(1) = 'F1_D_SIGM_EPSI'
            nomres(2) = 'F2_D_SIGM_EPSI'
            nomres(3) = 'F3_D_SIGM_EPSI'
            nomres(4) = 'F4_D_SIGM_EPSI'
            nomres(5) = 'C_D_SIGM_EPSI'
        elseif (meta_type.eq.2) then
            ASSERT(nb_phasis.eq.2)
            nb_res    = 3
            nomres(1) = 'F1_D_SIGM_EPSI'
            nomres(2) = 'F2_D_SIGM_EPSI'
            nomres(3) = 'C_D_SIGM_EPSI'
        else
            ASSERT(.false.)
        endif
        call rcvalb(fami, kpg, ksp, poum, j_mater,&
                    ' ', 'META_ECRO_LINE', 0, ' ', [0.d0],&
                    nb_res, nomres, valres, codret, 2)
        do i_res = 1, nb_res
            h(i_res) = valres(i_res)
            r0(i_res) = h(i_res)*young/(young-h(i_res))
        end do
    else
        nb_vale   = 1
        keyw_fact = 'META_TRACTION'
        if (meta_type.eq.1) then
            ASSERT(nb_phasis.eq.4)
            keyw_trac(1) = 'SIGM_F1'
            keyw_trac(2) = 'SIGM_F2'
            keyw_trac(3) = 'SIGM_F3'
            keyw_trac(4) = 'SIGM_F4'
            keyw_trac(5) = 'SIGM_C'
        elseif (meta_type.eq.2) then
            ASSERT(nb_phasis.eq.2)
            keyw_trac(1) = 'SIGM_F1'
            keyw_trac(2) = 'SIGM_F2'
            keyw_trac(3) = 'SIGM_C'
        else
            ASSERT(.false.)
        endif

        do i_phasis = 1, nb_phasis+1
            call rctype(j_mater, nb_vale, 'TEMP', [temp], para_vale,&
                        para_type, keyw_factz = keyw_fact, keywz = keyw_trac(i_phasis))
            if ((para_type.eq.'TEMP') .and. (.not.l_temp)) then
                call utmess('F', 'COMPOR5_5', sk = para_type)
            endif
            call rctrac(j_mater, 2, keyw_trac(i_phasis), para_vale, j_prol,&
                        j_vale, nb_vale, r8dummy)
            call rcfonc('V', 2, j_prol, j_vale, nb_vale,&
                        p = epsp(i_phasis), rprim = r0(i_phasis))
        end do
    endif
!
end subroutine
