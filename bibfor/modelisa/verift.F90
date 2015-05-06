subroutine verift(fami      , kpg       , ksp       , poum  , j_mater,&
                  materi_   , iret      , epsth     , vepsth,&
                  temp_prev_, temp_curr_, temp_refe_)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/get_elasth_para.h"
#include "asterfort/get_elas_type.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: fami
    integer, intent(in) :: kpg
    integer, intent(in) :: ksp
    character(len=*), intent(in) :: poum
    integer, intent(in) :: j_mater
    character(len=8), optional, intent(in) :: materi_
    integer, optional, intent(out) :: iret
    real(kind=8), optional, intent(out) :: epsth
    real(kind=8), optional, intent(out) :: vepsth(3)
    real(kind=8), optional, intent(out) :: temp_prev_
    real(kind=8), optional, intent(out) :: temp_curr_
    real(kind=8), optional, intent(out) :: temp_refe_
!
! --------------------------------------------------------------------------------------------------
!
! Compute thermic strain
!
! --------------------------------------------------------------------------------------------------
!
! In  fami         : Gauss family for integration point rule
! In  j_mater      : coded material address
! In  poum         : parameters evaluation 
!                     '-' for previous temperature
!                     '+' for current temperature
!                     'T' for current and previous temperature
! In  kpg          : current point gauss
! In  ksp          : current "sous-point" gauss
! In  materi       : name of material if multi-material Gauss point (PMF)
! In  iret         : 0 if temperature defined
!                    1 if not
! Out epsth        : thermic dilatation increment if poum=T
!                    thermic dilatation if poum = + ou -
! Out vepsth       : non-isotropic or multiphasic thermic dilatation
! Out temp_prev    : previous temperature
! Out temp_curr    : current temperature
! Out temp_refe    : reference temperature
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: elem_name, materi
    integer :: iret_temp_prev, iret_temp_curr, iret_temp, iret_temp_refe
    real(kind=8) :: temp_prev, temp_refe, temp_curr, epsth3(3)
    real(kind=8) :: alpha_p(2)
    real(kind=8) :: alpha_l_p, alpha_t_p, alpha_n_p
    real(kind=8) :: alpha_c(2)
    real(kind=8) :: alpha_l_c, alpha_t_c, alpha_n_c
    integer :: iadzi, iazk24
    integer :: elas_type
    character(len=16) :: elas_keyword
!
! --------------------------------------------------------------------------------------------------
!
    materi = ' '
    if (present(materi_)) then
        materi = materi_
    endif
!
    iret_temp      = 0
    iret_temp_prev = 0
    iret_temp_curr = 0
    iret_temp_refe = 0
    epsth3(1:3)    = 0.d0
!
! - No temperature -> thermic strain is zero
!
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, temp_curr, iret_temp)
    if (iret_temp .ne. 0) then
        goto 999
    endif
!
! - Get reference temperature
!
    call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                ksp, temp_refe, iret_temp_refe)
    if (iret_temp_refe .eq. 1) then
        call tecael(iadzi, iazk24)
        elem_name = zk24(iazk24-1+3) (1:8)
        call utmess('F', 'COMPOR5_8', sk=elem_name)
    endif
!
! - Get type of elasticity (Isotropic/Orthotropic/Transverse isotropic)
!
    call get_elas_type(j_mater, elas_type, elas_keyword)
!
! - Get temperatures
!
    if (poum.eq.'T'.or.poum.eq.'-') then
        call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                    ksp, temp_prev, iret_temp_prev)
    endif
    if (poum.eq.'T'.or.poum.eq.'+') then
        call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                    ksp, temp_curr, iret_temp_curr)
    endif
!
! - Get elastic parameters for thermic dilatation
!
    if (poum.eq.'T'.or.poum.eq.'-') then
        if (iret_temp_prev.eq.0) then
            call get_elasth_para(fami     , j_mater     , '-', kpg    , ksp,&
                                 elas_type, elas_keyword, materi,&
                                 alpha_p  , alpha_l_p   , alpha_t_p, alpha_n_p)
        endif
    endif
    if (poum.eq.'T'.or.poum.eq.'+') then
        if (iret_temp_curr.eq.0) then
            call get_elasth_para(fami     , j_mater     , '+', kpg    , ksp,&
                                 elas_type, elas_keyword, materi,&
                                 alpha_c  , alpha_l_c   , alpha_t_c, alpha_n_c)
        endif
    endif
!
! - Check non-isotropic material
!
    if (elas_type.ne.1) then
        if (.not.present(vepsth)) then
            call tecael(iadzi, iazk24)
            elem_name = zk24(iazk24-1+3) (1:8)
            call utmess('F', 'COMPOR5_9', sk=elem_name)
        endif
    endif
!
! - Check metallurgical material
!
    if (elas_keyword.eq.'ELAS_META') then
        if (.not.present(vepsth)) then
            call tecael(iadzi, iazk24)
            elem_name = zk24(iazk24-1+3) (1:8)
            call utmess('F', 'COMPOR5_10', sk=elem_name)
        endif
    endif
!
! - Compute thermic strain
!
    if (poum .eq. 'T') then
        if (iret_temp_prev+iret_temp_curr.eq.0) then
            if (elas_type.eq.1) then
                if (elas_keyword.eq.'ELAS_META') then
                    epsth3(1) = alpha_c(1)*(temp_curr-temp_refe)-alpha_p(1)*(temp_prev-temp_refe)
                    epsth3(2) = alpha_c(2)*(temp_curr-temp_refe)-alpha_p(2)*(temp_prev-temp_refe)
                else
                    epsth3(1) = alpha_c(1)*(temp_curr-temp_refe)-alpha_p(1)*(temp_prev-temp_refe)
                endif
            elseif (elas_type.eq.2) then
                epsth3(1) = alpha_l_c*(temp_curr-temp_refe)-alpha_l_p*(temp_prev-temp_refe)
                epsth3(2) = alpha_t_c*(temp_curr-temp_refe)-alpha_t_p*(temp_prev-temp_refe)
                epsth3(3) = alpha_n_c*(temp_curr-temp_refe)-alpha_n_p*(temp_prev-temp_refe)
            elseif (elas_type.eq.3) then
                epsth3(1) = alpha_l_c*(temp_curr-temp_refe)-alpha_l_p*(temp_prev-temp_refe)
                epsth3(2) = alpha_n_c*(temp_curr-temp_refe)-alpha_n_p*(temp_prev-temp_refe)
            else
                ASSERT(.false.)
            endif
        endif
    else if (poum .eq. '-') then
        if (iret_temp_prev.eq.0) then
            if (elas_type.eq.1) then
                if (elas_keyword.eq.'ELAS_META') then
                    epsth3(1) = alpha_p(1)*(temp_prev-temp_refe)
                    epsth3(2) = alpha_p(2)*(temp_prev-temp_refe)
                else
                    epsth3(1) = alpha_p(1)*(temp_prev-temp_refe)
                endif
            elseif (elas_type.eq.2) then
                epsth3(1) = alpha_l_p*(temp_prev-temp_refe)
                epsth3(2) = alpha_t_p*(temp_prev-temp_refe)
                epsth3(3) = alpha_n_p*(temp_prev-temp_refe)            
            elseif (elas_type.eq.3) then
                epsth3(1) = alpha_l_p*(temp_prev-temp_refe)
                epsth3(2) = alpha_n_p*(temp_prev-temp_refe)
            else
                ASSERT(.false.)
            endif
        endif
    else if (poum .eq. '+') then
        if (iret_temp_curr.eq.0) then
            if (elas_type.eq.1) then
                if (elas_keyword.eq.'ELAS_META') then
                    epsth3(1) = alpha_c(1)*(temp_curr-temp_refe)
                    epsth3(2) = alpha_c(2)*(temp_curr-temp_refe)
                else
                    epsth3(1) = alpha_c(1)*(temp_curr-temp_refe)
                endif
            elseif (elas_type.eq.2) then
                epsth3(1) = alpha_l_c*(temp_curr-temp_refe)
                epsth3(2) = alpha_t_c*(temp_curr-temp_refe)
                epsth3(3) = alpha_n_c*(temp_curr-temp_refe)            
            elseif (elas_type.eq.3) then
                epsth3(1) = alpha_l_c*(temp_curr-temp_refe)
                epsth3(2) = alpha_n_c*(temp_curr-temp_refe)
            else
                ASSERT(.false.)
            endif
        endif
    else
        ASSERT(.false.)
    endif
!
999 continue
!
! - Output temperature
!
    if (present(temp_refe_)) then
        temp_refe_ = temp_refe
    endif
    if (present(temp_prev_)) then
        temp_prev_ = temp_prev
    endif
    if (present(temp_curr_)) then
        temp_curr_ = temp_curr
    endif
!
! - Output strains
!
    if (present(vepsth)) then
        vepsth(1:3) = epsth3(1:3)
    endif
    if (present(epsth)) then
        epsth = epsth3(1)
    endif
!
! - Output error
!
    if (present(iret)) then
        iret = 0
        if ((iret_temp_prev+iret_temp_curr) .ne. 0) then 
            iret = 1
        endif
        if (iret_temp .ne. 0) then
            iret = 1
        endif
    endif
!
end subroutine
