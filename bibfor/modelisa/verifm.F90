subroutine verifm(fami , npg  , nspg , poum, j_mater,&
                  epsth, iret_)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/get_elasth_para.h"
#include "asterfort/get_elas_id.h"
#include "asterfort/moytem.h"
#include "asterfort/pmfmats.h"
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
    integer, intent(in) :: npg
    integer, intent(in) :: nspg
    character(len=*), intent(in) :: poum
    integer, intent(in) :: j_mater
    real(kind=8), intent(out) :: epsth
    integer, optional, intent(out) :: iret_
!
! --------------------------------------------------------------------------------------------------
!
! Compute thermic dilatation for beams
!
! --------------------------------------------------------------------------------------------------
!
! In  fami         : Gauss family for integration point rule
! In  j_mater      : coded material address
! In  poum         : parameters evaluation
!                     '-' for previous temperature
!                     '+' for current temperature
!                     'T' for current and previous temperature => epsth is increment
! In  npg          : current point gauss
! In  nspg         : number of "sous-point" gauss
! Out epsth        : thermic dilatation
! Out iret         : 0 if temperature defined
!                    1 if not
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: elem_name
    real(kind=8) :: alpha_prev(2), alpha_curr(2)
    integer :: iret_temp_curr, iret_temp_prev, iret_temp_refe, iret_temp
    real(kind=8) :: temp_prev, temp_refe, temp_curr
    integer :: iadzi, iazk24
    integer :: elas_id
    character(len=16) :: elas_keyword
    character(len=8) :: materi
!
! --------------------------------------------------------------------------------------------------
!
    iret_temp_curr  = 0
    iret_temp_prev  = 0
    iret_temp_refe  = 0
    epsth           = 0.d0
!
! - Get name of material if multi-material Gauss point (PMF)
!
    call pmfmats(j_mater, materi)
!
! - No temperature -> thermic strain is zero
!
    call rcvarc(' ' , 'TEMP'   , '+'      , fami, npg,&
                nspg, temp_curr, iret_temp)
    if (iret_temp .ne. 0) then
        goto 999
    endif  
!
! - Get reference temperature
!
    call rcvarc(' ' , 'TEMP', 'REF', fami, npg,&
                nspg, temp_refe, iret_temp_refe)
    if (iret_temp_refe .eq. 1) then
        call tecael(iadzi, iazk24)
        elem_name = zk24(iazk24-1+3) (1:8)
        call utmess('F', 'COMPOR5_8', sk=elem_name)
    endif
!
! - Get type of elasticity (Isotropic/Orthotropic/Transverse isotropic)
!
    call get_elas_id(j_mater, elas_id, elas_keyword)
!
! - Only isotropic elasticity
!
    if (elas_id.ne.1) then
        call tecael(iadzi, iazk24)
        elem_name = zk24(iazk24-1+3)(1:8)
        call utmess('F', 'COMPOR5_9', sk=elem_name)
    endif
!
! - No metallurgical material
!
    if (elas_keyword.eq.'ELAS_META') then
        call tecael(iadzi, iazk24)
        elem_name = zk24(iazk24-1+3) (1:8)
        call utmess('F', 'COMPOR5_10', sk=elem_name)
    endif
!
! - Get temperatures (mean value on all Gauss points)
!
    if (poum.eq.'T'.or.poum.eq.'-') then
        call moytem(fami, npg, nspg, '-', temp_prev,&
                    iret_temp_prev)
    endif
    if (poum.eq.'T'.or.poum.eq.'+') then
        call moytem(fami, npg, nspg, '+', temp_curr,&
                    iret_temp_curr)
    endif
!
! - Get elastic parameters for thermic dilatation
!
    if (poum.eq.'T'.or.poum.eq.'-') then
        if (iret_temp_prev.eq.0) then
            call get_elasth_para(fami     , j_mater     , '-', npg, nspg,&
                                 elas_id  , elas_keyword, materi_ = materi, temp_vale_ = temp_prev,&
                                 alpha = alpha_prev)
        endif
    endif
    if (poum.eq.'T'.or.poum.eq.'+') then
        if (iret_temp_curr.eq.0) then
            call get_elasth_para(fami     , j_mater     , '+', npg, nspg,&
                                 elas_id  , elas_keyword, materi_ = materi, temp_vale_ = temp_curr,&
                                 alpha = alpha_curr)
        endif
    endif
!
! - Compute thermic strain
!
    if (poum .eq. 'T') then
        if (iret_temp_prev+iret_temp_curr.eq.0) then
            if (elas_id.eq.1) then
                epsth = alpha_curr(1)*(temp_curr-temp_refe)-&
                        alpha_prev(1)*(temp_prev-temp_refe)
            else
                ASSERT(.false.)
            endif
        endif
    else if (poum .eq. '-') then
        if (iret_temp_prev.eq.0) then
            if (elas_id.eq.1) then
                epsth = alpha_prev(1)*(temp_prev-temp_refe)
            else
                ASSERT(.false.)
            endif
        endif
    else if (poum .eq. '+') then
        if (iret_temp_curr.eq.0) then
            if (elas_id.eq.1) then
                epsth = alpha_curr(1)*(temp_curr-temp_refe)
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
! - Output error
!
    if (present(iret_)) then
        iret_ = 0
        if ((iret_temp_prev+iret_temp_curr) .ne. 0) then
            iret_ = 1
        endif
        if (iret_temp .ne. 0) then
            iret_ = 1
        endif
    endif
!
end subroutine
