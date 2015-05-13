subroutine get_meta_phasis(fami     , poum   , ipg   , ispg , meta_type  ,&
                           nb_phasis, phasis_, zcold_, zhot_, tole_bound_)
!
implicit none
!
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
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
    character(len=*), intent(in) :: fami
    character(len=1), intent(in) :: poum
    integer, intent(in) :: ipg
    integer, intent(in) :: ispg
    integer, intent(in) :: meta_type
    integer, intent(in) :: nb_phasis
    real(kind=8), optional, intent(out) :: phasis_(*)
    real(kind=8), optional, intent(out) :: zcold_
    real(kind=8), optional, intent(out) :: zhot_
    real(kind=8), optional, intent(in) :: tole_bound_
!
! --------------------------------------------------------------------------------------------------
!
! Comportment utility - Metallurgy
!
! Get phasis
!
! --------------------------------------------------------------------------------------------------
!
! In  fami         : Gauss family for integration point rule
! In  poum         : '-' or '+' for parameters evaluation (previous or current temperature)
! In  ipg          : current point gauss
! In  ispg         : current "sous-point" gauss
! In  meta_type    : type of metallurgy
!                       0 - No metallurgy
!                       1 - Steel
!                       2 - Zirconium
! In  nb_phasis    : total number of phasis (cold and hot)
! Out phasis       : phasis
! Out zcold        : sum of cold phasis
! Out zhot         : hot phasis
! In  tole_bound   : tolerance to project phasis proportion on boundary
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: steel(4), zirc(2)
    integer :: i_phasis_c, i_phasis, iret, nb_phasis_c
    real(kind=8) :: zcold, zhot, phasis(5), tole_bound
!
    data steel /'PFERRITE','PPERLITE','PBAINITE','PMARTENS'/
    data zirc  /'ALPHPUR','ALPHBETA'/
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(nb_phasis.le.5)
    phasis(1:5) = 0.d0
    nb_phasis_c = nb_phasis-1
    if (present(tole_bound_)) then
        tole_bound = tole_bound_
    else
        tole_bound = r8prem()
    endif
!
! - Set cold phasis
!
    do i_phasis_c = 1, nb_phasis_c
        if (meta_type.eq.1) then
            call rcvarc('F', steel(i_phasis_c), poum, fami, ipg,&
                        ispg, phasis(i_phasis_c), iret)
            if (iret .eq. 1) then
                phasis(i_phasis_c) = 0.d0
            endif
        elseif (meta_type.eq.2) then
            call rcvarc('F', zirc(i_phasis_c), poum, fami, ipg,&
                        ispg, phasis(i_phasis_c), iret)
            if (iret .eq. 1) then
                phasis(i_phasis_c) = 0.d0
            endif
        else
            ASSERT(.false.)
        endif
    end do
!
! - Project phasis proportion on boundary
!
    do i_phasis = 1, nb_phasis
        if (phasis(i_phasis) .le. tole_bound) then
            phasis(i_phasis) = 0.d0
        endif
        if (phasis(i_phasis) .ge. 1.d0) then
            phasis(i_phasis) = 1.d0
        endif
    end do
!
! - Sum of cold phasis
!
    zcold = 0.d0
    do i_phasis_c = 1, nb_phasis_c
        zcold = zcold + phasis(i_phasis_c)
    end do
    if (zcold .le. tole_bound) then
        zcold = 0.d0
    endif
    if (zcold .ge. 1.d0) then
        zcold = 1.d0
    endif
!
! - Set hot phasis
!
    zhot = 1.d0 - zcold
    phasis(nb_phasis) = zhot
    if (zhot .le. tole_bound) then
        zhot = 0.d0
    endif
    if (zhot .ge. 1.d0) then
        zhot = 1.d0
    endif
!
    if (present(phasis_)) then
        phasis_(1:nb_phasis) = phasis(1:nb_phasis)
    endif
    if (present(zcold_)) then
        zcold_ = zcold
    endif
    if (present(zhot_)) then
        zhot_  = zhot
    endif
!
end subroutine
