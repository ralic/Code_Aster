subroutine get_meta_phasis(fami     , poum  , ipg   , ispg , meta_type,&
                           nb_phasis, phasis, zcold_, zhot_)
!
implicit none
!
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
    real(kind=8), intent(out) :: phasis(*)
    real(kind=8), optional, intent(out) :: zcold_
    real(kind=8), optional, intent(out) :: zhot_
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
! In  nb_phasis    : number of phasis
! Out phasis       : phasis
! Out zcold        : sum of cold phasis
! Out zhot         : hot phasis
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: steel(4), zirc(2)
    integer :: i_phasis, iret
    real(kind=8) :: zcold
!
    data steel /'PFERRITE','PPERLITE','PBAINITE','PMARTENS'/
    data zirc  /'ALPHPUR','ALPHBETA'/
!
! --------------------------------------------------------------------------------------------------
!
    do i_phasis = 1, nb_phasis
        if (meta_type.eq.1) then
            call rcvarc('F', steel(i_phasis), poum, fami, ipg,&
                        ispg, phasis(i_phasis), iret)
            if (iret .eq. 1) then
                phasis(i_phasis) = 0.d0
            endif
        elseif (meta_type.eq.2) then
            call rcvarc('F', zirc(i_phasis), poum, fami, ipg,&
                        ispg, phasis(i_phasis), iret)
            if (iret .eq. 1) then
                phasis(i_phasis) = 0.d0
            endif
        else
            ASSERT(.false.)
        endif
    end do
!
    zcold = 0.d0
    do i_phasis = 1, nb_phasis
        zcold = zcold + phasis(i_phasis)
    end do
!
    if (present(zcold_)) then
        zcold_ = zcold
    endif
    if (present(zhot_)) then
        zhot_  = 1.d0 - zcold 
    endif
end subroutine
