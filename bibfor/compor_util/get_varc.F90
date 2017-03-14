subroutine get_varc(fami     , kpg      , ksp      , poum   ,&
                    temp_prev, temp_curr, temp_refe, l_temp_)
!
use calcul_module, only : ca_iactif_
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    real(kind=8), intent(out) :: temp_prev
    real(kind=8), intent(out) :: temp_curr
    real(kind=8), intent(out) :: temp_refe
    aster_logical, optional, intent(out) :: l_temp_
!
! --------------------------------------------------------------------------------------------------
!
! Get external state variable at current Gauss point
!
! --------------------------------------------------------------------------------------------------
!
! In  fami         : Gauss family for integration point rule
! In  poum         : parameters evaluation
!                     '-' for previous temperature
!                     '+' for current temperature
!                     'T' for current and previous temperature => epsth is increment
! In  kpg          : current point gauss
! In  ksp          : current "sous-point" gauss
! Out temp_prev    : previous temperature
! Out temp_curr    : current temperature
! Out temp_refe    : reference temperature
! Out l_temp       : .true. if has temperature
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: elem_name
    integer :: iret_temp_prev, iret_temp_curr, iret_temp_refe
    integer :: iadzi, iazk24
    aster_logical :: l_temp
!
! --------------------------------------------------------------------------------------------------
!
    l_temp          = .false.
    iret_temp_prev  = 1
    iret_temp_curr  = 1
    iret_temp_refe  = 1
    temp_prev       = r8nnem()
    temp_curr       = r8nnem()
    temp_refe       = r8nnem()
!
! - Get temperature
!
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, temp_curr, iret_temp_curr)
    if (iret_temp_curr .eq. 0) then
! ----- Get reference temperature
        call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                    ksp, temp_refe, iret_temp_refe)
        if (iret_temp_refe .eq. 1) then
            if (ca_iactif_ .ne. 2) then
                call tecael(iadzi, iazk24)
                elem_name = zk24(iazk24-1+3) (1:8)
            else
                elem_name = 'Point'
            endif
            call utmess('F', 'COMPOR5_8', sk=elem_name)
        endif
! ----- Get temperatures
        if (poum.eq.'T' .or. poum.eq.'-') then
            call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                        ksp, temp_prev, iret_temp_prev)
        endif
    endif
!
    l_temp = iret_temp_curr.eq.0 .or. iret_temp_prev.eq.0 
    if (present(l_temp_)) then
        l_temp_ = l_temp
    endif
!
end subroutine
