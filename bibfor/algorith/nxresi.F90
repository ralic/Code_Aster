subroutine nxresi(ther_crit_i, ther_crit_r, vec2nd   , cnvabt   , cnresi,&
                  temp_iter  , temp_curr  , resi_rela, resi_maxi, conver)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/copisd.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    integer, intent(in) :: ther_crit_i(*)
    real(kind=8), intent(in) :: ther_crit_r(*)
    character(len=24), intent(in) :: vec2nd
    character(len=24), intent(in) :: cnvabt
    character(len=24), intent(in) :: cnresi
    character(len=24), intent(in) :: temp_iter
    character(len=24), intent(in) :: temp_curr
    real(kind=8), intent(out) :: resi_rela
    real(kind=8), intent(out) :: resi_maxi
    aster_logical, intent(out) :: conver
!
! --------------------------------------------------------------------------------------------------
!
! THER_NON_LINE
!
! Evaluate residuals
!
! --------------------------------------------------------------------------------------------------
!

!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8), pointer :: v_temp_curr(:) => null()
    real(kind=8), pointer :: v_vec2nd(:) => null()
    real(kind=8), pointer :: v_cnvabt(:) => null()
    real(kind=8), pointer :: v_cnresi(:) => null()
    real(kind=8) :: vnorm
    integer :: nb_equa, i_equa
!
! --------------------------------------------------------------------------------------------------
!
    resi_rela  = 0.d0
    resi_maxi  = 0.d0
    vnorm      = 0.d0
    conver     = .false.
!
! - Access to vectors
!
    call jeveuo(temp_curr(1:19)//'.VALE', 'L', vr = v_temp_curr)
    call jeveuo(vec2nd(1:19)//'.VALE'   , 'L', vr = v_vec2nd)
    call jeveuo(cnvabt(1:19)//'.VALE'   , 'L', vr = v_cnvabt)
    call jeveuo(cnresi(1:19)//'.VALE'   , 'L', vr = v_cnresi)
    call jelira(temp_curr(1:19)//'.VALE', 'LONMAX', nb_equa)
!
! - Compute maximum
!
    do i_equa = 1, nb_equa
        v_temp_curr(i_equa) = v_vec2nd(i_equa) - v_cnresi(i_equa) - v_cnvabt(i_equa)
        resi_rela = resi_rela + ( v_temp_curr(i_equa) )**2
        vnorm = vnorm + ( v_vec2nd(i_equa) - v_cnvabt(i_equa) )**2
        resi_maxi = max( resi_maxi,abs( v_temp_curr(i_equa) ) )
    end do
!
! - Compute relative
!
    if (vnorm .gt. 0.d0) then
        resi_rela = sqrt( resi_rela / vnorm )
    endif
!
! - Evaluate
!
    if (ther_crit_i(1) .ne. 0) then
        if (resi_maxi .lt. ther_crit_r(1)) then
            conver = .true.
            call copisd('CHAMP_GD', 'V', temp_iter, temp_curr)
        else
            conver = .false.
        endif
    else
        if (resi_rela .lt. ther_crit_r(2)) then
            conver = .true.
            call copisd('CHAMP_GD', 'V', temp_iter, temp_curr)
        else
            conver = .false.
        endif
    endif

end subroutine
