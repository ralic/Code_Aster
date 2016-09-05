subroutine dbr_calc_sele(ds_para, s, nb_sing, nb_mode)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    type(ROM_DS_ParaDBR), intent(in) :: ds_para
    real(kind=8), intent(in), pointer :: s(:)
    integer, intent(in) :: nb_sing
    integer, intent(out) :: nb_mode
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Compute
!
! Select singular vectors
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para          : datastructure for parameters
! In  nb_sing          : total number of singular values
! In  s                : singular values
! Out nb_mode          : number of modes selected
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    real(kind=8) :: tole_svd, vale_mini, vale_maxi, vale_tole, valr(2)
    integer :: i_sing, nb_mode_maxi, vali(2)
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_4')
    endif
!
! - Init
!
    nb_mode   = 0
!
! - Get parameters
!
    nb_mode_maxi = ds_para%nb_mode_maxi
    tole_svd     = ds_para%tole_svd
    vale_mini    = s(nb_sing)
    vale_maxi    = s(1)
    vale_tole    = tole_svd*vale_maxi
!
! - Select singular values
!
    do i_sing = 1, nb_sing
        if (s(i_sing) .ge. vale_tole) then
            nb_mode = nb_mode + 1
        endif
    end do
    if (nb_mode_maxi .ne. 0) then
        if (nb_mode .gt. nb_mode_maxi) then
            nb_mode = nb_mode_maxi
        endif
    endif
    valr(1)   = vale_mini
    valr(2)   = vale_maxi
    vali(1)   = nb_sing
    vali(2)   = nb_mode
    call utmess('I', 'ROM5_5', ni = 2, vali = vali, nr = 2, valr = valr)
!
    if (nb_mode .lt. 1) then
        call utmess('F', 'ROM5_6')
    endif
!
end subroutine
