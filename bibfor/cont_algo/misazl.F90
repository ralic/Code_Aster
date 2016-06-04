subroutine misazl(ds_contact, sdnume, vector)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: sdnume
    character(len=19), intent(in) :: vector
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Set Lagrangians to zero in unknowns vector
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  sdnume           : datastructure for dof positions
! In  vector           : name of vector to modify
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_equa, nb_equa, nt_patch
    character(len=24) :: sdnuco
    integer, pointer :: p_nuco(:) => null()
    real(kind=8), pointer :: p_vale(:) => null()
    character(len=24) :: sdcont_lagc
    real(kind=8), pointer :: v_sdcont_lagc(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jeveuo(vector//'.VALE', 'E', vr = p_vale)
    call jelira(vector//'.VALE', 'LONMAX', nb_equa)
    sdnuco = sdnume(1:19)//'.NUCO'
    call jeveuo(sdnuco, 'L', vi = p_nuco)
    do i_equa = 1, nb_equa
        if (p_nuco(i_equa) .eq. 1) then
            p_vale(i_equa) = 0.d0
        endif
    end do
!
! - For LAC method
!
    if (ds_contact%l_form_lac) then
        nt_patch    = ds_contact%nt_patch
        sdcont_lagc = ds_contact%sdcont_solv(1:14)//'.LAGC'
        call jeveuo(sdcont_lagc, 'E', vr = v_sdcont_lagc)
        v_sdcont_lagc(1:nt_patch) = 0.d0
    endif
!
end subroutine
