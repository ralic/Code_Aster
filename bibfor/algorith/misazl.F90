subroutine misazl(sdnume, vecinc)
!
implicit none
!
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
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
    character(len=19), intent(in) :: sdnume
    character(len=19), intent(in) :: vecinc
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! MISE A ZERO DES LAGRANGIENS CONTACT/FROTTEMENT DANS VECTEUR INCONNUES
!
! ----------------------------------------------------------------------
!
!
! ----------------------------------------------------------------------
!
    integer :: i_equa, nb_equa
    character(len=24) :: sdnuco
    integer, pointer :: p_nuco(:) => null()
    real(kind=8), pointer :: p_vale(:) => null()
!
! ----------------------------------------------------------------------
!
    call jeveuo(vecinc//'.VALE', 'E', vr = p_vale)
    call jelira(vecinc//'.VALE', 'LONMAX', nb_equa)
    sdnuco = sdnume(1:19)//'.NUCO'
    call jeveuo(sdnuco, 'L', vi = p_nuco)
    do i_equa = 1,nb_equa
        if (p_nuco(i_equa) .eq. 1) then
            p_vale(i_equa) = 0.d0
        endif
    end do
end subroutine
