subroutine carc_info(p_info_carc_valk, p_info_carc_valr, nbocc_compor)
!
    implicit none
!
#include "asterc/getfac.h"
#include "asterfort/as_allocate.h"
!
! ======================================================================
! COPYRIGHT (C) 2091 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=16), pointer, intent(inout) :: p_info_carc_valk(:)
    real(kind=8)     , pointer, intent(inout) :: p_info_carc_valr(:)
    integer          , intent(out) :: nbocc_compor
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! Create comportment informations objects
!
! --------------------------------------------------------------------------------------------------
!
! IO  p_info_carc_valk : pointer to carcri informations (character)
! IO  p_info_carc_valr : pointer to carcri informations (real)
! Out nbocc_compor     : number of comportement keywords
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: keywordfact
    integer :: nb_info_comp
!
! --------------------------------------------------------------------------------------------------
!
    nbocc_compor = 0
    keywordfact  = 'COMPORTEMENT'
    call getfac(keywordfact, nbocc_compor)
!
! - Number of comportement information
!
    if (nbocc_compor.eq.0) then
        nb_info_comp = 1
    else
        nb_info_comp = nbocc_compor
    endif
!
! - Create comportment informations objects
!
    AS_ALLOCATE(vk16 = p_info_carc_valk, size = 2*nb_info_comp)
    AS_ALLOCATE(vr   = p_info_carc_valr, size = 13*nb_info_comp)
!
end subroutine
