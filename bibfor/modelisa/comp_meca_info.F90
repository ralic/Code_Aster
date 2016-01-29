subroutine comp_meca_info(p_info_comp_valk, p_info_comp_vali, p_info_comp_nvar, nbocc_compor)
!
implicit none
!
#include "asterc/getfac.h"
#include "asterfort/as_allocate.h"
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
    character(len=16), pointer, intent(out) :: p_info_comp_valk(:)
    integer          , pointer, intent(out) :: p_info_comp_vali(:)
    integer          , pointer, intent(out) :: p_info_comp_nvar(:)
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
! IO  p_info_comp_valk : comportment informations (character)
! IO  p_info_comp_vali : comportment informations (integer)
! IO  p_info_comp_nvar : comportment informations (int. vari. count)
! Out nbocc_compor     : number of comportement keywords
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: keywordfact
    integer :: icomp, nb_info_comp
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
    AS_ALLOCATE(vk16 = p_info_comp_valk, size = 16*nb_info_comp)
    AS_ALLOCATE(vi   = p_info_comp_vali, size = nb_info_comp )
    AS_ALLOCATE(vi   = p_info_comp_nvar, size = 10*nb_info_comp)
!
! - If nothing in COMPORTEMENT: all is elastic
!
    if (nbocc_compor.eq.0) then
        do icomp = 1, 16
            p_info_comp_valk(icomp) = 'VIDE'
        enddo
        p_info_comp_valk(1) = 'ELAS'
        p_info_comp_valk(2) = 'PETIT'
        p_info_comp_valk(3) = 'COMP_ELAS'
        p_info_comp_valk(4) = 'ANALYTIQUE'
    endif
!
end subroutine
