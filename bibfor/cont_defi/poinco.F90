subroutine poinco(sdcont, keywf, mesh, nb_cont_zone, nb_cont_surf)
!
implicit none
!
#include "asterfort/nbzoco.h"
#include "asterfort/wkvect.h"
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
    character(len=8), intent(in) :: sdcont
    character(len=8), intent(in) :: mesh
    character(len=16), intent(in) :: keywf
    integer, intent(in) :: nb_cont_zone
    integer, intent(out) :: nb_cont_surf
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Surfaces of contact
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  keywf            : factor keyword to read
! In  mesh             : name of mesh
! In  nb_cont_zone     : number of zones of contact
! Out nb_cont_surf     : number of surfaces of contact
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_zone, nb_surf
    character(len=24) :: sdcont_pzoneco
    integer, pointer :: v_sdcont_pzoneco(:) => null()
    character(len=24) :: sdcont_psumaco, sdcont_psunoco
    integer :: j_sdcont_psumaco, j_sdcont_psunoco
    character(len=24) :: sdcont_defi
!
! --------------------------------------------------------------------------------------------------
!
    nb_cont_surf = 0
!
! - Datastructures for contact
!
    sdcont_defi    = sdcont(1:8)//'.CONTACT'
    sdcont_pzoneco = sdcont_defi(1:16)//'.PZONECO'
    sdcont_psumaco = sdcont_defi(1:16)//'.PSUMACO'
    sdcont_psunoco = sdcont_defi(1:16)//'.PSUNOCO'
!
! - Number of zones of contact
!
    call wkvect(sdcont_pzoneco, 'G V I', nb_cont_zone+1, vi = v_sdcont_pzoneco)
    do i_zone = 1, nb_cont_zone
        call nbzoco(keywf, mesh, i_zone, nb_surf)
        nb_cont_surf = nb_cont_surf + nb_surf
        v_sdcont_pzoneco(i_zone+1) = nb_cont_surf
    end do
!
! - Create datastructures: pointers to elements and nodes of contact
!
    call wkvect(sdcont_psumaco, 'G V I', nb_cont_surf+1, j_sdcont_psumaco)
    call wkvect(sdcont_psunoco, 'G V I', nb_cont_surf+1, j_sdcont_psunoco)
!
end subroutine
