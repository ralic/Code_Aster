subroutine caracc(sdcont, nb_cont_zone)
!
implicit none
!
#include "asterfort/cfmmvd.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: sdcont
    integer, intent(in) :: nb_cont_zone
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Creation of datastructures for continue formulation (depending on contact zone)
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  nb_cont_zone     : number of zones of contact
!
! --------------------------------------------------------------------------------------------------
!
    character(len=1) :: jv_base
    character(len=24) :: sdcont_defi
    integer :: zcmcf, zexcl
    character(len=24) :: sdcont_caracf, sdcont_exclfr
    integer :: j_sdcont_caracf, j_sdcont_exclfr
!
! --------------------------------------------------------------------------------------------------
!
    jv_base     = 'G'
    sdcont_defi = sdcont(1:8)//'.CONTACT'
!
! - Sizes
!
    zcmcf = cfmmvd('ZCMCF')
    zexcl = cfmmvd('ZEXCL')
!
! - Datastructure for contact definition
!
    sdcont_caracf = sdcont_defi(1:16)//'.CARACF'
    sdcont_exclfr = sdcont_defi(1:16)//'.EXCLFR'
!
! - Creation
!
    call wkvect(sdcont_caracf, jv_base//' V R', zcmcf*nb_cont_zone, j_sdcont_caracf)
    call wkvect(sdcont_exclfr, jv_base//' V R', zexcl*nb_cont_zone, j_sdcont_exclfr)
!
end subroutine
