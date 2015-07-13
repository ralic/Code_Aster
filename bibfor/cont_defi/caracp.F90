subroutine caracp(sdcont)
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
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Creation of datastructures for all formulations (Not depending on contact zone)
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=1) :: jv_base
    character(len=24) :: sdcont_defi
    character(len=24) :: sdcont_paraci, sdcont_paracr, sdcont_ndimco
    integer :: j_sdcont_paracr, j_sdcont_paraci, j_sdcont_ndimco
    integer :: zparr, zpari, zdime
!
! --------------------------------------------------------------------------------------------------
!
    jv_base = 'G'
!
! - Datastructure for contact definition
!
    sdcont_defi = sdcont(1:8)//'.CONTACT'
    sdcont_paraci = sdcont_defi(1:16)//'.PARACI'
    sdcont_paracr = sdcont_defi(1:16)//'.PARACR'
    sdcont_ndimco = sdcont_defi(1:16)//'.NDIMCO'
!
! - Sizes
!
    zparr = cfmmvd('ZPARR')
    zpari = cfmmvd('ZPARI')
    zdime = cfmmvd('ZDIME')
!
! - Creation
!
    call wkvect(sdcont_paracr, jv_base//' V R', zparr, j_sdcont_paracr)
    call wkvect(sdcont_paraci, jv_base//' V I', zpari, j_sdcont_paraci)
    call wkvect(sdcont_ndimco, jv_base//' V I', zdime, j_sdcont_ndimco)
!
end subroutine
