subroutine caracd(sdcont, nb_cont_zone)
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
! Creation of datastructures for discrete formulation (depending on contact zone)
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
    integer :: zcmdf
    character(len=24) :: sdcont_caradf
    integer :: j_sdcont_caradf
!
! --------------------------------------------------------------------------------------------------
!
    jv_base     = 'G'
    sdcont_defi = sdcont(1:8)//'.CONTACT'
!
! - Sizes
!
    zcmdf = cfmmvd('ZCMDF')
!
! - Datastructure for contact definition
!
    sdcont_caradf = sdcont_defi(1:16)//'.CARADF'
!
! - Creation
!
    call wkvect(sdcont_caradf, jv_base//' V R', zcmdf*nb_cont_zone, j_sdcont_caradf)
!
end subroutine
