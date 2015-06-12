subroutine sanscc(sdcont, keywf, mesh)
!
implicit none
!
#include "asterfort/sansno.h"
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
    character(len=8), intent(in) :: mesh
    character(len=16), intent(in) :: keywf
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Get SANS_ parameters for friction
!
! --------------------------------------------------------------------------------------------------
!
! In  keywf            : factor keyword to read
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  mesh             : name of mesh
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_keyw
    parameter (nb_keyw=2)
    character(len=16) :: keyw_name(nb_keyw), keyw_type(nb_keyw)
!
    character(len=24) :: sdcont_defi
    character(len=24) :: sdcont_sanofr, sdcont_psanofr
!
! --------------------------------------------------------------------------------------------------
!
    keyw_type(1) = 'GROUP_NO'
    keyw_type(2) = 'NOEUD'
    keyw_name(1) = 'SANS_GROUP_NO_FR'
    keyw_name(2) = 'SANS_NOEUD_FR'
!
! - Datastructure for contact
!
    sdcont_defi    = sdcont(1:8)//'.CONTACT'
    sdcont_sanofr  = sdcont_defi(1:16)//'.SANOFR'
    sdcont_psanofr = sdcont_defi(1:16)//'.PSANOFR'
!
! - Read list of nodes and save them
!
    call sansno(sdcont , keywf    , mesh     , sdcont_sanofr, sdcont_psanofr,&
                nb_keyw, keyw_type, keyw_name)
!
end subroutine
