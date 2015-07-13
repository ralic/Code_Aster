subroutine quadco(sdcont, l_node_q8)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
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
    aster_logical, intent(out) :: l_node_q8
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Discrete method - QUAD8 specific treatment activation
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! Out l_node_q8        : .true. if nead linearization for QUAD8
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_defi
    aster_logical :: l_all_verif, l_pena_cont, l_gliss
!
! --------------------------------------------------------------------------------------------------
!
    l_node_q8   = .true.
    sdcont_defi = sdcont(1:8)//'.CONTACT'
!
! - Parameters
!
    l_all_verif = cfdisl(sdcont_defi,'ALL_VERIF')
    l_pena_cont = cfdisl(sdcont_defi,'CONT_PENA')
    l_gliss     = cfdisl(sdcont_defi,'CONT_DISC_GLIS')
!
! - Activate elimination of QUAD8 middle nodes
!
    if (l_all_verif .or. l_pena_cont .or. l_gliss) then
        l_node_q8 = .false.
    else
        l_node_q8 = .true.
    endif
!
end subroutine
