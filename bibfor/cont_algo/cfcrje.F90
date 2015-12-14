subroutine cfcrje(ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/cfdisi.h"
#include "asterfort/infdbg.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Discrete methods - Create datastructures for gaps
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nt_cont_poin
    character(len=24) :: sdcont_jeuite, sdcont_jeusav, sdcont_jeux
    integer :: jv_sdcont_jeuite, jv_sdcont_jeusav, jv_sdcont_jeux
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... CREATION DES SD POUR LES JEUX'
    endif
!
! - Get contact parameters
!
    nt_cont_poin = cfdisi(ds_contact%sdcont_defi,'NTPC')
!
! - Updated gaps
!
    sdcont_jeuite = ds_contact%sdcont_solv(1:14)//'.JEUITE'
    call wkvect(sdcont_jeuite, 'V V R', 3*nt_cont_poin, jv_sdcont_jeuite)
!
! - Gaps at beginning of step time (for cut management)
!
    sdcont_jeusav = ds_contact%sdcont_solv(1:14)//'.JEUSAV'
    call wkvect(sdcont_jeusav, 'V V R', 3*nt_cont_poin, jv_sdcont_jeusav)
!
! - Gaps
!
    sdcont_jeux = ds_contact%sdcont_solv(1:14)//'.JEUX'
    call wkvect(sdcont_jeux, 'V V R', 3*nt_cont_poin, jv_sdcont_jeux)
!
end subroutine
