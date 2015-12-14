subroutine cfliin(mesh, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfecrd.h"
#include "asterfort/cfimp1.h"
#include "asterfort/cfinal.h"
#include "asterfort/cfinnl.h"
#include "asterfort/infdbg.h"
#include "asterfort/mmbouc.h"
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
    character(len=8), intent(in) :: mesh
    type(NL_DS_Contact), intent(inout) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Discrete methods - Set initial links
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! IO  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    aster_logical :: l_pair, l_first_geom
    integer :: nbliac, llf, llf1, llf2
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ......... DETECTION DES LIAISONS INITIALES'
    endif
!
! - Get geometric loop state
!
    l_pair       = ds_contact%l_pair
    l_first_geom = ds_contact%l_first_geom
!
! - Get total number of initial links
!
    call cfinnl(ds_contact, l_pair, nbliac, llf,&
                llf1, llf2)
!
! - Set initial links
!
    call cfinal(ds_contact, l_first_geom, l_pair, nbliac,&
                llf, llf1, llf2)
!
! - Save initial links
!
    call cfecrd(ds_contact%sdcont_solv, 'NBLIAC', nbliac)
    call cfecrd(ds_contact%sdcont_solv, 'LLF', llf)
    call cfecrd(ds_contact%sdcont_solv, 'LLF1', llf1)
    call cfecrd(ds_contact%sdcont_solv, 'LLF2', llf2)
!
! - Print
!
    if (niv .ge. 2) then
        call cfimp1('INI', mesh, ds_contact%sdcont_defi, ds_contact%sdcont_solv, ifm)
    endif
!
end subroutine
