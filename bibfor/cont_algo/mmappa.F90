subroutine mmappa(mesh, nume_dof, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/apcalc.h"
#include "asterfort/infdbg.h"
#include "asterfort/mmapre.h"
#include "asterfort/mmpoin.h"
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
    character(len=24), intent(in) :: nume_dof
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Pairing 
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  nume_dof         : name of numbering object (NUME_DDL)
! In  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: sdappa
    character(len=19) :: newgeo
    integer :: ifm, niv
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> .. Pairing'
    endif
!
! - Pairing datastructure
!
    sdappa = ds_contact%sdcont_solv(1:14)//'.APPA'
!
! - New geometry name
!
    newgeo = ds_contact%sdcont_solv(1:14)//'.NEWG'
!
! - Set pairing datastructure
!
    call mmpoin(mesh, ds_contact%sdcont_defi, newgeo, sdappa)
!
! - Pairing
!
    call apcalc(sdappa, mesh, ds_contact%sdcont_defi, newgeo)
!
! - Save pairing in contact datastructures
!
    call mmapre(mesh, nume_dof, ds_contact, sdappa)
!
end subroutine
