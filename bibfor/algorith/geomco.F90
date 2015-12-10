subroutine geomco(noma, ds_contact, depplu)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/infdbg.h"
#include "asterfort/vtgpld.h"
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
    character(len=8) :: noma
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19) :: depplu
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
!
! REACTUALISATION DE LA GEOMETRIE
!
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! In  ds_contact       : datastructure for contact management
! IN  DEPPLU : CHAMP DE DEPLACEMENTS A L'ITERATION DE NEWTON PRECEDENTE
!
    character(len=19) :: oldgeo, newgeo
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... REACTUALISATION DE LA GEOMETRIE'
    endif
!
    oldgeo = noma(1:8)//'.COORDO'
    newgeo = ds_contact%sdcont_solv(1:14)//'.NEWG'
    call vtgpld('CUMU', oldgeo, 1.d0, depplu, 'V',&
                newgeo)
!
end subroutine
