subroutine xreacg(model, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/infdbg.h"
#include "asterfort/xgecfi.h"
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
!
    character(len=8), intent(in) :: model
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM-GG
!
! CREER ET REACTUALISER LA GEOMETRIE DES FACETTES DE CONTACT
!
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
! In  model            : name of model
! In  ds_contact       : datastructure for contact management
!
!
    character(len=19) :: depla
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call infdbg('XFEM', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<XFEM> ... REACTUALISATION DES FACETTES DE CONTACT'
    endif
!
    depla = ds_contact%sdcont_solv(1:14)//'.DEPG'
    call xgecfi(model, depla)
!
end subroutine
