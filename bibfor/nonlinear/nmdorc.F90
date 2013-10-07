subroutine nmdorc(model, chmate, l_etat_init, compor, carcri)
!
    implicit none
!
#include "asterfort/nmdocc.h"
#include "asterfort/nmdocr.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: model
    character(len=8), intent(in) :: chmate
    logical, intent(in) :: l_etat_init
    character(len=19), intent(out) :: compor
    character(len=24), intent(out) :: carcri
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! SAISIE ET VERIFICATION DU MOT-CLE COMPORTEMENT
!
! --------------------------------------------------------------------------------------------------
!
! In  model       : name of model
! In  chmate      : name of material field
! In  l_etat_init : .true. if initial state is defined
! Out compor      : name of <CARTE> COMPOR
! Out carcri      : name of <CARTE> CARCRI
!
! --------------------------------------------------------------------------------------------------

!
! - CARTE COMPOR
!
    call nmdocc(model, chmate, l_etat_init, compor)
!
! - CARTE DE CRITERES LOCAUX
!
    call nmdocr(model, carcri)
!
end subroutine
