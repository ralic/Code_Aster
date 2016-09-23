subroutine nmdorc(model, chmate, l_etat_init, compor, carcri, mult_comp_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/nmdocc.h"
#include "asterfort/nmdocm.h"
#include "asterfort/nmdocr.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=*), intent(in) :: model
    character(len=*), intent(in) :: chmate
    aster_logical, intent(in) :: l_etat_init
    character(len=*), intent(in) :: compor
    character(len=*), intent(in) :: carcri
    character(len=*), optional, intent(in) :: mult_comp_
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Read objects for constitutive laws
!
! --------------------------------------------------------------------------------------------------
!
! In  model       : name of model
! In  chmate      : name of material field
! In  l_etat_init : .true. if initial state is defined
! In  compor      : name of <CARTE> COMPOR
! In  carcri      : name of <CARTE> CARCRI
! In  mult_comp   : name of <CARTE> MULT_COMP
!
! --------------------------------------------------------------------------------------------------
!
!
! - Get parameters from COMPORTEMENT keyword and prepare COMPOR <CARTE>
!
    call nmdocc(model, chmate, l_etat_init, compor)
!
! - Get parameters from COMPORTEMENT keyword and prepare CARCRI <CARTE>
!
    call nmdocr(model, carcri)
!
! - Get parameters from COMPORTEMENT keyword and prepare MULT_COMP <CARTE> (for crystals)
!
    if (present(mult_comp_)) then
        call nmdocm(model, mult_comp_)
    endif
!
end subroutine
