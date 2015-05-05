subroutine eneven(sddisc, i_event, lacti)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/utdidt.h"
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
    character(len=19), intent(in) :: sddisc
    integer, intent(in) :: i_event
    aster_logical :: lacti
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE EVENEMENT
!
! ENREGISTRE UN EVENEMENT COMME ETANT ACTIVE OU PAS
!
! ----------------------------------------------------------------------
!
! In  sddisc           : datastructure for time discretization
! IN  IEVENT : INDICE DE L'EVENEMENT ACTIVE
! IN  LACTI  : .TRUE. SI ACTIVATION
!              .FALSE. SI DESACTIVATION
!
! ----------------------------------------------------------------------
!
    character(len=16) :: active
!
! ----------------------------------------------------------------------
!
    if (i_event .ne. 0) then
        if (lacti) then
            active = 'OUI'
        else
            active = 'NON'
        endif
        call utdidt('E', sddisc, 'ECHE', 'VERIF_EVEN', index_ = i_event,&
                    valk_ = active)
    endif
!
end subroutine
