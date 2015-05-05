subroutine nmacto(sddisc, i_echec_acti)
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
    implicit none
#include "asterf_types.h"
#include "asterfort/dieven.h"
#include "asterfort/utdidt.h"
    character(len=19), intent(in) :: sddisc
    integer, intent(out) :: i_echec_acti
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! RECHERCHE DES EVENEMENTS ACTIVES
!
! --------------------------------------------------------------------------------------------------
!
! In  sddisc           : datastructure for time discretization
! Out i_echec_acti     : Index of echec
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_echec, i_echec
    aster_logical :: lacti
!
! ----------------------------------------------------------------------
!
    call utdidt('L', sddisc, 'LIST', 'NECHEC',&
                vali_ = nb_echec)
    lacti = .false.
!
! --- BOUCLE SUR LES EVENT-DRIVEN
! --- DES QU'UN EVENT-DRIVEN EST SATISFAIT, ON SORT
! --- ON NE CHERCHE PAS A VERIFIER LES AUTRES EVENEMENTS
! --- ATTENTION, L'ORDRE D'EVALUATION A DONC UNE IMPORTANCE !
!
    i_echec_acti = 0
    do i_echec = 1, nb_echec
        call dieven(sddisc, i_echec, lacti)
        if (lacti) then
            i_echec_acti = i_echec
            goto 99
        endif
    end do
!
99  continue
!
end subroutine
