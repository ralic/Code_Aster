subroutine nmevr0(sddisc)
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
#include "asterfort/nmlerr.h"
#include "asterfort/utdidt.h"
    character(len=19) :: sddisc
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - EVENEMENTS)
!
! REINITIALISATIONS DES EVENEMENTS
!
! ----------------------------------------------------------------------
!
!
! In  sddisc           : datastructure for time discretization TEMPORELLE
!
!
!
!
    integer :: itesup, i_echec, nb_echec
    real(kind=8) :: r8bid
    character(len=16) :: action, event_name
    aster_logical :: lacti
!
! ----------------------------------------------------------------------
!
    call utdidt('L', sddisc, 'LIST', 'NECHEC',&
                vali_ = nb_echec)
!
! --- DESACTIVATION DES EVENEMENTS
!
    do i_echec = 1, nb_echec
        call utdidt('L', sddisc, 'ECHE', 'NOM_EVEN', index_ = i_echec,&
                    valk_ = event_name)
        lacti = .false.
        call dieven(sddisc, i_echec, lacti)
        call utdidt('L', sddisc, 'ECHE', 'ACTION', index_ = i_echec,&
                    valk_ = action)
        if (action .eq. 'ITER_SUPPL') then
            itesup = 0
            call nmlerr(sddisc, 'E', 'ITERSUP', r8bid, itesup)
        endif
    end do
!
end subroutine
