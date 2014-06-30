subroutine nmevr0(sddisc)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/dieven.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
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
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
!
!
!
!
    integer :: ibid
    integer :: itesup, iechec, nechec
    real(kind=8) :: r8bid
    character(len=8) :: k8bid
    character(len=16) :: action, nomevd
    logical(kind=1) :: lacti
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NOMBRE D'EVENT-DRIVEN : NECHEC
!
    call utdidt('L', sddisc, 'LIST', ibid, 'NECHEC',&
                r8bid, nechec, k8bid)
!
! --- DESACTIVATION DES EVENEMENTS
!
    do 100 iechec = 1, nechec
!
! ----- RECUPERATION DU NOM DE L'EVENT-DRIVEN
!
        call utdidt('L', sddisc, 'ECHE', iechec, 'NOM_EVEN',&
                    r8bid, ibid, nomevd)
        lacti = .false.
        call dieven(sddisc, iechec, lacti)
        call utdidt('L', sddisc, 'ECHE', iechec, 'ACTION',&
                    r8bid, ibid, action)
        if (action .eq. 'ITER_SUPPL') then
            itesup = 0
            call nmlerr(sddisc, 'E', 'ITERSUP', r8bid, itesup)
        endif
100  end do
!
    call jedema()
!
end subroutine
