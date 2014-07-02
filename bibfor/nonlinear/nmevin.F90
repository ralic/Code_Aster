subroutine nmevin(sddisc, resoco, iechec, ievdac)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfdisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utdidt.h"
    character(len=24) :: resoco
    integer :: iechec, ievdac
    character(len=19) :: sddisc
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - EVENEMENTS)
!
! GESTION DE L'EVENEMENT INTERPENETRATION
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  IECHEC : OCCURRENCE DE L'ECHEC
! OUT IEVDAC : VAUT IECHEC SI EVENEMENT DECLENCHE
!                   0 SINON
!
!
!
!
    integer :: ifm, niv
    integer :: nbliai
    integer :: iliai
    character(len=24) :: jeuite
    integer :: jjeuit
    real(kind=8) :: jeufin, pnmaxi
    aster_logical :: levent
    real(kind=8) :: penmax
    integer :: ibid
    character(len=8) :: k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... INTERPENETRATION'
    endif
!
! --- INITIALISATIONS
!
    call utdidt('L', sddisc, 'ECHE', iechec, 'PENE_MAXI',&
                penmax, ibid, k8bid)
    ievdac = 0
    levent = .false.
    pnmaxi = 0.d0
!
! --- PARAMETRES
!
    nbliai = cfdisd(resoco,'NBLIAI')
!
! --- ACCES OBJETS DU CONTACT
!
    jeuite = resoco(1:14)//'.JEUITE'
    call jeveuo(jeuite, 'L', jjeuit)
!
! --- DETECTION PENETRATION
!
    do 10 iliai = 1, nbliai
        jeufin = zr(jjeuit+3*(iliai-1)+1-1)
        if (jeufin .le. 0.d0) then
            if (abs(jeufin) .gt. penmax) then
                if (abs(jeufin) .gt. pnmaxi) pnmaxi = abs(jeufin)
                levent = .true.
            endif
        endif
 10 end do
!
! --- ACTIVATION EVENEMENT
!
    if (levent) then
        ievdac = iechec
    endif
!
    call jedema()
end subroutine
