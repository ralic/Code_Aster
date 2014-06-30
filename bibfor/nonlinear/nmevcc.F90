subroutine nmevcc(sddisc, numins, defico, resoco, iechec,&
                  ievdac)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/diinst.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utdidt.h"
    character(len=24) :: defico, resoco
    integer :: iechec, ievdac, numins
    character(len=19) :: sddisc
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - EVENEMENTS)
!
! DETECTION DE L'EVENEMENT COLLISION - CAS CONTINU
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! IN  IECHEC : OCCURRENCE DE L'ECHEC
! OUT IEVDAC : VAUT IECHEC SI EVENEMENT DECLENCHE
!                   0 SINON
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=24) :: ctevco
    integer :: jctevc
    real(kind=8) :: etacin, etacfi, etacol
    real(kind=8) :: fincol, subdur
    integer :: ntpc, iptc
    real(kind=8) :: instam, instap
    logical(kind=1) :: levent
    integer :: ibid
    character(len=8) :: k8bid
    integer :: zeven
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... COLLISION'
    endif
!
! --- INITIALISATIONS
!
    ievdac = 0
    levent = .false.
    ASSERT(numins.gt.0)
!
! --- PARAMETRES
!
    ntpc = cfdisi(defico,'NTPC')
    instap = diinst(sddisc,numins)
    instam = diinst(sddisc,numins-1)
    call utdidt('L', sddisc, 'ECHE', iechec, 'SUBD_DUREE',&
                subdur, ibid, k8bid)
!
! --- ACCES OBJETS DU CONTACT
!
    ctevco = resoco(1:14)//'.EVENCO'
    call jeveuo(ctevco, 'E', jctevc)
    zeven = cfmmvd('ZEVEN')
!
! --- STATUT DE LA COLLISION
!
    do 10 iptc = 1, ntpc
        etacin = zr(jctevc+zeven*(iptc-1)+1-1)
        etacfi = zr(jctevc+zeven*(iptc-1)+2-1)
        etacol = zr(jctevc+zeven*(iptc-1)+3-1)
        fincol = zr(jctevc+zeven*(iptc-1)+4-1)
!
! ----- COLLISION EN COURS CE N'EST PAS UN EVENEMENT
!
        if (etacol .eq. 1.d0) then
            if (instap .gt. fincol) then
                etacol = 0.d0
                fincol = 0.d0
            endif
        else if (etacol.eq.0.d0) then
!
! ------- COLLISION QUI S'ACTIVE: C'EST UN EVENEMENT
!
            if ((etacin.eq.1.d0) .or. (etacfi.eq.1.d0)) then
                etacol = 1.d0
                fincol = instam+subdur
                levent = .true.
            endif
        endif
        zr(jctevc+zeven*(iptc-1)+3-1) = etacol
        zr(jctevc+zeven*(iptc-1)+4-1) = fincol
10  end do
!
! --- ACTIVATION EVENEMENT
!
    if (levent) then
        ievdac = iechec
    endif
!
    call jedema()
end subroutine
