subroutine nmdoin(evol, evonol, instin, numein)
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
#include "asterc/r8vide.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsorac.h"
#include "asterfort/u2mesk.h"
    character(len=24) :: evol
    logical :: evonol
    integer :: numein
    real(kind=8) :: instin
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (INITIALISATION)
!
! INSTANT ET NUME_ORDRE INITIAL
!
! ----------------------------------------------------------------------
!
!
! IN  EVOL   : NOM DU CONCEPT EVOL DANS ETAT_INIT
! IN  EVONOL : .TRUE. SI CONCEPT EVOL DANS ETAT_INIT
! OUT INSTIN : INSTANT INITIAL
!                R8VIDE SI NON DEFINI
! OUT NUMEIN : NUMERO ORDRE INSTANT INITIAL
!
!
!
!
    integer :: ibid, dernie, nume
    integer :: n1, n2, n3
    integer :: jinst
    real(kind=8) :: r8bid, prec, inst
    complex(kind=8) :: c16bid
    character(len=8) :: k8bid, criter
    character(len=16) :: motfac
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    instin = r8vide()
    numein = -1
    motfac = 'ETAT_INIT'
!
! --- RECHERCHE INSTANT INITIAL
!
    if (evonol) then
!
! ----- NUMERO D'ACCES ET INSTANT CORRESPONDANT
!
        call getvr8(motfac, 'INST', iocc=1, scal=inst, nbret=n1)
        call getvis(motfac, 'NUME_ORDRE', iocc=1, scal=nume, nbret=n2)
!
! ----- NUME_ORDRE ET INST ABSENTS, ON PREND LE DERNIER PAS ARCHIVE
!
        if (n1+n2 .eq. 0) then
            call rsorac(evol, 'DERNIER', ibid, r8bid, k8bid,&
                        c16bid, r8bid, k8bid, dernie, 1,&
                        n3)
            if (n3 .eq. 0) then
                call u2mesk('F', 'ETATINIT_2', 1, evol)
            else
                numein = dernie
                call rsadpa(evol, 'L', 1, 'INST', numein,&
                            0, jinst, k8bid)
                instin = zr(jinst)
            endif
        endif
!
! ----- ACCES PAR INSTANT
!
        if (n1 .ne. 0) then
            instin = inst
            call getvr8(motfac, 'PRECISION', iocc=1, scal=prec, nbret=ibid)
            call getvtx(motfac, 'CRITERE', iocc=1, scal=criter, nbret=ibid)
            call rsorac(evol, 'INST', ibid, instin, k8bid,&
                        c16bid, prec, criter, numein, 1,&
                        n3)
            if (n3 .eq. 0) call u2mesk('F', 'ETATINIT_3', 1, evol)
            if (n3 .lt. 0) call u2mesk('F', 'ETATINIT_4', 1, evol)
        endif
!
! ----- ACCES PAR NUMERO D'ORDRE
!
        if (n2 .ne. 0) then
            numein = nume
            call rsadpa(evol, 'L', 1, 'INST', numein,&
                        0, jinst, k8bid)
            instin = zr(jinst)
        endif
    endif
!
! --- DEFINITION INSTANT INITIAL
!
    call getvr8(motfac, 'INST_ETAT_INIT', iocc=1, scal=inst, nbret=n2)
    if (n2 .ne. 0) then
        instin = inst
    endif
!
    call jedema()
!
end subroutine
