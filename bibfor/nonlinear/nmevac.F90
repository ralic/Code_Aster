subroutine nmevac(sdimpr, sddisc, sderro, defico, resoco,&
                  solveu, ievdac, numins, iterat, retact)
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
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmadcp.h"
#include "asterfort/nmdeco.h"
#include "asterfort/nmecev.h"
#include "asterfort/nmeraz.h"
#include "asterfort/nmerge.h"
#include "asterfort/nmevdp.h"
#include "asterfort/nmitsp.h"
#include "asterfort/nmrepc.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
    character(len=24) :: sdimpr, sderro
    character(len=24) :: defico, resoco
    character(len=19) :: sddisc, solveu
    integer :: ievdac
    integer :: iterat, numins
    integer :: retact
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! ACTIONS SUITE A UN EVENEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  SDIMPR : SD AFFICHAGE
! IN  SDDISC : SD DISCRETISATION
! IN  SDERRO : SD ERREUR
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  SOLVEU : SD SOLVEUR
! IN  IEVDAC : INDICE DE L'EVENEMENT ACTIF
! IN  NUMINS : NUMERO D'INSTANT
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! OUT RETACT : CODE RETOUR
!     0 - ON NE FAIT RIEN
!     1 - ON REFAIT LE PAS DE TEMPS
!     2 - ON CONTINUE LA BOUCLE DE NEWTON (ITERATIONS EN PLUS)
!     3 - L'ACTION A ECHOUE
!
! ----------------------------------------------------------------------
!
    integer :: ibid
    real(kind=8) :: r8bid
    character(len=16) :: action, nomevd
    integer :: retrpc, retsup, retswa, retpen, retdec
    logical :: trydec, litmax
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    retact = 3
    action = 'ARRET'
    trydec = .false.
    ASSERT(ievdac.ne.0)
!
! --- RECUPERATION ERREURS PARTICULIERES
!
    litmax = .false.
    if (sderro .ne. ' ') then
        call nmerge(sderro, 'ITER_MAXI', litmax)
    endif
!
! --- EVENEMENT ET ACTION
!
    call utdidt('L', sddisc, 'ECHE', ievdac, 'NOM_EVEN',&
                r8bid, ibid, nomevd)
    call utdidt('L', sddisc, 'ECHE', ievdac, 'ACTION',&
                r8bid, ibid, action)
!
! --- REALISATION DE L'ACTION
!
    if (action .eq. 'ARRET') then
        call utmess('I', 'MECANONLINE10_30')
        retact = 3
        trydec = .false.
    else if (action.eq.'REAC_PRECOND') then
        call utmess('I', 'MECANONLINE10_31')
        call nmrepc(sddisc, solveu, ievdac, retrpc)
        if (retrpc .eq. 0) then
            trydec = .true.
        else if (retrpc.eq.1) then
            retact = 1
        else
            ASSERT(.false.)
        endif
    else if (action.eq.'ITER_SUPPL') then
        ASSERT(iterat.ge.0)
        if (litmax) then
            call utmess('I', 'MECANONLINE10_32')
            call nmitsp(sdimpr, sddisc, iterat, retsup)
        else
            retsup = 0
        endif
        if (retsup .eq. 0) then
            trydec = .true.
        else if (retsup.eq.1) then
            retact = 2
        else
            ASSERT(.false.)
        endif
    else if (action.eq.'DECOUPE') then
        trydec = .true.
    else if (action.eq.'AUTRE_PILOTAGE') then
        if (litmax) then
            call utmess('I', 'MECANONLINE10_34')
            call nmevdp(sddisc, ievdac, retswa)
        else
            retswa = 0
        endif
        if (retswa .eq. 0) then
            trydec = .true.
        else if (retswa.eq.1) then
            retact = 1
        else
            ASSERT(.false.)
        endif
    else if (action.eq.'ADAPT_COEF_PENA') then
        call utmess('I', 'MECANONLINE10_35')
        call nmadcp(sddisc, defico, resoco, ievdac, retpen)
        trydec = .false.
        if (retpen .eq. 0) then
            retact = 3
        else if (retpen.eq.1) then
            retact = 1
        else
            ASSERT(.false.)
        endif
    else if (action.eq.'CONTINUE') then
        retact = 0
    else
        ASSERT(.false.)
    endif
!
! --- CAS DE LA DECOUPE
!
    if (trydec) then
        call utmess('I', 'MECANONLINE10_33')
        call nmdeco(sddisc, numins, iterat, ievdac, retdec)
        if (retdec .eq. 0) then
            retact = 3
        else if (retdec.eq.1) then
            retact = 1
        else if (retdec.eq.2) then
            retact = 0
        else
            ASSERT(.false.)
        endif
    endif
!
! --- ECHEC DE L'ACTION -> EVENEMENT ERREUR FATALE
!
    if (retact .eq. 3) then
        call nmecev(sderro, 'E', nomevd, action)
    endif
!
! --- ON DESACTIVE LES EVENEMENTS
!
    call nmeraz(sderro, 'EVEN')
!
    call jedema()
end subroutine
