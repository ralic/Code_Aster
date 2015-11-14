subroutine utmess_core(typ, idmess, nk, valk, ni,&
                       vali, nr, valr, fname)
use message_module
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
! person_in_charge: mathieu.courtois at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "asterc/getres.h"
#include "asterc/isjvup.h"
#include "asterc/uexcep.h"
#include "asterc/utprin.h"
#include "asterfort/asmpi_warn.h"
#include "asterfort/assert.h"
#include "asterfort/ib1mai.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetv.h"
#include "asterfort/jefini.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevema.h"
#include "asterfort/lxlgut.h"
#include "asterfort/onerrf.h"
#include "asterfort/post_op.h"
#include "asterfort/trabck.h"
    character(len=*), intent(in) :: typ
    character(len=*), intent(in) :: idmess
    integer, intent(in) :: nk
    character(len=*), intent(in) :: valk(*)
    integer, intent(in) :: ni
    integer, intent(in) :: vali(*)
    integer, intent(in) :: nr
    real(kind=8), intent(in) :: valr(*)
    character(len=*), intent(in) :: fname
!
    integer :: nexcep
    common /utexc /  nexcep
!
    integer :: recurs
    character(len=24) :: msgId
    character(len=16) :: compex
    character(len=8) :: nomres, k8b
    character(len=2) :: typm
    aster_logical :: lerror, lvalid, labort, suite, lstop, lerrm, ltrb
    integer :: lout, idf, i, lc, imaap
    integer :: numex
!
    aster_logical :: isFirst=ASTER_TRUE
    type(Message) :: firstMsg
!
    save             recurs, firstMsg, isFirst
!
!     TYPES DE MESSAGES :
!     ERREURS :
!       F : ERREUR AVEC DESTRUCTION DU CONCEPT PRODUIT PAR LA COMMANDE
!       S : ERREUR AVEC VALIDATION DU CONCEPT, EXCEPTION
!       Z : LEVEE D'EXCEPTION PARTICULIERE, COMME 'S'
!       M : ERREUR SUIVIE DE MPI_ABORT, NE PAS LEVER D'EXCEPTION --> 'F'
!     MESSAGES :
!       E : SIMPLE MESSAGE D'ERREUR QUI SERA SUIVI D'UNE ERREUR 'F'
!       D : COMME 'E' MAIS AFFICHE AVEC 'F' POUR ASSURER UN 'D'IAGNOSTIC
!       I : INFORMATION
!       A : ALARME
!
!     LE TRACEBACK INTEL, SI DISPO, EST AFFICHE EN CAS D'ERREUR OU
!     EXCEPTION DVP_NNN, OU ERREUR 'D' CAR SUIVIE DE MPI_ABORT
    msgId = idmess
    typm = typ
    idf = index('EFIMASZD', typm(1:1))
    ASSERT(idf .ne. 0)
    lstop = .false.
!
!     --- COMPORTEMENT EN CAS D'ERREUR
    call onerrf(' ', compex, lout)
!
    lerrm = idf.eq.4
    if (lerrm) then
        idf = 2
        typm(1:1) = 'F'
!       L'EXCEPTION A-T-ELLE DEJA ETE LEVEE ?
        if (recurs .ne. 0) then
!         L'EXCEPTION A DEJA ETE LEVEE
            recurs = 0
        else
            lerrm = .false.
        endif
    endif
!
    lerror = idf.eq.2 .or. idf.eq.6 .or. idf.eq.7
!     DOIT-ON VALIDER LE CONCEPT ?
    lvalid = (idf.eq.6 .or. idf.eq.7) .or. (idf.eq.2 .and. compex(1:lout).eq.'EXCEPTION+VALID')
!     DOIT-ON S'ARRETER BRUTALEMENT (POUR DEBUG) ?
    labort = idf.eq.2 .and. compex(1:lout).eq.'ABORT'
!     AFFICHIER LE TRACEBACK SI DISPONIBLE
    ltrb = labort .or. (lerror .and. msgId(1:4).eq.'DVP_') .or. idf.eq.8
!
    numex = nexcep
    if (lerror .and. idf .ne. 7) then
!     SI EXCEPTION, NEXCEP EST FIXE PAR COMMON VIA UTEXCP
!     SINON ON LEVE L'EXCEPTION DE BASE ASTER.ERROR
        numex = 21
    endif
!
    suite = .false.
    if (len(typm) .gt. 1) then
        if (typm(2:2) .eq. '+') suite=.true.
    endif
!
!   Keep the first message in memory because this is one that will be used
!   to raise the exception
    if ( isFirst ) then
        call init_message(firstMsg, typ, msgId, &
                          nk=nk, valk=valk, &
                          ni=ni, vali=vali, &
                          nr=nr, valr=valr, &
                          num_except=numex)
        isFirst = ASTER_FALSE
    endif
! --- SE PROTEGER DES APPELS RECURSIFS POUR LES MESSAGES D'ERREUR
    if (lerror) then
        if (recurs .eq. 1234567891) then
            call jefini('ERREUR')
        endif
!
        if (recurs .eq. 1234567890) then
            recurs = 1234567891
!          ON EST DEJA PASSE PAR UTMESG... SANS EN ETRE SORTI
            call utprin('F', 0, 'CATAMESS_55', 0, valk,&
                        0, vali, 0, valr, fname)
!          ON NE FAIT PLUS RIEN ET ON SORT DE LA ROUTINE
            goto 999
        endif
        recurs = 1234567890
    endif
!
    call jevema(imaap)
    if (imaap .ge. 200) call jefini('ERREUR')
    if (isjvup() .eq. 1) then
        call jemarq()
    endif
!
    call utprin(typm, numex, msgId, nk, valk,&
                ni, vali, nr, valr, fname)
!
!     --- REMONTEE D'ERREUR SI DISPO
    if (ltrb) then
        call trabck('Traceback printed by Intel compiler', int(-1, 4))
    endif
! --- EN CAS DE MESSAGE AVEC SUITE, PAS D'ARRET, PAS D'EXCEPTION
    if (.not. suite) then
!
!     -- ABORT SUR ERREUR <F> "ORDINAIRE"
        if (labort) then
!           AVERTIR LE PROC #0 QU'ON A RENCONTRE UN PROBLEME !
            call asmpi_warn(0)
!
            call jefini('ERREUR')
!
!     -- LEVEE D'UNE EXCEPTION
        else if (lerror) then
!
!        -- QUELLE EXCEPTION ?
!           SI EXCEPTION, NEXCEP EST FIXE PAR COMMON VIA UTEXCP
!           IL A ETE COPIE DANS NUMEX POUR NE PAS ETRE MODIFIE SI
!           DES APPELS SONT IMBRIQUES
            if (idf .ne. 7) then
!           SINON ON LEVE L'EXCEPTION DE BASE ASTER.ERROR
                numex = 21
            endif
!
!           NOM DU CONCEPT COURANT
            call getres(nomres, k8b, k8b)
!
            if (isjvup() .eq. 1) then
                call post_op()
            endif
!
            if (nomres .ne. ' ') then
!             LE CONCEPT EST REPUTE VALIDE :
!               - SI ERREUR <S> OU EXCEPTION
!               - SI ERREUR <F> MAIS LA COMMANDE A DIT "EXCEPTION+VALID"
                if (lvalid) then
                    call utprin('I', 0, 'CATAMESS_70', 1, nomres,&
                                0, vali, 0, valr, fname)
!
!             SINON LE CONCEPT COURANT EST DETRUIT
                else
                    call utprin('I', 0, 'CATAMESS_69', 1, nomres,&
                                0, vali, 0, valr, fname)
                    lc = lxlgut(nomres)
                    if (lc .gt. 0) then
                        call jedetc(' ', nomres(1:lc), 1)
                    endif
                endif
            endif
!
            if (isjvup() .eq. 1) then
!
!             REMONTER LES N JEDEMA COURT-CIRCUITES
                call jevema(imaap)
                do i = imaap, 1, -1
                    call jedema()
                end do
!
            endif
!
!           AVERTIR LE PROC #0 QU'ON A RENCONTRE UN PROBLEME !
            call asmpi_warn(1)
!
!           ON REMONTE UNE EXCEPTION AU LIEU DE FERMER LES BASES
            if (lerror) recurs = 0
            lstop = .true.
            if (.not. lerrm) then
!               raise the exception with the first msg id & reinit id
                isFirst = ASTER_TRUE
                call ib1mai()
                call uexcep(numex, firstMsg%id, firstMsg%nk, firstMsg%valk, firstMsg%ni,&
                            firstMsg%vali, firstMsg%nr, firstMsg%valr)
                call free_message(firstMsg)
            endif
        else
!           info/warning, reinit id
            isFirst = ASTER_TRUE
            call free_message(firstMsg)
        endif
!
    endif
!
    if (lerror) recurs = 0
999 continue
    if (isjvup() .eq. 1 .and. .not. lstop) then
        call jedema()
    endif
end subroutine
