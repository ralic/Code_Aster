subroutine onerrf(set, get, long)
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
    implicit none
! person_in_charge: mathieu.courtois at edf.fr
!     ----------------------------------------------------------------
!     QUE FAIT-ON EN CAS D'ERREUR <F> ?
!        VEXCF = 0 == ARRET AVEC ABORT
!        VEXCF = 1 == EXCEPTION FATALE (SUPPRESSION DU CONCEPT COURANT)
!        VEXCF = 2 == EXCEPTION SURE (ON VALIDERA LE CONCEPT COURANT)
!
!     POUR DEFINIR LE COMPORTEMENT EN CAS D'ERREUR : SET EN IN
!     POUR RECUPERER LA VALEUR COURANTE : SET=' ', GET, LONG EN OUT
!
! IN  SET : VALEUR DU NOUVEAU COMPORTEMENT (ABORT OU EXCEPTION)
! OUT GET : VALEUR ACTUELLE DU COMPORTEMENT
! OUT LEN : LONGUEUR DE GET EN SORTIE
!
!     VINI CONTIENT LA VALEUR FIXEE PAR DEBUT (INITIALISER AU 1ER APPEL)
!     ----------------------------------------------------------------
#include "asterfort/jefini.h"
#include "asterfort/lxlgut.h"
    character(len=*) :: set
    character(len=16) :: get
    integer :: long
!     ----------------------------------------------------------------
    integer :: vexcf, vini
    save             vexcf, vini
    data vexcf       /  0 /
    data vini        / -1 /
!     ----------------------------------------------------------------
!
!     SI DANS DEBUT, ON A CHOISI ABORT, ON Y RESTE
    if (vini .eq. 0) then
        get='ABORT'
        goto 99
    endif
!
    get = set
! --- L'INITIALISATION EST FAITE PAR UN PREMIER APPEL AVEC SET<>' '
!
! --- ON RECUPERE LA VALEUR ACTUELLE
    if (set(1:1) .eq. ' ') then
!        ON RETOURNE LA VALEUR COURANTE
        if (vexcf .eq. 0) then
            get='ABORT'
        else if (vexcf .eq. 1) then
            get='EXCEPTION'
        else if (vexcf .eq. 2) then
            get='EXCEPTION+VALID'
        else
            write(6,*) 'ERREUR DE PROGRAMMATION : ONERRF NUMERO 1'
            call jefini('ERREUR')
        endif
!
! --- ON POSITIONNE LA VALEUR
    else if (set .eq. 'INIT') then
!        PERMET DE REMETTRE LA VALEUR INITIALE
        vexcf=vini
!
    else if (set .eq. 'EXCEPTION+VALID') then
!        EXCEPTION EN CAS D'ERREUR ET VALIDATION DU CONCEPT COURANT
        vexcf=2
!
    else if (set .eq. 'EXCEPTION') then
!        EXCEPTION EN CAS D'ERREUR ET SUPPRESSION DU CONCEPT COURANT
        vexcf=1
!
    else if (set .eq. 'ABORT') then
!        "CALL ABORT" EN CAS D'ERREUR
        vexcf=0
!
    else
!        PAS D'ASSERT ICI (RECURSIVITE)
        write(6,*) 'ERREUR DE PROGRAMMATION : ONERRF NUMERO 2'
        call jefini('ERREUR')
    endif
!
99  continue
    long = lxlgut(get)
!
    if (vini .lt. 0 .and. set(1:1) .ne. ' ') then
        vini = vexcf
    endif
!
end subroutine
