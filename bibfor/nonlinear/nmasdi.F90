subroutine nmasdi(fonact, veasse, cndfdo, cndfpi)
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
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdebg.h"
#include "asterfort/vtaxpy.h"
#include "asterfort/vtzero.h"
    character(len=19) :: cndfdo, cndfpi
    character(len=19) :: veasse(*)
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL DES COMPOSANTES DU VECTEUR SECOND MEMBRE
!  - CHARGEMENT DE TYPE DIRICHLET
!  - CHARGEMENT FIXE AU COURS DU PAS DE TEMPS
!  - CHARGEMENT DONNE ET PILOTE
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! OUT CNDFDO : VECT_ASSE DE TOUS LES DEPLACEMENTS FIXES DONNES
! OUT CNDFPI : VECT_ASSE DE TOUS LES DEPLACEMENTS FIXES PILOTES
!
!
!
!
!
    integer :: ifm, niv
    integer :: ifdo
    integer :: n
    character(len=19) :: cndonn(20)
    real(kind=8) :: codonn(20)
    character(len=19) :: cndido, cndipi
    character(len=19) :: cncine, cndidi
    logical(kind=1) :: ldidi, lpilo
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ...... CALCUL DIRICHLET CONSTANT'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    lpilo = isfonc(fonact,'PILOTAGE')
    ldidi = isfonc(fonact,'DIDI')
!
! --- INITIALISATIONS
!
    ifdo = 0
    call vtzero(cndfdo)
    call vtzero(cndfpi)
!
! --- DEPLACEMENTS DONNES (Y COMPRIS DIDI SI NECESSAIRE)
!
    call nmchex(veasse, 'VEASSE', 'CNDIDO', cndido)
    ifdo = ifdo+1
    cndonn(ifdo) = cndido
    codonn(ifdo) = 1.d0
!
    if (ldidi) then
        call nmchex(veasse, 'VEASSE', 'CNDIDI', cndidi)
        ifdo = ifdo+1
        cndonn(ifdo) = cndidi
        codonn(ifdo) = 1.d0
    endif
!
! --- CONDITIONS CINEMATIQUES IMPOSEES
!
    call nmchex(veasse, 'VEASSE', 'CNCINE', cncine)
    ifdo = ifdo+1
    cndonn(ifdo) = cncine
    codonn(ifdo) = 1.d0
!
! --- VECTEUR RESULTANT DEPLACEMENTS DONN2S
!
    do 17 n = 1, ifdo
        call vtaxpy(codonn(n), cndonn(n), cndfdo)
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ......... DEPL. DONNES'
            write (ifm,*) '<MECANONLINE> .........  ',n,' - COEF: ',&
     &                   codonn(n)
            call nmdebg('VECT', cndonn(n), ifm)
        endif
17  end do
!
! --- VECTEUR RESULTANT DEPLACEMENTS PILOTES
!
    if (lpilo) then
        call nmchex(veasse, 'VEASSE', 'CNDIPI', cndipi)
        cndfpi = cndipi
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ......... DEPL. PILOTES'
            write (ifm,*) '<MECANONLINE> .........  ',1,' - COEF: ',&
     &                   1.d0
            call nmdebg('VECT', cndfpi, ifm)
        endif
!
    endif
!
    call jedema()
end subroutine
