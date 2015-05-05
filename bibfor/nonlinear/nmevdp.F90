subroutine nmevdp(sddisc, retswa)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
    integer :: retswa
    character(len=19) :: sddisc
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - EVENEMENTS)
!
! GESTION DE L'EVENEMENT DIVE_ITER_PILO
!
! ----------------------------------------------------------------------
!
!
! In  sddisc           : datastructure for time discretization TEMPORELLE
! OUT RETSWA : CODE RETOUR CHANGE PILOTAGE
!               0 ECHEC DU SWAP
!               1 SWAP OK - ON REFAIT LE PAS DE TEMPS
!
!
!
!
    integer :: piless
    character(len=8) :: pilcho
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    retswa = 0
!
! --- PARAMETRES
!
    call utdidt('L', sddisc, 'ECHE', 'ESSAI_ITER_PILO',&
                vali_ = piless)
!
! --- L'UTILISATEUR UTILISE LE PILOTAGE
! --- ET SOUHAITE BASCULER SI NON-CONVERGENCE
!
    if (piless .eq. 2) then
!
! ----- ON A DEJA CHOISI UNE SOLUTION: ON DECOUPE
!
        piless = 1
        pilcho = 'NATUREL'
        retswa = 0
        call utmess('I', 'MECANONLINE10_42')
    else if (piless.eq.1) then
!
! ----- ON RETENTE EN CHOISISSANT L'AUTRE SOLUTION
!
        piless = 2
        pilcho = 'AUTRE'
        call utmess('I', 'MECANONLINE10_43')
        retswa = 1
    else
        ASSERT(.false.)
    endif
!
! --- SAUVEGARDE INFO
!
    call utdidt('E', sddisc, 'ECHE', 'ESSAI_ITER_PILO',&
                vali_ = piless)
    call utdidt('E', sddisc, 'ECHE', 'CHOIX_SOLU_PILO',&
                valk_ = pilcho)
!
    call jedema()
end subroutine
