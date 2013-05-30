subroutine nmevdp(sddisc, ievdac, retswa)
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
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utdidt.h'
    integer :: ievdac, retswa
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
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  IEVDAC : INDICE DE L'EVENEMENT ACTIF
! OUT RETSWA : CODE RETOUR CHANGE PILOTAGE
!               0 ECHEC DU SWAP
!               1 SWAP OK - ON REFAIT LE PAS DE TEMPS
!
!
!
!
    real(kind=8) :: r8bid
    integer :: ibid
    integer :: piless
    character(len=8) :: k8bid, pilcho
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
    call utdidt('L', sddisc, 'ECHE', ievdac, 'ESSAI_ITER_PILO',&
                r8bid, piless, k8bid)
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
        call u2mess('I', 'MECANONLINE10_42')
    else if (piless.eq.1) then
!
! ----- ON RETENTE EN CHOISISSANT L'AUTRE SOLUTION
!
        piless = 2
        pilcho = 'AUTRE'
        call u2mess('I', 'MECANONLINE10_43')
        retswa = 1
    else
        call assert(.false.)
    endif
!
! --- SAUVEGARDE INFO
!
    call utdidt('E', sddisc, 'ECHE', ievdac, 'ESSAI_ITER_PILO',&
                r8bid, piless, k8bid)
    call utdidt('E', sddisc, 'ECHE', ievdac, 'CHOIX_SOLU_PILO',&
                r8bid, ibid, pilcho)
!
    call jedema()
end subroutine
