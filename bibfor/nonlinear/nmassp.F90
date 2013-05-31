subroutine nmassp(modele, numedd, mate, carele, comref,&
                  compor, lischa, carcri, fonact, sdstat,&
                  defico, sddyna, valinc, solalg, veelem,&
                  veasse, sdtime, ldccvg, codere, cnpilo,&
                  cndonn, sdnume, matass)
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
! aslint: disable=W1504
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/ndassp.h'
    include 'asterfort/ndynlo.h'
    include 'asterfort/nsassp.h'
    include 'asterfort/vtzero.h'
    integer :: ldccvg
    integer :: fonact(*)
    character(len=19) :: lischa, sddyna, sdnume, matass
    character(len=24) :: defico, sdtime, sdstat
    character(len=24) :: modele, numedd, mate, codere
    character(len=24) :: carele, compor, comref, carcri
    character(len=19) :: solalg(*), valinc(*)
    character(len=19) :: veasse(*), veelem(*)
    character(len=19) :: cnpilo, cndonn
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PREDICTION)
!
! CALCUL DU SECOND MEMBRE POUR LA PREDICTION
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  NUMEDD : NOM DE LA NUMEROTATION
! IN  MATE   : NOM DU CHAMP DE MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VALEURS DE REF DES VARIABLES DE COMMANDE
! IN  COMPOR : CARTE DECRIVANT LE TYPE DE COMPORTEMENT
! IN  LISCHA : SD L_CHARGES
! IN  DEFICO : SD DEFINITION CONTACT
! IN  CARCRI : CARTE DES CRITERES DE CONVERGENCE LOCAUX
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  SDSTAT : SD STATISTIQUES
! IN  SDDYNA : SD DYNAMIQUE
! IN  SDTIME : SD TIMER
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  SDNUME : SD NUMEROTATION
! OUT CNPILO : VECTEUR ASSEMBLE DES FORCES PILOTEES
! OUT CNDONN : VECTEUR ASSEMBLE DES FORCES DONNEES
! OUT CODERE : CHAM_ELEM CODE RETOUR INTEGRATION LDC
! OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 2 : ERREUR SUR LA NON VERIF. DE CRITERES PHYSIQUES
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORST
!
!
!
!
    integer :: ifm, niv
    logical :: lstat, ldyna
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CALCUL SECOND MEMBRE'
    endif
!
! --- INITIALISATIONS
!
    ldccvg = -1
    call vtzero(cnpilo)
    call vtzero(cndonn)
!
! --- FONCTIONNALITES ACTIVEES
!
    lstat = ndynlo(sddyna,'STATIQUE')
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
!
! --- EVALUATION DU SECOND MEMBRE
!
    if (ldyna) then
        call ndassp(modele, numedd, mate, carele, comref,&
                    compor, lischa, carcri, sdstat, fonact,&
                    defico, sddyna, valinc, solalg, veelem,&
                    veasse, sdtime, ldccvg, codere, cndonn,&
                    sdnume, matass)
    else if (lstat) then
        call nsassp(modele, numedd, lischa, fonact, sddyna,&
                    sdtime, valinc, veelem, veasse, cnpilo,&
                    cndonn, mate, carele, defico, matass)
    else
        call assert(.false.)
    endif
!
    call jedema()
end subroutine
