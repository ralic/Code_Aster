subroutine ndxpre(modele, numedd, numfix, mate, carele,&
                  comref, compor, lischa, method, solveu,&
                  fonact, carcri, sddisc, sdstat, sdtime,&
                  numins, valinc, solalg, matass, maprec,&
                  sddyna, sderro, meelem, measse, veelem,&
                  veasse, lerrit)
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
    include 'asterfort/diinst.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/ndxprm.h'
    include 'asterfort/nmassx.h'
    include 'asterfort/nmchar.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/nmcret.h'
    include 'asterfort/nmltev.h'
    include 'asterfort/nmresd.h'
    include 'asterfort/vtzero.h'
    integer :: fonact(*)
    integer :: numins
    character(len=16) :: method(*)
    character(len=19) :: matass, maprec
    character(len=24) :: sdtime, sdstat
    character(len=19) :: lischa, solveu, sddisc, sddyna
    character(len=24) :: modele, mate, carele, comref, compor
    character(len=24) :: numedd, numfix
    character(len=24) :: carcri
    character(len=24) :: sderro
    character(len=19) :: meelem(*), veelem(*)
    character(len=19) :: measse(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
    logical :: lerrit
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! PHASE DE PREDICTION - CAS EXPLICITE
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARIABLES DE COMMANDE DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION
! IN  SOLVEU : SOLVEUR
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  LISCHA : SD LISTE DES CHARGES
! IN  CARCRI : CARTE DES CRITERES DE CONVERGENCE LOCAUX
! IN  SDSTAT : SD STATISTIQUES
! IN  SDDISC : SD DISCRETISATION
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  SDDYNA : SD DYNAMIQUE
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  MAPREC : NOM DE LA MATRICE DE PRECONDITIONNEMENT (GCPC)
! OUT MATASS : MATRICE DE RESOLUTION ASSEMBLEE
! OUT MAPREC : MATRICE DE RESOLUTION ASSEMBLEE - PRECONDITIONNEMENT
! OUT LERRIT  : .TRUE. SI ERREUR PENDANT PREDICTION
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: instap
    character(len=19) :: cncine, cndonn, cnzero
    character(len=24) :: k24bla, codere
    integer :: ldccvg, faccvg, rescvg
    real(kind=8) :: r8bid
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> CALCUL DE PREDICTION'
    endif
!
! --- INITIALISATIONS
!
    instap = diinst(sddisc,numins)
    k24bla = ' '
    cndonn = '&&CNCHAR.DONN'
    cnzero = '&&CNPART.ZERO'
    call vtzero(cndonn)
!
! --- INITIALISATION CODES RETOURS
!
    codere = '&&NDXPRE.CODERE'
    faccvg = -1
    rescvg = -1
    ldccvg = -1
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(veasse, 'VEASSE', 'CNCINE', cncine)
!
! --- CALCUL DE LA MATRICE GLOBALE
!
    call ndxprm(modele, mate, carele, compor, carcri,&
                method, lischa, numedd, numfix, solveu,&
                comref, sddisc, sddyna, sdstat, sdtime,&
                numins, fonact, valinc, solalg, veelem,&
                meelem, measse, maprec, matass, codere,&
                faccvg, ldccvg)
!
! --- ERREUR SANS POSSIBILITE DE CONTINUER
!
    if ((faccvg.eq.1) .or. (faccvg.eq.2)) goto 9999
    if (ldccvg .eq. 1) goto 9999
!
! --- CALCUL DES CHARGEMENTS VARIABLES AU COURS DU PAS DE TEMPS
!
    call nmchar('VARI', 'PREDICTION', modele, numedd, mate,&
                carele, compor, lischa, carcri, numins,&
                sdtime, sddisc, r8bid, fonact, k24bla,&
                k24bla, comref, valinc, solalg, veelem,&
                measse, veasse, sddyna)
!
! --- CALCUL DU SECOND MEMBRE
!
    call nmassx(modele, numedd, mate, carele, comref,&
                compor, lischa, carcri, fonact, sdstat,&
                sddyna, valinc, solalg, veelem, veasse,&
                sdtime, ldccvg, codere, cndonn)
!
! --- ERREUR SANS POSSIBILITE DE CONTINUER
!
    if (ldccvg .eq. 1) goto 9999
!
! --- RESOLUTION
!
    call nmresd(fonact, sddyna, sdstat, sdtime, solveu,&
                numedd, instap, maprec, matass, cndonn,&
                cnzero, cncine, solalg, rescvg)
!
9999  continue
!
! --- TRANSFORMATION DES CODES RETOURS EN EVENEMENTS
!
    call nmcret(sderro, 'LDC', ldccvg)
    call nmcret(sderro, 'FAC', faccvg)
    call nmcret(sderro, 'RES', rescvg)
!
! --- EVENEMENT ERREUR ACTIVE ?
!
    call nmltev(sderro, 'ERRI', 'NEWT', lerrit)
!
    call jedema()
end subroutine
