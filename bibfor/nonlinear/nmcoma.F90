subroutine nmcoma(modelz, mate, carele, compor, carcri,&
                  parmet, method, lischa, numedd, numfix,&
                  solveu, comref, sddisc, sddyna, sdimpr,&
                  sdstat, sdtime, numins, iterat, fonact,&
                  defico, resoco, valinc, solalg, veelem,&
                  meelem, measse, veasse, maprec, matass,&
                  codere, faccvg, ldccvg, sdnume)
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
! TOLE CRP_21
!
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/isfonc.h'
    include 'asterfort/ndynlo.h'
    include 'asterfort/nmaint.h'
    include 'asterfort/nmchcc.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/nmchoi.h'
    include 'asterfort/nmchra.h'
    include 'asterfort/nmchrm.h'
    include 'asterfort/nmcmat.h'
    include 'asterfort/nmfint.h'
    include 'asterfort/nmimck.h'
    include 'asterfort/nmmatr.h'
    include 'asterfort/nmrenu.h'
    include 'asterfort/nmrinc.h'
    include 'asterfort/nmtime.h'
    include 'asterfort/nmxmat.h'
    include 'asterfort/preres.h'
    real(kind=8) :: parmet(*)
    character(len=16) :: method(*)
    integer :: fonact(*)
    character(len=*) :: modelz
    character(len=24) :: mate, carele
    character(len=24) :: sdimpr, sdtime, sdstat
    character(len=24) :: compor, carcri, numedd, numfix
    character(len=19) :: sddisc, sddyna, lischa, solveu, sdnume
    character(len=24) :: comref, codere
    character(len=19) :: meelem(*), veelem(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=19) :: measse(*), veasse(*)
    integer :: numins, iterat, ibid
    character(len=24) :: defico, resoco
    character(len=19) :: maprec, matass
    integer :: faccvg, ldccvg
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! CALCUL DE LA MATRICE GLOBALE EN CORRECTION
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  DEFICO : SD DEF. CONTACT
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION (VOIR NMLECT)
! IN  PARMET : PARAMETRES DES METHODES DE RESOLUTION (VOIR NMLECT)
! IN  SOLVEU : SOLVEUR
! IN  CARCRI : PARAMETRES METHODES D'INTEGRATION LOCALES (VOIR NMLECT)
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDIMPR : SD AFFICHAGE
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  NUMINS : NUMERO D'INSTANT
! IN  ITERAT : NUMERO D'ITERATION
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! OUT LFINT  : .TRUE. SI FORCES INTERNES CALCULEES
! OUT MATASS : MATRICE DE RESOLUTION ASSEMBLEE
! OUT MAPREC : MATRICE DE RESOLUTION ASSEMBLEE - PRECONDITIONNEMENT
! OUT FACCVG : CODE RETOUR FACTORISATION MATRICE GLOBALE
!                -1 : PAS DE FACTORISATION
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : MATRICE SINGULIERE
!                 2 : ERREUR LORS DE LA FACTORISATION
!                 3 : ON NE SAIT PAS SI SINGULIERE
! OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 2 : ERREUR SUR LA NON VERIF. DE CRITERES PHYSIQUES
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORST
! OUT CODERE : CHAM_ELEM CODE RETOUR ERREUR INTEGRATION LDC
!
! ----------------------------------------------------------------------
!
    logical :: reasma, lcamor
    logical :: ldyna, lamor, lsuiv, lcrigi, lcfint, larigi
    character(len=16) :: metcor, metpre
    character(len=16) :: optrig, optamo
    character(len=19) :: vefint, cnfint
    character(len=24) :: modele
    logical :: renume
    integer :: ifm, niv
    integer :: nbmatr
    character(len=6) :: ltypma(20)
    character(len=16) :: loptme(20), loptma(20)
    logical :: lassme(20), lcalme(20)
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ...... CALCUL MATRICE'
    endif
!
! --- INTIIALISATIONS
!
    call nmcmat('INIT', '      ', ' ', ' ', .false.,&
                .false., nbmatr, ltypma, loptme, loptma,&
                lcalme, lassme)
    modele = modelz
    faccvg = -1
    ldccvg = -1
    renume = .false.
    lcamor = .false.
    call nmchex(veelem, 'VEELEM', 'CNFINT', vefint)
    call nmchex(veasse, 'VEASSE', 'CNFINT', cnfint)
!
! --- FONCTIONNALITES ACTIVEES
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lamor = ndynlo(sddyna,'MAT_AMORT')
    lsuiv = isfonc(fonact,'FORCE_SUIVEUSE')
!
! --- CHOIX DE REASSEMBLAGE DE LA MATRICE GLOBALE
!
    call nmchrm('CORRECTION', parmet, method, fonact, sddisc,&
                sddyna, numins, iterat, defico, metpre,&
                metcor, reasma)
!
! --- CHOIX DE REASSEMBLAGE DE L'AMORTISSEMENT
!
    if (lamor) then
        call nmchra(sddyna, optamo, lcamor)
    endif
!
! --- RE-CREATION DU NUME_DDL OU PAS
!
    call nmrenu(modelz, fonact, numedd, lischa, solveu,&
                resoco, renume)
!
! --- OPTION DE CALCUL POUR MERIMO
!
    call nmchoi('CORRECTION', sddyna, numins, fonact, metpre,&
                metcor, reasma, lcamor, optrig, lcrigi,&
                larigi, lcfint)
!
! --- CALCUL DES FORCES INTERNES
!
    if (lcfint) then
        call nmfint(modele, mate, carele, comref, compor,&
                    carcri, fonact, iterat, sddyna, sdstat,&
                    sdtime, valinc, solalg, ldccvg, codere,&
                    vefint)
    endif
!
! --- ERREUR SANS POSSIBILITE DE CONTINUER
!
    if (ldccvg .eq. 1) goto 9999
!
! --- ASSEMBLAGE DES FORCES INTERNES
!
    if (lcfint) then
        lcfint = .false.
        call nmaint(numedd, fonact, defico, veasse, vefint,&
                    cnfint, sdnume)
    endif
!
! --- CALCUL DES MATR_ELEM CONTACT/XFEM_CONTACT
!
    call nmchcc(fonact, nbmatr, ltypma, loptme, loptma,&
                lassme, lcalme)
!
! --- ASSEMBLAGE DES MATR-ELEM DE RIGIDITE
!
    if (larigi) then
        call nmcmat('AJOU', 'MERIGI', optrig, ' ', .false.,&
                    larigi, nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
    endif
!
! --- CALCUL DES MATR-ELEM D'AMORTISSEMENT DE RAYLEIGH A CALCULER
! --- NECESSAIRE SI MATR_ELEM RIGIDITE CHANGE !
!
    if (lcamor) then
        call nmcmat('AJOU', 'MEAMOR', optamo, ' ', .true.,&
                    .true., nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
    endif
!
! --- CALCUL DES MATR-ELEM DES CHARGEMENTS SUIVEURS
!
    if (lsuiv) then
        call nmcmat('AJOU', 'MESUIV', ' ', ' ', .true.,&
                    .false., nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
    endif
!
! --- RE-CREATION MATRICE MASSE SI NECESSAIRE (NOUVEAU NUME_DDL)
!
    if (renume) then
        if (ldyna) then
            call nmcmat('AJOU', 'MEMASS', ' ', ' ', .false.,&
                        .true., nbmatr, ltypma, loptme, loptma,&
                        lcalme, lassme)
        endif
        if (.not.reasma) then
            call assert(.false.)
        endif
    endif
!
! --- CALCUL ET ASSEMBLAGE DES MATR_ELEM DE LA LISTE
!
    if (nbmatr .gt. 0) then
        call nmxmat(modelz, mate, carele, compor, carcri,&
                    sddisc, sddyna, fonact, numins, iterat,&
                    valinc, solalg, lischa, comref, defico,&
                    resoco, solveu, numedd, numfix, sdstat,&
                    sdtime, nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme, lcfint, meelem, measse,&
                    veelem, ldccvg, codere)
    endif
!
! --- ERREUR SANS POSSIBILITE DE CONTINUER
!
    if (ldccvg .eq. 1) goto 9999
!
! --- CALCUL DE LA MATRICE ASSEMBLEE GLOBALE
!
    if (reasma) then
        call nmmatr('CORRECTION', fonact, lischa, solveu, numedd,&
                    sddyna, numins, defico, resoco, meelem,&
                    measse, matass)
    endif
!
! --- AFFICHAGE
!
    if (reasma) then
        call nmimck(sdimpr, 'MATR_ASSE', metcor, .true.)
    else
        call nmimck(sdimpr, 'MATR_ASSE', metcor, .false.)
    endif
!
! --- FACTORISATION DE LA MATRICE ASSEMBLEE GLOBALE
!
    if (reasma) then
        call nmtime(sdtime, 'INI', 'FACTOR')
        call nmtime(sdtime, 'RUN', 'FACTOR')
        call preres(solveu, 'V', faccvg, maprec, matass,&
                    ibid, -9999)
        call nmtime(sdtime, 'END', 'FACTOR')
        call nmrinc(sdstat, 'FACTOR')
    endif
!
9999  continue
!
end subroutine
