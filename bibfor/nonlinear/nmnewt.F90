subroutine nmnewt(noma, modele, numins, numedd, numfix,&
                  mate, carele, comref, compor, lischa,&
                  method, fonact, carcri, parcon, conv,&
                  parmet, parcri, sdstat, sdieto, sdtime,&
                  sderro, sdimpr, sdnume, sddyna, sddisc,&
                  sdcrit, sdsuiv, sdpilo, sdconv, solveu,&
                  maprec, matass, valinc, solalg, meelem,&
                  measse, veelem, veasse, defico, resoco,&
                  deficu, resocu, eta, nbiter)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! TOLE CRP_21
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'asterfort/isfonc.h'
    include 'asterfort/nmactf.h'
    include 'asterfort/nmactn.h'
    include 'asterfort/nmaffi.h'
    include 'asterfort/nmchar.h'
    include 'asterfort/nmconv.h'
    include 'asterfort/nmcrel.h'
    include 'asterfort/nmcvgf.h'
    include 'asterfort/nmcvgn.h'
    include 'asterfort/nmdcin.h'
    include 'asterfort/nmdepl.h'
    include 'asterfort/nmdesc.h'
    include 'asterfort/nmeceb.h'
    include 'asterfort/nmeraz.h'
    include 'asterfort/nmevdt.h'
    include 'asterfort/nmevr0.h'
    include 'asterfort/nmfcon.h'
    include 'asterfort/nmfcor.h'
    include 'asterfort/nmible.h'
    include 'asterfort/nmimci.h'
    include 'asterfort/nmimcr.h'
    include 'asterfort/nmimr0.h'
    include 'asterfort/nmleeb.h'
    include 'asterfort/nmnble.h'
    include 'asterfort/nmnpas.h'
    include 'asterfort/nmpred.h'
    include 'asterfort/nmrinc.h'
    include 'asterfort/nmstat.h'
    include 'asterfort/nmsuiv.h'
    include 'asterfort/nmtble.h'
    include 'asterfort/nmtime.h'
    include 'asterfort/nmtimr.h'
    integer :: numins
    integer :: fonact(*)
    character(len=16) :: method(*)
    real(kind=8) :: parmet(*), parcri(*), parcon(*), conv(*)
    character(len=24) :: carcri
    character(len=24) :: sdtime, sderro, sdimpr, sdieto, sdstat, sdconv, sdsuiv
    character(len=19) :: sdnume, sddyna, sddisc, sdcrit
    character(len=19) :: sdpilo
    character(len=19) :: valinc(*), solalg(*)
    character(len=19) :: meelem(*), veelem(*)
    character(len=19) :: measse(*), veasse(*)
    character(len=19) :: lischa
    character(len=19) :: solveu, maprec, matass
    character(len=24) :: modele, numedd, numfix
    character(len=24) :: comref, compor
    character(len=24) :: mate, carele
    character(len=24) :: defico, resoco, deficu, resocu
    real(kind=8) :: eta
    integer :: nbiter
    character(len=8) :: noma
!
! ----------------------------------------------------------------------
!
! OPERATEUR NON-LINEAIRE MECANIQUE
!
! ALGORITHME DE NEWTON (STATIQUE ET DYNAMIQUE)
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
! IN  LISCHA : LISTE DES CHARGES
! IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION
! IN  SOLVEU : SOLVEUR
! IN  SDSTAT : SD STATISTIQUES
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  PARMET : PARAMETRES DES METHODES DE RESOLUTION
! I/O CONV   : INFORMATIONS SUR LA CONVERGENCE DU CALCUL
! IN  PARCRI : CRITERES DE CONVERGENCE
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IN  SDDISC : SD DISC_INST
! IN  SDTIME : SD TIMER
! IN  SDERRO : GESTION DES ERREURS
! IN  SDIETO : SD GESTION IN ET OUT
! IN  SDCONV : SD GESTION DE LA CONVERGENCE
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  DEFICO : SD DEFINITION CONTACT
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  RESOCU : SD RESOLUTION LIAISON_UNILATER
! IN  SDDYNA : SD DYNAMIQUE
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  MAPREC : NOM DE LA MATRICE DE PRECONDITIONNEMENT (GCPC)
! IN  SDNUME : SD NUMEROTATION
! I/O ETA    : PARAMETRE DE PILOTAGE
! OUT NBITER : NOMBRE D'ITERATIONS DE NEWTON
! OUT ETATIN : ETAT DE LA CONVERGENCE DU PAS DE TEMPS
!     0 - CVG  -> LE PAS DE TEMPS A CONVERGE
!     1 - NOOK -> UN EVENEMENT DURANT LE PAS DE TEMPS
!     2 - NCVG -> LE PAS DE TEMPS N'A PAS CONVERGE
!     3 - STOP -> ERREUR FATALE - ARRET DU CALCUL
!
! ----------------------------------------------------------------------
!
    integer :: niveau, iterat
    logical :: lerrit
    logical :: lboucl, lctcd
    character(len=4) :: etnewt, etfixe
    real(kind=8) :: time
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    iterat = 0
    niveau = 0
    nbiter = 0
    lerrit = .false.
!
! --- REMISE A ZERO DES EVENEMENTS INTRINSEQUES
!
    call nmeraz(sderro, 'TOUS')
!
! --- REMISE A ZERO DES EVENEMENTS UTILISATEURS
!
    call nmevr0(sddisc)
!
! --- INITIALISATION AFFECTATION DES COLONNES
!
    call nmimr0(sdimpr, 'NEWT')
!
! --- ACTIVATION BOUCLES CONTACT
!
    lboucl = isfonc(fonact,'BOUCLE_EXTERNE')
    lctcd = isfonc(fonact,'CONT_DISCRET')
!
    if (lboucl) niveau = 3
!
! --- VERIFICATION DECOUPE INITIALE DU PAS DE TEMPS
!
    call nmdcin(sddisc, numins)
!
! --- INITIALISATIONS POUR LE NOUVEAU PAS DE TEMPS
!
    call nmnpas(modele, noma, mate, carele, lischa,&
                fonact, sdimpr, sddisc, sdsuiv, sddyna,&
                sdnume, sdstat, sdtime, numedd, numins,&
                conv, defico, resoco, valinc, solalg,&
                solveu)
!
! --- CALCUL DES CHARGEMENTS CONSTANTS AU COURS DU PAS DE TEMPS
!
    call nmchar('FIXE', ' ', modele, numedd, mate,&
                carele, compor, lischa, carcri, numins,&
                sdtime, sddisc, parcon, fonact, resoco,&
                resocu, comref, valinc, solalg, veelem,&
                measse, veasse, sddyna)
!
! ======================================================================
!     BOUCLE POINTS FIXES
! ======================================================================
!
100  continue
!
    iterat = 0
    nbiter = nbiter + 1
!
! --- GESTION DEBUT DE BOUCLE POINTS FIXES
!
    call nmible(modele, noma, defico, resoco, fonact,&
                numins, niveau, numedd, sdstat, sdtime,&
                sdimpr)
!
! --- CREATION OBJETS POUR CONTACT CONTINU
!
    call nmnble(numins, modele, noma, numedd, sdstat,&
                sdtime, sddyna, sddisc, fonact, defico,&
                resoco, valinc, solalg)
!
! ======================================================================
!     PREDICTION
! ======================================================================
!
    call nmtime(sdtime, 'RUN', 'ITE')
!
! --- PREDICTION D'UNE DIRECTION DE DESCENTE
!
    call nmpred(modele, numedd, numfix, mate, carele,&
                comref, compor, lischa, method, solveu,&
                fonact, parmet, carcri, sdimpr, sdstat,&
                sdtime, sddisc, sdnume, sderro, numins,&
                valinc, solalg, matass, maprec, defico,&
                resoco, resocu, sddyna, meelem, measse,&
                veelem, veasse, lerrit)
!
    if (lerrit) goto 315
!
! ======================================================================
!     BOUCLE SUR LES ITERATIONS DE NEWTON
! ======================================================================
!
300  continue
    if (iterat .ne. 0) call nmtime(sdtime, 'RUN', 'ITE')
!
! --- CALCUL PROPREMENT DIT DE L'INCREMENT DE DEPLACEMENT
! --- EN CORRIGEANT LA (LES) DIRECTIONS DE DESCENTE
! --- SI CONTACT OU PILOTAGE OU RECHERCHE LINEAIRE
!
    call nmdepl(modele, numedd, mate, carele, comref,&
                compor, lischa, fonact, sdstat, parmet,&
                carcri, noma, method, numins, iterat,&
                solveu, matass, sddisc, sddyna, sdnume,&
                sdpilo, sdtime, sderro, defico, resoco,&
                deficu, resocu, valinc, solalg, veelem,&
                veasse, eta, conv, lerrit)
!
    if (lerrit) goto 315
!
! --- CALCUL DES FORCES APRES CORRECTION
!
    call nmfcor(modele, numedd, mate, carele, comref,&
                compor, lischa, fonact, parmet, carcri,&
                method, numins, iterat, sdstat, sdtime,&
                sddisc, sddyna, sdnume, sderro, defico,&
                resoco, resocu, parcon, valinc, solalg,&
                veelem, veasse, meelem, measse, matass,&
                lerrit)
!
    if (lerrit) goto 315
!
! --- SUIVI DE DDL
!
    call nmsuiv(noma, sdieto, sdsuiv, sdimpr)
!
! --- ESTIMATION DE LA CONVERGENCE
!
315  continue
    call nmconv(noma, modele, mate, numedd, sdnume,&
                fonact, sddyna, sdconv, sdimpr, sdstat,&
                sddisc, sdtime, sdcrit, sderro, parmet,&
                comref, matass, solveu, numins, iterat,&
                conv, eta, parcri, defico, resoco,&
                valinc, solalg, measse, veasse)
!
! --- MISE A JOUR DES EFFORTS DE CONTACT
!
    call nmfcon(modele, numedd, mate, fonact, defico,&
                resoco, sdstat, sdtime, valinc, solalg,&
                veelem, veasse)
!
! --- ETAT DE LA CONVERGENCE DE NEWTON
!
    call nmcvgn(sddisc, sderro, valinc, defico, resoco)
    call nmleeb(sderro, 'NEWT', etnewt)
!
! --- AFFICHAGE PENDANT LES ITERATIONS DE NEWTON
!
    call nmimci(sdimpr, 'ITER_NUME', iterat, .true.)
    call nmaffi(fonact, sdconv, sdimpr, sderro, sddisc,&
                'NEWT')
!
    if (etnewt .ne. 'CONT') goto 330
!
! --- ON CONTINUE LES ITERATIONS DE NEWTON : CALCUL DE LA DESCENTE
!
320  continue
!
    call nmdesc(modele, numedd, numfix, mate, carele,&
                comref, compor, lischa, resoco, method,&
                solveu, parmet, carcri, fonact, numins,&
                iterat, sddisc, sdimpr, sdstat, sdtime,&
                sddyna, sdnume, sderro, matass, maprec,&
                defico, valinc, solalg, meelem, measse,&
                veasse, veelem, lerrit)
!
    if (lerrit) goto 315
!
! --- ON CONTINUE NEWTON
!
    iterat = iterat + 1
    nbiter = nbiter + 1
!
! --- CAS DU CONTACT DISCRET
!
    call nmleeb(sderro, 'NEWT', etnewt)
    if (etnewt .eq. 'CTCD') then
        call nmeceb(sderro, 'NEWT', 'CONT')
        call nmtime(sdtime, 'END', 'ITE')
        goto 300
    endif
!
330  continue
!
! --- TEMPS CPU ITERATION DE NEWTON
!
    call nmtime(sdtime, 'END', 'ITE')
    call nmrinc(sdstat, 'ITE')
!
! --- TEMPS PASSE DANS L'ITERATION
!
    call nmtimr(sdtime, 'TEMPS_PHASE', 'N', time)
    call nmimcr(sdimpr, 'ITER_TIME', time, .true.)
!
! --- VERIFICATION DU DECLENCHEMENT DES ERREURS FATALES
!
    call nmevdt(sdtime, sderro, 'ITE')
!
! --- STATISTIQUES SUR ITERATION DE NEWTON
!
    call nmstat('N', fonact, sdstat, sdtime, sdimpr,&
                defico)
!
! --- ON CONTINUE NEWTON ?
!
    call nmleeb(sderro, 'NEWT', etnewt)
    if (etnewt .eq. 'CONT') goto 300
!
! ======================================================================
!     FIN BOUCLE SUR LES ITERATIONS DE NEWTON
! ======================================================================
!
!
!
! --- GESTION DES ACTIONS A LA FIN DE LA BOUCLE DE NEWTON
!
    call nmactn(sdimpr, sddisc, sderro, defico, resoco,&
                solveu, parcri, iterat, numins)
!
! --- ON FAIT DES ITERATIONS SUPPLEMENTAIRES ?
!
    call nmleeb(sderro, 'NEWT', etnewt)
    if (etnewt .eq. 'CONT') then
        call nmtime(sdtime, 'RUN', 'ITE')
        call nmcrel(sderro, 'ITER_MAXI', .false.)
        goto 320
    endif
!
! --- GESTION FIN DE BOUCLE POINTS FIXES
!
    call nmtble(modele, noma, mate, defico, resoco,&
                niveau, fonact, sdimpr, sdstat, sdtime,&
                sddyna, sderro, sdconv, sddisc, valinc,&
                solalg)
!
! --- ETAT DE LA CONVERGENCE POINT FIXE
!
    call nmcvgf(sddisc, sderro, valinc, defico, resoco)
!
! --- GESTION DES ACTIONS A LA FIN D'UNE BOUCLE DE POINT FIXE
!
    call nmactf(sdimpr, sddisc, sderro, defico, resoco,&
                solveu, parcri, iterat, numins)
!
! --- POUR LA CONTINUATION DU POINT FIXE: GLUTE DUE AU CONTACT DISCRET
!
    call nmleeb(sderro, 'FIXE', etfixe)
    if (etfixe .eq. 'CONT') then
        if (lctcd) then
            call nmeceb(sderro, 'NEWT', 'CTCD')
            call nmtime(sdtime, 'RUN', 'ITE')
            goto 320
        else if (lboucl) then
            goto 100
        else
            call nmeceb(sderro, 'FIXE', 'CONV')
        endif
    endif
!
! ======================================================================
!     FIN BOUCLE POINTS FIXES
! ======================================================================
!
end subroutine
