subroutine nminit(result, modele, numedd, numfix, mate,&
                  compor, carele, parmet, lischa, maprec,&
                  solveu, carcri, numins, sdstat, sddisc,&
                  sdnume, defico, sdcrit, comref, fonact,&
                  parcon, parcri, method, lisch2, noma,&
                  sdpilo, sddyna, sdimpr, sdsuiv, sdobse,&
                  sdtime, sderro, sdpost, sdieto, sdener,&
                  sdconv, sdcriq, deficu, resocu, resoco,&
                  valinc, solalg, measse, veelem, meelem,&
                  veasse, codere)
!
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
! TOLE CRP_21
!
    implicit none
    include 'asterfort/accel0.h'
    include 'asterfort/cetule.h'
    include 'asterfort/cfmxsd.h'
    include 'asterfort/cucrsd.h'
    include 'asterfort/diinit.h'
    include 'asterfort/diinst.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/isfonc.h'
    include 'asterfort/liscpy.h'
    include 'asterfort/ndynlo.h'
    include 'asterfort/nmchap.h'
    include 'asterfort/nmchar.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/nmcrcg.h'
    include 'asterfort/nmcrch.h'
    include 'asterfort/nmcrcv.h'
    include 'asterfort/nmcrst.h'
    include 'asterfort/nmcrti.h'
    include 'asterfort/nmdoco.h'
    include 'asterfort/nmdoct.h'
    include 'asterfort/nmdoet.h'
    include 'asterfort/nmdopi.h'
    include 'asterfort/nmetcr.h'
    include 'asterfort/nmexso.h'
    include 'asterfort/nmfonc.h'
    include 'asterfort/nmihht.h'
    include 'asterfort/nminim.h'
    include 'asterfort/nminma.h'
    include 'asterfort/nminmc.h'
    include 'asterfort/nminvc.h'
    include 'asterfort/nmlssv.h'
    include 'asterfort/nmnoli.h'
    include 'asterfort/nmnume.h'
    include 'asterfort/nmobsv.h'
    include 'asterfort/nmpro2.h'
    include 'asterfort/nmrini.h'
    include 'asterfort/nmvcle.h'
    include 'asterfort/nmvcre.h'
    integer :: fonact(*)
    real(kind=8) :: parcon(*), parcri(*), parmet(*)
    character(len=16) :: method(*)
    integer :: numins
    character(len=8) :: result, noma
    character(len=19) :: solveu, sdnume, sddisc, sdcrit, sdpilo, sdobse, sdener
    character(len=19) :: sdpost
    character(len=19) :: lischa, lisch2, sddyna
    character(len=19) :: maprec
    character(len=24) :: modele, compor, numedd, numfix
    character(len=24) :: defico, resoco
    character(len=24) :: carcri
    character(len=24) :: mate, carele, codere
    character(len=19) :: veelem(*), meelem(*)
    character(len=19) :: veasse(*), measse(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=24) :: sdimpr, sdtime, sderro, sdieto, sdstat, sdconv
    character(len=24) :: deficu, resocu, sdsuiv, sdcriq
    character(len=24) :: comref
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! INITIALISATIONS
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM DE LA SD RESULTAT
! IN  SDNUME : NOM DE LA SD NUMEROTATION
! OUT LISCH2 : NOM DE LA SD INFO CHARGE POUR STOCKAGE DANS LA SD
!              RESULTAT
! OUT FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! OUT NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! OUT NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
!
! ----------------------------------------------------------------------
!
    integer :: iret, ibid
    real(kind=8) :: r8bid3(3)
    real(kind=8) :: instin
    character(len=19) :: commoi
    character(len=2) :: codret
    logical :: lacc0, lpilo, lmpas, lsstf, lerrt, lreli, lviss
    logical :: lcont, lunil
    integer :: ifm, niv
    character(len=19) :: ligrcf, ligrxf
    character(len=8) :: nomo
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> INITIALISATION DU CALCUL'
    endif
!
! --- INITIALISATIONS
!
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                noma, iret)
    nomo = modele(1:8)
    lacc0 = .false.
    lunil = .false.
    lcont = .false.
!
! --- CREATION DE LA STRUCTURE DE DONNEE GESTION DU TEMPS
!
    call nmcrti(sdtime)
!
! --- CREATION DE LA STRUCTURE DE DONNEE STATISTIQUES
!
    call nmcrst(sdstat)
!
! --- SAISIE ET VERIFICATION DE LA COHERENCE DU CHARGEMENT CONTACT
!
    call nmdoct(lischa, defico, deficu, lcont, lunil,&
                ligrcf, ligrxf)
!
! --- CREATION DE LA NUMEROTATION ET PROFIL DE LA MATRICE
!
    call nmnume(modele, result, lischa, lcont, defico,&
                compor, solveu, numedd, sdnume)
!
! --- CREATION DE VARIABLES "CHAPEAU" POUR STOCKER LES NOMS
!
    call nmchap(valinc, solalg, meelem, veelem, veasse,&
                measse)
!
! --- FONCTIONNALITES ACTIVEES
!
    call nmfonc(parcri, parmet, method, solveu, modele,&
                defico, lischa, lcont, lunil, sdnume,&
                sddyna, sdcriq, mate, compor, result,&
                fonact)
    lpilo = isfonc(fonact,'PILOTAGE' )
    lmpas = ndynlo(sddyna,'MULTI_PAS' )
    lsstf = isfonc(fonact,'SOUS_STRUC')
    lerrt = isfonc(fonact,'ERRE_TEMPS_THM')
    lreli = isfonc(fonact,'RECH_LINE' )
    lviss = ndynlo(sddyna,'VECT_ISS' )
!
! --- CREATION DE LA STRUCTURE DE DONNEE RESULTAT DU CONTACT
!
    if (lcont) then
        call cfmxsd(noma, nomo, numedd, fonact, sddyna,&
                    defico, resoco, ligrcf, ligrxf)
    endif
!
! --- CREATION DE LA STRUCTURE DE LIAISON_UNILATERALE
!
    if (lunil) then
        call cucrsd(noma, numedd, deficu, resocu)
    endif
!
! --- CREATION DES VECTEURS D'INCONNUS
!
    call nmcrch(numedd, fonact, sddyna, defico, valinc,&
                solalg, veasse)
!
! --- CONSTRUCTION DU CHAM_NO ASSOCIE AU PILOTAGE
!
    if (lpilo) then
        call nmdopi(modele, numedd, method, lreli, sdpilo)
    endif
!
! --- DUPLICATION NUME_DDL POUR CREER UN DUME_DDL FIXE
!
    call nmpro2(fonact, numedd, numfix)
!
! --- CONSTRUCTION DU CHAM_ELEM_S ASSOCIE AU COMPORTEMENT
!
    call nmdoco(modele, carele, compor)
!
! --- CREATION DE LA SD IN ET OUT
!
    call nmetcr(modele, compor, fonact, sddyna, sdpost,&
                defico, resoco, sdieto, carele)
!
! --- LECTURE ETAT_INIT
!
    call nmdoet(modele, compor, fonact, numedd, sdpilo,&
                sddyna, sdcriq, sdieto, solalg, lacc0,&
                instin)
!
! --- CREATION SD DISCRETISATION, ARCHIVAGE ET OBSERVATION
!
    call diinit(noma, nomo, result, mate, carele,&
                fonact, sddyna, parcri, instin, sdieto,&
                solveu, defico, sddisc, sdobse, sdsuiv)
!
! --- CREATION DU CHAMP DES VARIABLES DE COMMANDE DE REFERENCE
!
    call nmvcre(modele, mate, carele, comref)
!
! --- PRE-CALCUL DES MATR_ELEM CONSTANTES AU COURS DU CALCUL
!
    call nminmc(fonact, lischa, sddyna, modele, compor,&
                solveu, numedd, numfix, defico, resoco,&
                carcri, solalg, valinc, mate, carele,&
                sddisc, sdstat, sdtime, comref, meelem,&
                measse, veelem, codere)
!
! --- INSTANT INITIAL
!
    numins = 0
    instin = diinst(sddisc,numins)
!
! --- EXTRACTION VARIABLES DE COMMANDES AU TEMPS T-
!
    call nmchex(valinc, 'VALINC', 'COMMOI', commoi)
    call nmvcle(modele, mate, carele, lischa, instin,&
                commoi, codret)
!
! --- CALCUL ET ASSEMBLAGE DES VECT_ELEM CONSTANTS AU COURS DU CALCUL
!
    call nminvc(modele, mate, carele, compor, carcri,&
                sdtime, sddisc, sddyna, valinc, solalg,&
                lischa, comref, resoco, resocu, numedd,&
                fonact, parcon, veelem, veasse, measse)
!
! --- CREATION DE LA SD POUR ARCHIVAGE DES INFORMATIONS DE CONVERGENCE
!
    call nmcrcv(sdcrit)
!
! --- INITIALISATION CALCUL PAR SOUS-STRUCTURATION
!
    if (lsstf) then
        call nmlssv('INIT', lischa, ibid)
    endif
!
! --- CREATION DE LA SD EXCIT_SOL
!
    if (lviss) call nmexso(noma, result, sddyna, numedd)
!
! --- CALCUL DE L'ACCELERATION INITIALE
!
    if (lacc0) then
        call nmchar('ACCI', ' ', modele, numedd, mate,&
                    carele, compor, lischa, carcri, numins,&
                    sdtime, sddisc, parcon, fonact, resoco,&
                    resocu, comref, valinc, solalg, veelem,&
                    measse, veasse, sddyna)
        call accel0(modele, numedd, numfix, fonact, lischa,&
                    defico, resoco, maprec, solveu, valinc,&
                    sddyna, sdstat, sdtime, meelem, measse,&
                    veelem, veasse, solalg)
    endif
!
! --- CREATION DE LA SD CONVERGENCE
!
    call nmcrcg(fonact, sdconv)
!
! --- INITIALISATION DE LA SD AFFICHAGE
!
    call nminim(sdsuiv, sdimpr)
!
! --- PRE-CALCUL DES MATR_ASSE CONSTANTES AU COURS DU CALCUL
!
    call nminma(fonact, lischa, sddyna, solveu, numedd,&
                numfix, meelem, measse)
!
! --- OBSERVATION INITIALE
!
    call nmobsv(noma, sddisc, sdieto, sdobse, numins)
!
! --- CREATION DE LA SD EVOL_NOLI
!
    call nmnoli(result, sddisc, sderro, carcri, sdimpr,&
                sdcrit, fonact, sddyna, sdpost, modele,&
                mate, carele, lisch2, sdpilo, sdtime,&
                sdener, sdieto, sdcriq)
!
!NS   ICI ON UTILISE LISCPY A LA PLACE DE COPISD POUR
!NS   RESPECTER L'ESPRIT DE COPISD QUI NE SERT QU'A
!NS   RECOPIER DE SD SANS FILTRER
!
    call liscpy(lischa, lisch2, 'G')
!
! --- CREATION DE LA TABLE DES GRANDEURS
!
    if (lerrt) then
        call cetule(modele, r8bid3, iret)
    endif
!
! --- CALCUL DU SECOND MEMBRE INITIAL POUR MULTI-PAS
!
    if (lmpas) then
        call nmihht(modele, numedd, mate, compor, carele,&
                    lischa, carcri, comref, fonact, sdstat,&
                    sddyna, sdtime, sdnume, defico, resoco,&
                    resocu, valinc, sddisc, parcon, solalg,&
                    veasse)
    endif
!
! --- INITIALISATIONS TIMERS ET STATISTIQUES
!
    call nmrini(sdtime, sdstat, 'T')
!
end subroutine
