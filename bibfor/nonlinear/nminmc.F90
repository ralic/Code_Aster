subroutine nminmc(fonact, lischa, sddyna, modele, compor,&
                  solveu, numedd, numfix, defico, resoco,&
                  carcri, solalg, valinc, mate, carele,&
                  sddisc, sdstat, sdtime, comref, meelem,&
                  measse, veelem, codere)
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
    include 'asterfort/infdbg.h'
    include 'asterfort/isfonc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/ndynlo.h'
    include 'asterfort/nmcmat.h'
    include 'asterfort/nmxmat.h'
    include 'asterfort/u2mess.h'
    integer :: fonact(*)
    character(len=19) :: lischa, sddyna, solveu
    character(len=24) :: numedd, numfix, resoco, defico
    character(len=24) :: modele, compor
    character(len=24) :: carcri
    character(len=24) :: mate, carele
    character(len=19) :: meelem(*), measse(*)
    character(len=19) :: veelem(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=19) :: sddisc
    character(len=24) :: sdtime, sdstat
    character(len=24) :: codere, comref
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! PRE-CALCUL DES MATRICES ELEMENTAIRES CONSTANTES AU COURS DU CALCUL
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  SDDYNA : SD DYNAMIQUE
! IN  COMPOR : CARTE COMPORTEMENT
! IN  MODELE : NOM DU MODELE
! IN  SOLVEU : SOLVEUR
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  COMREF : VARIABLES DE COMMANDE DE REFERENCE
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  LISCHA : LISTE DES CHARGEMENTS
! IN  MATE   : NOM DU CHAMP DE MATERIAU
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR DEPLACEMENTS
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDSTAT : SD STATISTIQUES
! IN  SDTIME : SD TIMER
! OUT MEELEM : MATRICES ELEMENTAIRES
! OUT VEELEM : VECTEURS ELEMENTAIRES
! OUT MEASSE : MATRICES ASSEMBLEES
! OUT CODERE : CHAM_ELEM CODE RETOUR ERREUR INTEGRATION LDC
!
! ----------------------------------------------------------------------
!
    character(len=16) :: opmass, oprigi
    logical :: lmacr, ldyna, lexpl, lbid
    logical :: lamor, lktan, lelas, lvarc, lcfint, lamra
    integer :: ifm, niv
    integer :: numins, iterat, ldccvg
    integer :: nbmatr
    character(len=16) :: optrig, optamo
    character(len=6) :: ltypma(20)
    character(len=16) :: loptme(20), loptma(20)
    logical :: lassme(20), lcalme(20)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> PRECALCUL DES MATR_ELEM CONSTANTES'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    lmacr = isfonc(fonact,'MACR_ELEM_STAT')
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lexpl = ndynlo(sddyna,'EXPLICITE')
    lvarc = isfonc(fonact,'EXI_VARC' )
    lamor = ndynlo(sddyna,'MAT_AMORT')
    lktan = ndynlo(sddyna,'RAYLEIGH_KTAN')
    lamra = ndynlo(sddyna,'AMOR_RAYLEIGH')
!
! --- INITIALISATIONS
!
    lelas = .false.
    lcfint = .false.
    ldccvg = -1
    if (lamra .and. .not.lktan) then
        lelas = .true.
    endif
!
    call nmcmat('INIT', ' ', ' ', ' ', lbid,&
                lbid, nbmatr, ltypma, loptme, loptma,&
                lcalme, lassme)
!
! --- INSTANT INITIAL
!
    numins = 1
!
! --- MATRICE DE RIGIDITE ASSOCIEE AUX LAGRANGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... MATR_ELEM DE'//&
     &                ' RIGIDITE ASSOCIEE AUX LAGRANGE'
    endif
    call nmcmat('AJOU', 'MEDIRI', ' ', ' ', .true.,&
                .false., nbmatr, ltypma, loptme, loptma,&
                lcalme, lassme)
!
! --- MATRICE DE MASSE
!
    if (ldyna) then
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ... MATR_ELEM DE MASSE'
        endif
        if (lexpl) then
            if (ndynlo(sddyna,'MASS_DIAG')) then
                opmass = 'MASS_MECA_EXPLI'
            else
                opmass = 'MASS_MECA'
            endif
        else
            opmass = 'MASS_MECA'
        endif
        call nmcmat('AJOU', 'MEMASS', opmass, ' ', .true.,&
                    .false., nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
!
    endif
!
! --- MATRICES DES MACRO-ELEMENTS
! --- ON DOIT ASSEMBLER _AVANT_ ACCEL0
!
    if (lmacr) then
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ... MATR_ELEM DES MACRO_ELEMENTS'
        endif
        oprigi = 'RIGI_MECA'
        call nmcmat('AJOU', 'MESSTR', oprigi, ' ', .true.,&
                    .true., nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
    endif
!
! --- AJOUT DE LA MATRICE ELASTIQUE DANS LA LISTE
!
    if (lelas) then
        optrig = 'RIGI_MECA'
! ----- PARAMETRE INUTILE POUR OPTION RIGI_MECA
        iterat = 0
        call nmcmat('AJOU', 'MERIGI', optrig, ' ', .true.,&
                    .false., nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
        if (lvarc) then
            call u2mess('A', 'MECANONLINE3_2')
        endif
    endif
!
! --- AJOUT DE LA MATRICE AMORTISSEMENT DANS LA LISTE
!
    if (lamor .and. .not.lktan) then
        optamo = 'AMOR_MECA'
        call nmcmat('AJOU', 'MEAMOR', optamo, ' ', .true.,&
                    .false., nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
!
    endif
!
! --- CALCUL ET ASSEMBLAGE DES MATR_ELEM DE LA LISTE
!
    if (nbmatr .gt. 0) then
        call nmxmat(modele, mate, carele, compor, carcri,&
                    sddisc, sddyna, fonact, numins, iterat,&
                    valinc, solalg, lischa, comref, defico,&
                    resoco, solveu, numedd, numfix, sdstat,&
                    sdtime, nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme, lcfint, meelem, measse,&
                    veelem, ldccvg, codere)
        if (ldccvg .gt. 0) then
            call u2mess('F', 'MECANONLINE_1')
        endif
    endif
!
    call jedema()
end subroutine
