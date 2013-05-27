subroutine dltlec(result, modele, numedd, materi, mate,&
                  carael, carele, imat, masse, rigid,&
                  amort, lamort, nchar, nveca, lischa,&
                  charge, infoch, fomult, iaadve, ialifo,&
                  nondp, iondp, solveu, iinteg, t0,&
                  nume, baseno, numrep)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ----------------------------------------------------------------------
!
!       DYNAMIQUE LINEAIRE TRANSITOIRE - LECTURE DES DONNEES
!       -         -        -             ---
!
! ----------------------------------------------------------------------
!
!      OUT RESULT : NOM UTILISATEUR DU RESULTAT DE STAT_NON_LINE
!      OUT MODELE : NOM DU MODELE
!      OUT NUMEDD : NUME_DDL DE LA MATR_ASSE RIGID
!      OUT MATERI : NOM DU CHAMP DE MATERIAU
!      OUT MATE   : NOM DU CHAMP DE MATERIAU CODE
!      OUT CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
!      OUT MASSE  : MATRICE DE MASSE
!      OUT RIGID  : MATRICE DE RIGIDITE
!      OUT AMORT  : MATRICE D'AMORTISSEMENT
!      OUT LAMORT : LOGIQUE INDIQUANT SI IL Y A AMORTISSEMENT
!      OUT IMAT   : TABLEAU D'ADRESSES POUR LES MATRICES
!      OUT NCHAR  : NOMBRE D'OCCURENCES DU MOT CLE CHARGE
!      OUT NVECA  : NOMBRE D'OCCURENCES DU MOT CLE VECT_ASSE
!      OUT LISCHA : INFO SUR LES CHARGES
!      OUT CHARGE : LISTE DES CHARGES
!      OUT INFOCH : INFO SUR LES CHARGES
!      OUT FOMULT : LISTE DES FONC_MULT ASSOCIES A DES CHARGES
!      OUT IAADVE : ADRESSE
!      OUT IAADVE : ADRESSE
!      OUT NONDP  : NOMBRE D'ONDES PLANES
!      OUT IONDP  : ADRESSE
!      OUT SOLVEU : NOM DU SOLVEUR
!      OUT IINTEG : TYPE D'INTEGRATION
!                   1 : NEWMARK
!                   2 : WILSON
!                   3 : DIFF_CENTRE
!                   4 : ADAPT
!      OUT T0     : INSTANT INITIAL
!      OUT NUME   : NUMERO D'ORDRE DE REPRISE
!      IN  BASENO : BASE DU NOM DES STRUCTURES
!      OUT NUMREP : NUMERO DE REUSE POUR LA TABLE PARA_CALC
! ----------------------------------------------------------------------
!
    implicit none
!
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/chpver.h'
    include 'asterfort/codent.h'
    include 'asterfort/cresol.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/dltp0.h'
    include 'asterfort/focste.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mtdscr.h'
    include 'asterfort/nmarnr.h'
    include 'asterfort/nmcrpc.h'
    include 'asterfort/nmdome.h'
    include 'asterfort/rcmfmc.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: imat(3)
    integer :: nveca, nchar
    integer :: iaadve, ialifo, iondp, iener
    integer :: iinteg, nondp
    integer :: nume, numrep
!
    real(kind=8) :: t0
!
    logical :: lamort
!
    character(len=8) :: result, baseno
    character(len=8) :: masse, rigid, amort
    character(len=8) :: materi, carael
    character(len=19) :: lischa, solveu
    character(len=24) :: modele, numedd, mate, carele
    character(len=24) :: charge, infoch, fomult
!
!
!
    integer :: niv, ifm
    integer :: nr, nm, na, nvect, ivec, ie, n1
    integer :: iaux, ibid
    integer :: indic, nond, jinf, ialich, ich
!
    real(kind=8) :: rval
!
    character(len=8) :: k8b
    character(len=8) :: blan8
    character(len=16) :: method
    character(len=16) :: k16bid, nomcmd
    character(len=19) :: channo
    integer :: iarg
!
!     -----------------------------------------------------------------
!
!====
! 1. PREALABLES
!====
!
    modele = ' '
    blan8 = ' '
!
    lamort = .true.
    amort = ' '
!
    call infniv(ifm, niv)
!
!====
! 2. LES DONNEES DU CALCUL
!====
!
! 2.1. ==> LE CONCEPT RESULTAT CREE PAR LA COMMANDE
!
    call getres(result, k16bid, nomcmd)
!
! 2.3. ==> CALCUL DES ENERGIES
!
    call wkvect(baseno//'.ENER      .VALE', 'V V R', 6, iener)
!
! 2.4. ==> --- LES MATRICES ---
    call getvid(' ', 'MATR_RIGI', 0, iarg, 1,&
                rigid, nr)
    call getvid(' ', 'MATR_MASS', 0, iarg, 1,&
                masse, nm)
    call getvid(' ', 'MATR_AMOR', 0, iarg, 1,&
                amort, na)
    if (na .le. 0) then
        write(ifm,*)'PAS DE MATRICE D''AMORTISSEMENT'
        lamort = .false.
    endif
    call mtdscr(rigid)
    call jeveuo(rigid//'           .&INT', 'E', imat(1))
    call mtdscr(masse)
    call jeveuo(masse//'           .&INT', 'E', imat(2))
    if (lamort) then
        call mtdscr(amort)
        call jeveuo(amort//'           .&INT', 'E', imat(3))
    endif
!
!====
! 3. LE CHARGEMENT
!====
!
! 3.1. ==> DECODAGE DU CHARGEMENT
!
    call getfac('EXCIT', nvect)
!
    if (nvect .gt. 0) then
!
! 3.1.1. ==> DECODAGE DU CHARGEMENT
!
        nveca = 0
        nchar = 0
        do 311 , ivec = 1 , nvect
        call getvid('EXCIT', 'VECT_ASSE', ivec, iarg, 1,&
                    channo, iaux)
        if (iaux .eq. 1) then
            nveca = nveca + 1
        endif
        call getvid('EXCIT', 'CHARGE', ivec, iarg, 1,&
                    channo, iaux)
        if (iaux .eq. 1) then
            nchar = nchar + 1
        endif
311      continue
!
! 3.1.2. ==> LISTE DE VECT_ASSE DECRIVANT LE CHARGEMENT
!
        if (nveca .ne. 0) then
!
            call wkvect(baseno//'.LIFONCT', 'V V K24', nveca, ialifo)
            call wkvect(baseno//'.ADVECASS', 'V V I  ', nveca, iaadve)
!
            indic = 0
            do 312 ivec = 1, nveca
                indic = indic + 1
3121              continue
                call getvid('EXCIT', 'VECT_ASSE', indic, iarg, 1,&
                            channo, iaux)
                if (iaux .eq. 0) then
                    indic = indic + 1
                    goto 3121
                endif
                call chpver('F', channo, 'NOEU', 'DEPL_R', ibid)
                call jeveuo(channo//'.VALE', 'L', zi(iaadve+ivec-1))
                call getvid('EXCIT', 'FONC_MULT', indic, iarg, 1,&
                            zk24( ialifo+ivec-1), iaux)
                if (iaux .eq. 0) then
                    call getvid('EXCIT', 'ACCE', indic, iarg, 1,&
                                zk24( ialifo+ivec-1), iaux)
                    if (iaux .eq. 0) then
                        rval = 1.d0
                        call getvr8('EXCIT', 'COEF_MULT', indic, iarg, 1,&
                                    rval, iaux)
                        zk24(ialifo+ivec-1) = baseno//'.F_'
                        call codent(ivec, 'G', zk24(ialifo+ivec-1)(12: 19))
                        call focste(zk24(ialifo+ivec-1), 'INST', rval, 'V')
                    endif
                endif
312          continue
!
!
        endif
!
! 3.1.3. ==> LISTE DES CHARGES
!
        if (nchar .ne. 0) then
            call getvid(' ', 'MODELE', 0, iarg, 1,&
                        k8b, iaux)
            if (iaux .eq. 0) then
                call u2mess('F', 'ALGORITH9_26')
            endif
            call nmdome(modele, mate, carele, lischa, blan8,&
                        ibid)
            fomult = lischa//'.FCHA'
        endif
!
! 3.1.4. ==> PAS DE CHARGES
!
    else
!
        nveca=0
        nchar=0
!
    endif
!
! 3.2. ==> TEST DE LA PRESENCE DE CHARGES DE TYPE 'ONDE_PLANE'
!
    nondp = 0
    if (nchar .ne. 0) then
        call jeveuo(infoch, 'L', jinf)
        call jeveuo(charge, 'L', ialich)
        do 32 ich = 1, nchar
            if (zi(jinf+nchar+ich) .eq. 6) then
                nondp = nondp + 1
            endif
32      continue
    endif
!
    if (nveca .ne. 0 .and. nchar .ne. 0) then
        if (nchar .ne. nondp) then
            call u2mess('F', 'ALGORITH9_27')
        endif
    endif
!
! 3.3. ==> RECUPERATION DES DONNEES DE CHARGEMENT PAR ONDE PLANE
!
    if (nondp .eq. 0) then
        call wkvect(baseno//'.ONDP', 'V V K8', 1, iondp)
    else
        call wkvect(baseno//'.ONDP', 'V V K8', nondp, iondp)
        nond = 0
        do 33 ich = 1, nchar
            if (zi(jinf+nchar+ich) .eq. 6) then
                nond = nond + 1
                zk8(iondp+nond-1) = zk24(ialich+ich-1)(1:8)
            endif
33      continue
    endif
!
!
!====
! 4. AUTRES DONNEES
!====
!
! 4.1. ==>
!
    call dismoi('F', 'NOM_NUME_DDL', rigid, 'MATR_ASSE', ibid,&
                numedd, ie)
    call dismoi('F', 'NOM_MODELE', rigid, 'MATR_ASSE', ibid,&
                modele, ie)
    call dismoi('F', 'CARA_ELEM', rigid, 'MATR_ASSE', ibid,&
                carael, ie)
    materi = ' '
    call dismoi('F', 'CHAM_MATER', rigid, 'MATR_ASSE', ibid,&
                materi, ie)
    if (materi .ne. ' ') then
        call rcmfmc(materi, mate)
    endif
!
! 4.2. ==> LECTURE DES PARAMETRES DU MOT CLE FACTEUR SOLVEUR ---
!
    call cresol(solveu)
!
! 4.3. ==> TYPE D'INTEGRATION
!
    call getvtx('SCHEMA_TEMPS', 'SCHEMA', 1, iarg, 1,&
                method, n1)
!
    if (method .eq. 'NEWMARK') then
        iinteg = 1
    else
        if (method .eq. 'WILSON') then
            iinteg=2
        else
            if (method .eq. 'DIFF_CENTRE') then
                iinteg=3
            else
                if (method .eq. 'ADAPT_ORDRE2') then
                    iinteg=4
                endif
            endif
        endif
    endif
!
! 4.4. ==> L'INSTANT INITIAL ET SON NUMERO D'ORDRE SI REPRISE
!
    call dltp0(t0, nume)
!
! --- CREATION DE LA TABLE DES PARAMETRES A SAUVEGARDER
!
    call nmcrpc(result)
!
! --- RECUPERATION NUMERO REUSE - TABLE PARA_CALC
!
    call nmarnr(result, 'PARA_CALC', numrep)
!
end subroutine
