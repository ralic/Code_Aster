subroutine digouj(option, compor, nno, nbt, neq,&
                  nc, icodma, dul, sim, varim,&
                  pgl, klv, klc, varip, fono,&
                  sip, nomte)
! ----------------------------------------------------------------------
    implicit none
    include 'asterfort/mavec.h'
    include 'asterfort/pmavec.h'
    include 'asterfort/rcfonc.h'
    include 'asterfort/rctrac.h'
    include 'asterfort/rctype.h'
    include 'asterfort/tecach.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/ut2vlg.h'
    include 'asterfort/utpvlg.h'
    include 'asterfort/vecma.h'
    integer :: nbt, neq, icodma, nc
    real(kind=8) :: dul(neq), sim(neq), sip(neq), varim(*)
    real(kind=8) :: pgl(3, 3), klv(nbt), varip(*), fono(neq), klc(neq, neq)
    character(len=16) :: option, compor(*), nomte
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!  COMPORTEMENT DIS_GOUJON : APPLICATION : GOUJ2ECH
!           RELATION DE COMPORTEMENT : ELASTIQUE PARTOUT
!           SAUF SUIVANT Y LOCAL : DIS_GOUJON
!       ELEMENTS MECA_DIS_T_L
!
! IN  : NBT    : NOMBRE DE VALEURS POUR LA DEMI-MATRICE
!       NEQ    : NOMBRE DE DDL DE L'ELEMENT
!       NC     : NOMBRE DE DDL PAR NOEUD = 3 OU 6
!       ICODMA : ADRESSE DU MATERIAU CODE
!       DUL    : INCREMENT DE DEPLACEMENT REPERE LOCAL
!       SIM    : EFFORTS GENERALISES A L'INSTANT PRECEDENT
!       TP     : INSTANT ACTUEL
!       VARIM  : VARIABLE INTERNE A L'INSTANT PRECEDENT
!       PGL    : MATRICE DE PASSAGE REPERE GLOBAL -> LOCAL
!
! VAR : KLV    : MATRICE ELASTIQUE REPERE LOCAL EN ENTREE
!              : MATRICE TANGENTE EN SORTIE
! OUT : VARIP  : VARIABLE INTERNE REACTUALISEE
!       FONI   : FORCES NODALES
!       SIP    : EFFORTS INTERNES
!
    integer :: i, nno, jprolp, jvalep, nbvalp, lgpg, jtab(7)
    real(kind=8) :: seuil
    real(kind=8) :: dfl(6), fl(6)
    real(kind=8) :: nu, dum, rbid, resu, valpap
    character(len=8) :: nompar, type
    character(len=24) :: valk(2)
    logical :: plasti
!
!-----------------------------------------------------------------------
    integer :: iret
    real(kind=8) :: a, airerp, coef, deps, dp, dsidep, dut
    real(kind=8) :: e, rp, rprim, sieleq, sigdv, sigel, sigeps
    real(kind=8) :: sigy
!-----------------------------------------------------------------------
    call tecach('OON', 'PVARIMR', 'L', 7, jtab,&
                iret)
    lgpg = max(jtab(6),1)*jtab(7)
!
    if (nc .ne. 2) then
        valk(1) = nomte
        valk(2) = compor(1)
        call u2mesk('F', 'ELEMENTS_31', 2, valk)
    endif
!
! --- CALCUL ELASTIQUE
!
! --- DEMI-MATRICE KLV TRANSFORMEE EN MATRICE PLEINE KLC
!
    call vecma(klv, nbt, klc, neq)
!
! --- CALCUL DE FL = KLC.DUL (INCREMENT D'EFFORT ELASTIQUE)
!
    call pmavec('ZERO', neq, klc, dul, dfl)
    dut = dul(2+nc)-dul(2)
!
    if ((compor(1)(1:10).ne.'DIS_GOUJ2E')) then
        call u2mesk('F', 'ELEMENTS_32', 1, compor(1))
    endif
!
    call rctype(icodma, 0, nompar, valpap, resu,&
                type)
    call rctrac(icodma, 1, 'SIGM', resu, jprolp,&
                jvalep, nbvalp, e)
    if (compor(1) .eq. 'DIS_GOUJ2E_PLAS') then
        call rcfonc('S', 1, jprolp, jvalep, nbvalp,&
                    sigy, dum, dum, dum, dum,&
                    dum, dum, dum, dum)
        call rcfonc('V', 1, jprolp, jvalep, nbvalp,&
                    rbid, rbid, rbid, varim( 1), rp,&
                    rprim, airerp, rbid, rbid)
        plasti=(varim(2).ge.0.5d0)
    else if (compor(1).eq.'DIS_GOUJ2E_ELAS') then
        sigy=0.d0
        rp=0.d0
        plasti=.false.
    endif
!
    deps=dut
!
    sigel = sim(2) + e*deps
    sieleq = abs(sigel)
    seuil = sieleq - rp
!
    dp=0.d0
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
!
        do 100 i = 1, nc
            sip(i) = -dfl(i) + sim(i)
            sip(i+nc) = dfl(i+nc) + sim(i+nc)
            fl(i) = dfl(i) - sim(i)
            fl(i+nc) = dfl(i+nc) + sim(i+nc)
100      continue
!
        if (compor(1) .eq. 'DIS_GOUJ2E_ELAS') then
            sip(2 ) = sigel
!
        else if (compor(1).eq. 'DIS_GOUJ2E_PLAS') then
            if (seuil .le. 0.d0) then
                varip(2) = 0.d0
                dp = 0.d0
            else
                varip(2) = 1.d0
                nu=0.5d0
                call rcfonc('E', 1, jprolp, jvalep, nbvalp,&
                            rbid, e, nu, varim(1), rp,&
                            rprim, airerp, sieleq, dp)
            endif
            varip(1) = varim(1) + dp
            plasti=(varip(2).ge.0.5d0)
!
            sip(2) = sigel*rp/(rp+e*dp)
            varip(1+lgpg) = varip(1)
            varip(2+lgpg) = varip(2)
        endif
        sip(2+nc) = sip(2)
!
!        FL : EFFORTS GENERALISES AUX NOEUDS 1 ET 2 (REPERE LOCAL)
!            ON CHANGE LE SIGNE DES EFFORTS SUR LE PREMIER NOEUD
!        FONO : FORCES NODALES AUX NOEUDS 1 ET 2 (REPERE GLOBAL)
!
        fl(2) = -sip(2)
        fl(2+nc) = sip(2)
        if (nomte .eq. 'MECA_2D_DIS_T_L') then
            call ut2vlg(nno, nc, pgl, fl, fono)
        else
            call utpvlg(nno, nc, pgl, fl, fono)
        endif
    endif
!
    if (option(1:14) .eq. 'RIGI_MECA_TANG' .or. option(1:9) .eq. 'FULL_MECA') then
!
        if (option(1:14) .eq. 'RIGI_MECA_TANG') then
!         - - OPTION='RIGI_MECA_TANG' => SIGMA(T)
            rp=0.d0
            sigdv = sim(2)
            rp = abs(sigdv)
        else
!         - - OPTION='FULL_MECA' => SIGMA(T+DT)
            sigdv = sip(2)
        endif
!
        a=1.d0
        dsidep=0.d0
        if (compor(1) .eq. 'DIS_GOUJ2E_PLAS') then
            sigeps = 0.d0
            sigeps = sigeps + sigdv*deps
            if (plasti .and. (sigeps.ge.0.d0)) then
                a = 1.d0+e*dp/rp
                coef = - e**2/(e+rprim)/rp**2 *(1.d0 - dp*rprim/rp )/ a
                dsidep = coef*sigdv*sigdv
            endif
        endif
        dsidep = dsidep + e/a
    endif
!
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
        if (nc .eq. 2) then
!            KLV(3)  =  DSIDEP
!            KLV(10)  = DSIDEP
            klc(2,2) = dsidep
            klc(4,4) = dsidep
            klc(2,4) = -dsidep
            klc(4,2) = -dsidep
        else if (nc.eq.3) then
!            KLV(3)  =  DSIDEP
!            KLV(15)  = DSIDEP
            klc(2,2) = dsidep
            klc(5,5) = dsidep
            klc(2,5) = -dsidep
            klc(5,2) = -dsidep
        endif
        call mavec(klc, neq, klv, nbt)
    endif
end subroutine
