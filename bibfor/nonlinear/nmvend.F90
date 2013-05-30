subroutine nmvend(fami, kpg, ksp, materd, materf,&
                  nmat, dt1, epsm, deps, sigm,&
                  vim, ndim, crit, dammax, etatf,&
                  p, np, beta, nb, iter,&
                  ier)
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
! TOLE CRP_4
! TOLE CRP_7
! TOLE CRP_21
!-----------------------------------------------------------------------
    implicit none
!
    include 'asterc/r8miem.h'
    include 'asterfort/lcdevi.h'
    include 'asterfort/lcnrts.h'
    include 'asterfort/lcprsv.h'
    include 'asterfort/nmfedd.h'
    include 'asterfort/nmfend.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvarc.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utlcal.h'
    include 'asterfort/zerofr.h'
    integer :: kpg, ksp, nmat, np, nb, ier, ndim
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: p(np), beta(nb), vim(*), dt1, crit(*)
    real(kind=8) :: epsm(6), deps(6), sigm(6)
    character(len=*) :: fami
    character(len=7) :: etatf(3)
    character(len=16) :: meth
!-----------------------------------------------------------------------
!     INTEGRATION DE LA LOI DE COMPORTEMENT VISCO PLASTIQUE DE
!     CHABOCHE AVEC ENDOMAGEMENT. INTEGRATION EULER IMPLICITE
!     CAS OU ON SE RAMENE A UNE SEULE EQUATION
!-----------------------------------------------------------------------
!-- ARGUMENTS
!------------
!
! IN   MATE    : PARAMETRE MATERIAU A L'INSTANT T
!      IMATE   : ADRESSE DU MATERIAU CODE
!      NMAT    : DIMENSION DE MATE
!      MATCST  : 'OUI' SI MATERIAU CST ENTRE T- ET T
!                'NAP' SI LE PARAMETRE K_D EST UNE NAPPE
!                'NON' SINON
!      HOOK    : OPERATEUR DE HOOK
!      DT      : INCREMENT DE TEMPS
!      NP      : NOMBRE D'INCONNUES ASSOCIEES AUX VARIABLES D'ETAT
!      NB      : NOMBRE D'INCONNUES ASSOCIEES AUX CONTRAINTES
!      RM      : VARIABLES INTERNES A T-
!      DM      : VARIABLES INTERNES A T-
!      EP      : DEFORMATIONS TOTALES ET THERMIQUE A T ET
!                VISCOPLASTIQUE A T-
! OUT  P       : INCONNUES ASSOCIEES AUX VARIABLES D'ETAT
!      BETA    : INCONNUES ASSOCIEES AUX CONTRAINTES
!      IER     : CODE DE RETOUR D'ERREUR
!                0=OK
!                1=NOOK
!
! INFO P(1)=RPOINT,  P(2)=DFOINT
!-----------------------------------------------------------------------
    integer :: i, ndt, ndi, niter, iret, iret1, iret2, iret3, it2, iter
    integer(kind=8) :: ir
    real(kind=8) :: dammax, prec, precr, val0, devse(6)
!
    real(kind=8) :: e, nu, alphap, alpham, dd, dr
    common /tdim/   ndt  , ndi
    real(kind=8) :: xap, epsef(6)
    real(kind=8) :: seq1md, seqe, troisk, troikm, sigmmo
    real(kind=8) :: tp, tm, tref
    common /fvendo/mu,syvp,kvp,rm,dm,seqe,ad,dt,rd,ir,unsurn,unsurm
    real(kind=8) :: mu, syvp, kvp, seq, ad, dt, unsurn, unsurm, rm, dm, rd, nvp
    real(kind=8) :: em, num, devsig(6), depsmo, coef, sigpmo, df, val1, devsm(6)
    real(kind=8) :: mum
    real(kind=8) :: devep(6), deno, valp1
!
!-----------------------------------------------------------------------
!     1. INITIALISATIONS
!     ===================
    niter = int(crit(1))
    prec = crit(3)
    ier = 0
    dt=dt1
    it2=0
    iter=0
!
    rm=vim(nb+2)
    dm=vim(nb+3)
    e =materf(1,1)
    nu =materf(2,1)
    mu=e/2.d0/(1.d0+nu)
    troisk = e/(1.d0-2.d0*nu)
    em =materd(1,1)
    num =materd(2,1)
    troikm = em/(1.d0-2.d0*num)
    mum=em/2.d0/(1.d0+num)
!
    if (ndim .eq. 2) then
        sigm(5)=0.d0
        sigm(6)=0.d0
        deps(5)=0.d0
        deps(6)=0.d0
    endif
!
    alphap=materf(3,1)
    alpham=materd(3,1)
    nvp = materf(1,2)
    unsurn=1.d0/nvp
    unsurm=materf(2,2)
    kvp = 1.d0/materf(3,2)
    syvp = materf(4,2)
    rd = materf(5,2)
    ad = materf(6,2)
!
    call lcdevi(sigm, devsm)
    call lcdevi(deps, devep)
!
    if (ndim .eq. 2) then
        devsm(5)=0.d0
        devsm(6)=0.d0
        devep(5)=0.d0
        devep(6)=0.d0
    endif
!
    if (dm .ge. 1.d0) dm=dammax
    do 15 i = 1, 6
        epsef(i)=devsm(i)/(1.d0-dm)/2.d0/mum+devep(i)
15  end do
    call lcprsv(2.d0*mu, epsef, devse)
!
! -- TEMPERATURE
!
    call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                ksp, tref, iret1)
    call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                ksp, tm, iret2)
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, tp, iret3)
    iret=iret1+iret2+iret3
    if ((iret.eq.0) .and. ((alphap+alpham).eq.0.d0)) then
        call u2mess('F', 'CALCULEL_31')
    else if (((alphap+alpham).eq.0.d0).or.(iret.ge.1)) then
        coef = 0.d0
    else
        coef = alphap*(tp-tref)- alpham*(tm-tref)
    endif
!
    seqe= lcnrts(devse)
!
    if (seqe .gt. syvp) then
!
!        RESOLUTION DE L'EQUATION EN DR
!
        val0 = nmfend(0.d0)
        if (val0 .gt. 0.d0) then
            ier=21
            goto 9999
        endif
!
!        PRECISION RELATIVE DE RESOLUTION : F(X) < PREC
        precr = prec * abs(val0)
!
!        APPROXIMATION INITIALE  DE LA BORNE SUPERIEURE
        xap = seqe/mu/3.d0
!
30      continue
!        RECHERCHE DE LA BORNE SUPERIEURE
        val1 = nmfend(xap)
        if (abs(val1) .lt. precr) then
            dr=xap
            goto 50
        else if (val1.gt.0.d0) then
!           LA SOLUTION EST DANS L INTERVALLE (0,XAP)
            goto 21
        else
!           LA BORNE SUPERIEURE DOIT VERIFIER F(XAP) >0
!           ICI F(XAP) <0. SI F'(XAP) >0, XAP EST A AUGMENTER
            valp1=nmfedd(xap)
            if (valp1 .gt. 0.d0) then
                xap=xap*10.d0
                it2=it2+1
                if (it2 .gt. niter) then
                    ier=22
                    goto 9999
                endif
                goto 30
            else
!              RECHERCHE DE XAP TEL QUE F(XAP) >0
!              A FAIRE : UNE VRAIE DICHOTOMIE
                do 22 i = 1, niter
                    xap = xap/2.d0
                    if (abs(xap) .lt. r8miem()) then
                        dr=0.d0
                        goto 50
                    endif
                    val1 = nmfend(xap)
                    if (val1 .gt. 0.d0) goto 21
22              continue
                ier=23
                goto 9999
            endif
        endif
!
21      continue
!
!        RECUPERATION DE L'ALGORITHME DE RESOLUTION 1D
        call utlcal('VALE_NOM', meth, crit(6))
!
!        RESOLUTION 1D
        call zerofr(0, meth, nmfend, 0.d0, xap,&
                    precr, niter, dr, ier, iter)
        if (ier .ne. 0) goto 9999
!
50      continue
!
        seq1md=kvp*((dr/dt)**unsurn)*((rm+dr)**unsurm)+syvp
        dd=dt*(seq1md/ad)**rd
        df=dm+dd
!
        if (df .ge. dammax) then
            dd = 0.d0
            df = dammax
            dr=0.d0
            etatf(3)='DAMMAXO'
        endif
!
        seq=(1.d0-df)*seqe-3.d0*mu*dr
        deno=1.d0+3.d0*mu*dr/seq
        do 16 i = 1, 6
            devsig(i)=(1.d0-df)*devse(i)/deno
16      continue
!
    else
!
        dr=0.d0
        dd=0.d0
        seq1md=syvp
        df=dm
        call r8inir(6, 0.d0, devsig, 1)
!
    endif
!
    depsmo = 0.d0
    do 13 i = 1, 3
        depsmo = depsmo + deps(i) -coef
13  end do
    depsmo = depsmo/3.d0
!
    sigmmo = 0.d0
    do 17 i = 1, 3
        sigmmo = sigmmo + sigm(i)
17  end do
    sigmmo = sigmmo /3.d0
    sigpmo=(sigmmo/troikm/(1.d0-dm)+depsmo)*(1.d0-df)*troisk
    do 18 i = 1, 3
        beta(i)=devsig(i)+sigpmo
18  end do
    do 19 i = 4, 6
        beta(i)=devsig(i)
19  end do
!
    p(1)=dr/dt
    p(2)=dd/dt
!
9999  continue
end subroutine
