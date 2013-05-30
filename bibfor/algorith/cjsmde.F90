subroutine cjsmde(mod, crit, mater, nvi, epsd,&
                  deps, sigd, sigf, vind, vinf,&
                  noconv, aredec, stopnc, niter, epscon,&
                  trac)
    implicit none
!       ================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       ----------------------------------------------------------------
!     INTEGRATION PLASTIQUE (MECANISME ISOTROPE SEUL) DE LA LOI CJS
!     IN   MOD      :  MODELISATION
!          CRIT     :  CRITERES DE CONVERGENCE
!          MATER    :  COEFFICIENTS MATERIAU A T+DT
!          NVI      :  NB DE VARIABLES INTERNES
!          EPSD     :  DEFORMATIONS A T
!          DEPS     :  INCREMENTS DE DEFORMATION
!          SIGD     :  CONTRAINTE  A T
!          VIND     :  VARIABLES INTERNES  A T
!          AREDEC   :  ARRET DES DECOUPAGES
!          STOPNC   :  ARRET EN CAS DE NON CONVERGENCE
!     VAR  SIGF     :  CONTRAINTE  A T+DT
!          VINF     :  VARIABLES INTERNES  A T+DT
!          NOCONV   :  PAS DE CONVERGENCE
!          NITER    :  NOMBRE D ITERATIONS A CONVERGENCE
!          EPSCON   :  VALEUR ERR FINALE
!       ----------------------------------------------------------------
!
    include 'asterfort/cjside.h'
    include 'asterfort/cjsjde.h'
    include 'asterfort/cjsncn.h'
    include 'asterfort/cjsncv.h'
    include 'asterfort/cjsnor.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/lcnrvn.h'
    include 'asterfort/lcsovn.h'
    include 'asterfort/mgauss.h'
    integer :: ndt, ndi, nvi, nr, nmod
    parameter ( nmod = 14 )
    integer :: iter, niter, iret
    logical :: noconv, aredec, stopnc, trac
!
    real(kind=8) :: epsd(6), deps(6)
    real(kind=8) :: sigd(6), sigf(6), gd(6)
    real(kind=8) :: vind(*), vinf(*), epscon
    real(kind=8) :: crit(*), mater(14, 2)
    real(kind=8) :: r(nmod), drdy(nmod, nmod)
    real(kind=8) :: ddy(nmod), dy(nmod), yd(nmod), yf(nmod)
    real(kind=8) :: err, err1, err2, signe
    real(kind=8) :: det, pa, qinit
    integer :: umess
    integer :: nitimp
    parameter (nitimp = 200)
!
    integer :: essai, essmax
    parameter (essmax = 10)
!
!    SI ABS(COS_NORMALES) < TOLROT RELAX = RELAX*DECREL
!
    real(kind=8) :: tolrot, decrel
    parameter (tolrot = 0.8d0)
    parameter (decrel = 0.5d0)
!
    real(kind=8) :: relax(essmax+1), rotagd(essmax+1), xf(6), nor1(7), nor2(7)
    real(kind=8) :: erimp(nitimp, 4)
!
    integer :: i, j
!
    logical :: devnu1, devnu2, tra1, tra2
!
    character(len=8) :: mod
!
    common /tdim/   ndt, ndi
    umess = iunifi('MESSAGE')
    noconv = .false.
    trac = .false.
    pa = mater(12,2)
    qinit = mater(13,2)
    trac = .false.
!
!
!
! -> DIMENSION DU PROBLEME NR = NDT(SIG) + 1(R) + NDT(X) + 1(DLAMBD)
!
    nr = 2* ndt + 2
!
! -> MISE A ZERO DES DATAS
!
    do 10 i = 1, nr
        ddy(i) = 0.d0
        dy(i) = 0.d0
        yd(i) = 0.d0
        yf(i) = 0.d0
10  continue
!
    do 15 i = 1, ndt
        gd(i) = 0.d0
15  continue
!
!
! -> INITIALISATION DE YD PAR LES CHAMPS (SIGD, VIND, ZERO)
!
    call lceqvn(ndt, sigd, yd)
    yd(ndt+1) = vind(2)
!
    do 30 i = 1, ndt
        yd(ndt+1+i) = vind(i+2)
30  continue
    yd(2*ndt+2) = 0.d0
!
! -> INITIALISATION : DY : CALCUL DE LA SOLUTION D ESSAI INITIALE EN DY
!    (SOLUTION EXPLICITE)
!
!
    call cjside(mod, mater, epsd, deps, yd,&
                gd, dy)
!
!
!---------------------------------------
! -> BOUCLE SUR LES ITERATIONS DE NEWTON
!---------------------------------------
!
    iter = 0
100  continue
!
    iter = iter + 1
!
! -> INCREMENTATION DE YF = YD + DY
!
    call lcsovn(nr, yd, dy, yf)
!
!
!
!--->   EN CAS D'ENTREE EN TRACTION, ON A
!          1.  LES CONTRAINTES SONT RAMENEES SUR L'AXE HYDROSTATIQUE
!              A DES VALEURS FAIBLES ( EGALES A PA/100.0 SOIT -1 KPA )
!          2.  LES VARIABLES INTERNES N'EVOLUENT PAS
!          3.  ON SORT DIRECTEMENT DE CJSMDE, CAR ON SUPPOSE ALORS
!              ETRE REVENU DANS LE DOMAINE ELASTIQUE
!
!       SINON IL APPARAIT ENSUITE UNE ERREUR DANS LA ROUTINE MGAUSS
!
!
!
    if ((yf(1)+yf(2)+yf(3)) .ge. -qinit) then
!
!
        do 31 i = 1, ndi
            sigf(i) = -qinit/3.d0+pa/100.0d0
31      continue
        do 32 i = ndi+1, ndt
            sigf(i) = 0.d0
32      continue
        call lceqvn(nvi-1, vind, vinf)
!
!
        vinf(nvi) = 0.d0
        trac = .true.
        goto 9999
    endif
!
!
!
! -> CALCUL DU SECOND MEMBRE A T+DT :  -R(DY)
!    CALCUL DE SIGNE(S:DEPSDP)
! ET CALCUL DU JACOBIEN DU SYSTEME A T+DT :  DRDY(DY)
!
    do 50 i = 1, nr
        r(i) = 0.d0
        do 60 j = 1, nr
            drdy(i,j) = 0.d0
60      continue
50  continue
!
!
    call cjsjde(mod, mater, epsd, deps, yd,&
                yf, gd, r, signe, drdy)
!
! -> RESOLUTION DU SYSTEME LINEAIRE : DRDY(DY).DDY = -R(DY)
!
    call lceqvn(nr, r, ddy)
    call mgauss('NFVP', drdy, ddy, nmod, nr,&
                1, det, iret)
!
    relax(1) = 1.d0
!
!   ESTIMATION NORMALE AU POINT YD + DY
!
    do 24 i = 1, ndt
        sigf(i) = yd(i)+dy(i)
        xf(i) = yd(ndt+1+i)+dy(ndt+1+i)
24  continue
    call cjsnor(mater, sigf, xf, nor1, devnu1,&
                tra1)
!
    essai = 0
40  continue
    essai = essai + 1
    if ((.not.devnu1) .and. (.not.tra1)) then
        if (essai .gt. essmax) then
            if (aredec .and. stopnc) then
                call cjsncn('CJSMDE', essmax, ndt, nvi, umess,&
                            relax, rotagd, epsd, deps, sigd,&
                            vind)
            else
                noconv = .true.
                goto 200
            endif
        endif
!
!   ESTIMATION NORMALE AU POINT YD + RELAX*DY
!
        do 25 i = 1, ndt
            sigf(i) = yd(i)+dy(i)+relax(essai)*ddy(i)
            xf(i) = yd(ndt+1+i)+dy(ndt+1+i)+ relax(essai)*ddy(ndt+1+i)
25      continue
        call cjsnor(mater, sigf, xf, nor2, devnu2,&
                    tra2)
!
        rotagd(essai) = 0.d0
        do 26 i = 1, ndt
            rotagd(essai) = rotagd(essai)+nor1(i)*nor2(i)
26      continue
        rotagd(essai) = rotagd(essai)/(nor1(ndt+1)*nor2(ndt+1))
!
        if (abs(rotagd(essai)) .lt. tolrot .and. (.not.devnu2) .and. ( .not.tra2)) then
            relax(essai+1) = relax(essai)*decrel
            goto 40
        endif
    endif
!
!  DANS LES CAS OU DEVNU1 OU DEVNU2 (ON A DETCTE DES DEVIATEURS NULS)
!  OU TRA1 OU TRA2 ( ON A DETECTE DES TRACTIONS) ON ABANDONNE
!  LA RELAXATION SUR LES NORMALES
!
!
    do 42 i = 1, nr
        dy(i) = dy(i)+relax(essai)*ddy(i)
        yf(i) = yd(i)+dy(i)
42  continue
!
! -> VERIFICATION DE LA CONVERGENCE : ERREUR = !!DDY!!/!!DY!! < TOLER
!
    call lcnrvn(nr, ddy, err1)
    call lcnrvn(nr, dy, err2)
    if (err2 .eq. 0.d0) then
        err = err1
    else
        err = err1 / err2
    endif
    if (iter .le. nitimp) then
        erimp(iter,1) = err1
        erimp(iter,2) = err2
        erimp(iter,3) = err
        erimp(iter,4) = relax(essai)
    endif
!
    if (iter .le. int(abs(crit(1)))) then
!
!          --   CONVERVENCE   --
        if (err .lt. crit(3)) then
            goto 200
!
!          --  NON CONVERVENCE : ITERATION SUIVANTE  --
        else
            goto 100
        endif
!
    else
!
!          --  NON CONVERVENCE : ITERATION MAXI ATTEINTE  --
        if (aredec .and. stopnc) then
            call cjsncv('CJSMDE', nitimp, iter, ndt, nvi,&
                        umess, erimp, epsd, deps, sigd,&
                        vind)
        else
            noconv = .true.
        endif
    endif
!
200  continue
!
!
!
    niter = iter
    epscon = err
!
!
! -> INCREMENTATION DE YF = YD + DY
!
    call lcsovn(nr, yd, dy, yf)
!
!
! -> MISE A JOUR DES CONTRAINTES ET VARIABLES INTERNES
!
    call lceqvn(ndt, yf(1), sigf)
    vinf(2) = yf(ndt+1)
    do 250 i = 1, ndt
        vinf(2+i) = yf(ndt+1+i)
250  continue
    vinf(nvi-1) = signe
!
!
9999  continue
end subroutine
