subroutine calcfe(nr, ndt, nvi, vind, df,&
                  gamsns, fe, fp, iret)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!       ----------------------------------------------------------------
!       MONOCRISTAL : CALCUL DE Fe et Fp, F=Fe.Fp
!       IN  NR     :  DIMENSION DECLAREE DRDY
!           NDT    :  NOMBRE DE COMPOSANTES DE SIGMA (6)
!           NVI    :  NOMBRE DE VARIABLES INTERNES
!           VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
!                     contiennent Fe(t), Fp(t)
!           DF     :  Increment de Gradient de deformation
!           GAMSNS :  Somme de dGamma.Ms*Ns
!       OUT FE     :  Gradient de tranformation elastique
!           FP     :  Gradient de tranformation plastique
!
    include 'asterc/r8prem.h'
    include 'asterfort/lcdetf.h'
    include 'asterfort/matinv.h'
    include 'asterfort/pmat.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/dscal.h'
    real(kind=8) :: fe(3, 3), df(3, 3), gamsns(3, 3), dffe(3, 3), dfpm(3, 3)
    real(kind=8) :: fem(3, 3)
    real(kind=8) :: vind(*), id(3, 3), det, coef, dfp(3, 3), expo, fp(3, 3)
    real(kind=8) :: fpm(3, 3), dfpmax, dfpmin, det2
    integer :: nr, ndt, iret, iopt, i, nvi
    data id/1.d0,0.d0,0.d0, 0.d0,1.d0,0.d0, 0.d0,0.d0,1.d0/
!     ----------------------------------------------------------------
!
    iret=0
    iopt=2
!
    call dcopy(9, vind(nvi-3-18+10), 1, fem, 1)
    call dcopy(9, vind(nvi-3-18+1 ), 1, fpm, 1)
    call daxpy(9, 1.d0, id, 1, fem,&
               1)
    call daxpy(9, 1.d0, id, 1, fpm,&
               1)
!
    call pmat(3, df, fem, dffe)
!
    call dcopy(9, gamsns, 1, dfp, 1)
!
    if (iopt .eq. 1) then
!
!        suivant ANNAND 1996
!
        call daxpy(9, 1.d0, id, 1, dfp,&
                   1)
!
!        TEST ANALOGUE A SIMO_MIEHE NMGPFI
        dfpmax=0.d0
        dfpmin=100.d0
        do 10 i = 1, 3
            if (dfp(i,i) .gt. dfpmax) dfpmax=dfp(i,i)
            if (dfp(i,i) .lt. dfpmin) dfpmin=dfp(i,i)
10      continue
        if ((dfpmax.gt.1.d3) .or. (dfpmin.lt.1.d-3)) then
            iret=1
            goto 9999
        endif
!
        call lcdetf(3, dfp, det)
!
        if (det .gt. r8prem()) then
            expo=-1.d0/3.d0
            coef=det**expo
            call dscal(9, coef, dfp, 1)
        else
            iret=1
            goto 9999
        endif
!
        call matinv('S', 3, dfp, dfpm, det2)
!
    else if (iopt.eq.2) then
!
!        linearisation directe de exp(-dgamma.ms x ns)
!
        call dcopy(9, dfp, 1, dfpm, 1)
        call dscal(9, -1.d0, dfpm, 1)
        call daxpy(9, 1.d0, id, 1, dfpm,&
                   1)
!
        dfpmax=0.d0
        dfpmin=100.d0
        do 20 i = 1, 3
            if (dfpm(i,i) .gt. dfpmax) dfpmax=dfpm(i,i)
            if (dfpm(i,i) .lt. dfpmin) dfpmin=dfpm(i,i)
20      continue
        if ((dfpmax.gt.1.d3) .or. (dfpmin.lt.1.d-3)) then
            iret=1
            goto 9999
        endif
!
        call lcdetf(3, dfpm, det)
!
        if (det .gt. r8prem()) then
            expo=1.d0/3.d0
            coef=det**expo
            call dscal(9, coef, dfpm, 1)
        else
            iret=1
            goto 9999
        endif
!
        call matinv('S', 3, dfpm, dfp, det2)
!
!
    else if (iopt.eq.3) then
!
! suivant DE SOUZA-NIETO
!
!         DFPMAX=0.D0
!         DFPMIN=100.D0
!         DO 30 I=1,3
!            IF (DFP(I,I).GT.DFPMAX) DFPMAX=DFP(I,I)
!            IF (DFP(I,I).LT.DFPMIN) DFPMIN=DFP(I,I)
! 30      CONTINUE
!         IF ((ABS(DFPMAX).GT.10.D0).OR.(ABS(DFPMIN).GT.10.D0)) THEN
!           IRET=1
!           GOTO 9999
!         ENDIF
!         CALL DSCAL(9,-1.0D0,DFP,1)
!         CALL EXPMAP(DFPM,NOCONV,DFP)
!         IF (NOCONV) THEN
!            IRET=1
!            GOTO 9999
!         ENDIF
!         CALL MATINV('S',3,DFPM,DFP,DET2)
!
    endif
!
    call pmat(3, dffe, dfpm, fe)
!
! post traitement
!
    call pmat(3, dfp, fpm, fp)
!
9999  continue
!
end subroutine
