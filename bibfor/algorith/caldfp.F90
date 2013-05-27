subroutine caldfp(msns, gamsns, dfpmdg, iret)
    implicit none
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jean-michel.proix at edf.fr
!     ----------------------------------------------------------------
!
!     MONOCRISTAL : calcul des derivees de Fe en GDEF
!     IN  MSNS   : MS * NS
!         GAMSNS : somme des dGamma.Ms*Ns
!     OUT DFPMDG : dFp/dGamma_S
!         IRET   :  CODE RETOUR
!
!    Pour un seul systeme on calcule DF.FEn.d(Fp-1)/dGammaS
!
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/lcdetf.h'
    include 'asterfort/lctr2m.h'
    include 'asterfort/matinv.h'
    include 'asterfort/r8inir.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/ddot.h'
    include 'blas/dscal.h'
    integer :: iret, iopt, i, j, k, l
    real(kind=8) :: msns(3, 3), gamsns(3, 3), ddetdg
    real(kind=8) :: id(3, 3), coef, expo
    real(kind=8) :: det2, dfpmdg(3, 3)
    real(kind=8) :: dfpdg(3, 3), dfpmdf(3, 3, 3, 3), amax, amin, bmax, bmin
    real(kind=8) :: a(3, 3), am(3, 3), amt(3, 3), deta, coef2
    real(kind=8) :: b(3, 3), bm(3, 3), bmt(3, 3), detb
    data id/1.d0,0.d0,0.d0, 0.d0,1.d0,0.d0, 0.d0,0.d0,1.d0/
!     ----------------------------------------------------------------
!
    iret=0
    iopt=2
!
    if (iopt .eq. 1) then
!
!        calcul de dFp/dGamma suivant ANNAND 1996
!
        call dcopy(9, gamsns, 1, a, 1)
!
        call daxpy(9, 1.d0, id, 1, a,&
                   1)
!
!        TEST ANALOGUE A SIMO_MIEHE NMGPFI
        amax=0.d0
        amin=100.d0
        do 10 i = 1, 3
            if (a(i,i) .gt. amax) amax=a(i,i)
            if (a(i,i) .lt. amin) amin=a(i,i)
10      continue
        if ((amax.gt.1.d3) .or. (amin.lt.1.d-3)) then
            iret=1
            goto 9999
        endif
!
        call lcdetf(3, a, deta)
!
        if (deta .gt. r8prem()) then
            expo=-1.d0/3.d0
            coef=deta**expo
        else
            iret=1
            goto 9999
        endif
!
        call matinv('S', 3, a, am, det2)
        call lctr2m(3, am, amt)
!
        ddetdg = ddot(9,amt,1,msns,1)
!
        call dscal(9, ddetdg, a, 1)
!
        call dcopy(9, a, 1, dfpdg, 1)
!
        call dscal(9, -1.d0/3.d0, dfpdg, 1)
!
        call daxpy(9, 1.d0, msns, 1, dfpdg,&
                   1)
!
        call dscal(9, coef, dfpdg, 1)
!
! calcul de dFp-1
        call r8inir(81, 0.d0, dfpmdf, 1)
        do 100 i = 1, 3
            do 100 j = 1, 3
                do 100 k = 1, 3
                    do 100 l = 1, 3
                        dfpmdf(i,j,k,l)=dfpmdf(i,j,k,l)+am(i,k)*amt(j,&
                        l)
100                  continue
        coef2= -deta**(2.d0/3.d0)
!
        call dscal(81, coef2, dfpmdf, 1)
!
        call r8inir(9, 0.d0, dfpmdg, 1)
        do 200 i = 1, 3
            do 200 j = 1, 3
                do 200 k = 1, 3
                    do 200 l = 1, 3
                        dfpmdg(i,j)=dfpmdg(i,j)+dfpmdf(i,j,k,l)*dfpdg(&
                        k,l)
200                  continue
!
!
    else if (iopt.eq.2) then
!
!        calcul de dFp/dGamma par linearisation directe
!        de exp(-dgamma.ms x ns)
!
        call dcopy(9, gamsns, 1, b, 1)
        call dscal(9, -1.d0, b, 1)
        call daxpy(9, 1.d0, id, 1, b,&
                   1)
!
        bmax=0.d0
        bmin=100.d0
        do 20 i = 1, 3
            if (b(i,i) .gt. bmax) bmax=b(i,i)
            if (b(i,i) .lt. bmin) bmin=b(i,i)
20      continue
        if ((bmax.gt.1.d3) .or. (bmin.lt.1.d-3)) then
            iret=1
            goto 9999
        endif
!
        call lcdetf(3, b, detb)
!
        if (detb .gt. r8prem()) then
            expo=1.d0/3.d0
            coef=detb**expo
        else
            iret=1
            goto 9999
        endif
!
        call matinv('S', 3, b, bm, det2)
!
        call lctr2m(3, bm, bmt)
!
        ddetdg = ddot(9,bmt,1,msns,1)
!
        call dscal(9, ddetdg, b, 1)
!
        call dcopy(9, b, 1, dfpmdg, 1)
!
        call dscal(9, 1.d0/3.d0, dfpmdg, 1)
!
        call daxpy(9, 1.d0, msns, 1, dfpmdg,&
                   1)
!
        call dscal(9, -coef, dfpmdg, 1)
!
!
    else if (iopt.eq.3) then
!
! suivant DE SOUZA-NIETO
        call assert(.false.)
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
!         CALL DEXPMAP(DFPMDG,NOCONV,DFP)
!
    else
        call assert(.false.)
!
    endif
!
!
9999  continue
end subroutine
