subroutine hujela(mod, crit, mater, deps, sigd,&
                  sigf, iret)
    implicit none
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
!       ---------------------------------------------------------------
!       INTEGRATION ELASTIQUE NON LINEAIRE DE LA LOI DE HUJEUX
!       IN  MOD    :  MODELISATION
!           CRIT   :  CRITERES DE CONVERGENCE
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           SIGD   :  CONTRAINTE  A T
!           DEPS   :  INCREMENT DE DEFORMATION
!       OUT SIGF   :  CONTRAINTE A T+DT
!           IRET   :  CODE RETOUR DE  L'INTEGRATION DE LA LOI CJS
!                         IRET=0 => PAS DE PROBLEME
!                         IRET=1 => ECHEC
!       ---------------------------------------------------------------
    include 'asterc/r8prem.h'
    include 'asterfort/hujci1.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/lcsove.h'
    include 'asterfort/u2mess.h'
    integer :: ndt, ndi, iret, i, j
    real(kind=8) :: coef, e, nu, al, demu, i1, n, pref
    real(kind=8) :: deps(6), dsig(6), sigd(6), sigf(6)
    real(kind=8) :: hook(6, 6), mater(22, 2), crit(*)
    real(kind=8) :: zero, un, d13, deux, la, epsv, i1e
    real(kind=8) :: e1, e2, e3, nu12, nu13, nu23, g1, g2, g3, nu21, nu31, nu32
    real(kind=8) :: delta
    real(kind=8) :: piso, tole, c11, c12, c13, c22, c23, c33
    character(len=8) :: mod
    logical :: tract
!
    common /tdim/     ndt, ndi
!
    data      zero  / 0.d0 /
    data      un    / 1.d0 /
    data      d13   / 0.33333333333334D0 /
    data      deux  / 2.d0 /
    data      tole  / 1.d-7 /
!
!       ---------------------------------------------------------------
    pref = mater(8,2)
    piso = 1.5d0*mater(21,2)
    piso = zero
    n = mater(1,2)
    i1e = d13*(sigd(1)+sigd(2)+ sigd(3))
    epsv = deps(1)+deps(2)+deps(3)
    tract= .false.
!
    if (abs(n) .lt. r8prem()) then
!
        if (mater(17,1) .eq. un) then
!
            i1 = i1e + d13*mater(1,1)/(un-deux*mater(2,1)) *((i1e- piso)/pref)**n * epsv
!
        else if (mater(17,1).eq.deux) then
!
            e1 = mater(1,1)
            e2 = mater(2,1)
            e3 = mater(3,1)
            nu12 = mater(4,1)
            nu13 = mater(5,1)
            nu23 = mater(6,1)
            nu21 = mater(10,1)
            nu31 = mater(11,1)
            nu32 = mater(12,1)
            delta= mater(13,1)
!
            c11 = (un - nu23*nu32)*e1/delta
            c12 = (nu21 + nu31*nu23)*e1/delta
            c13 = (nu31 + nu21*nu32)*e1/delta
            c22 = (un - nu13*nu31)*e2/delta
            c23 = (nu32 + nu31*nu12)*e2/delta
            c33 = (un - nu21*nu12)*e3/delta
!
            i1 = (c11+c12+c13)*deps(1) + (c12+c22+c23)*deps(2) + (c13+c23+c33)*deps(3)
            i1 = i1e + d13*i1
!
        else
            call u2mess('F', 'COMPOR1_40')
        endif
!
        if ((i1 -piso)/pref .lt. tole) then
            tract=.true.
            goto 5
        endif
        goto 30
!
    endif
!
!
!--->  CALCUL DE I1=TR(SIG) A T+DT PAR METHODE DE LA SECANTE
!      OU EXPLICITEMENT SI NIVEAU HUJEUX
    call hujci1(crit, mater, deps, sigd, i1,&
                tract, iret)
!
    if (iret .eq. 1) goto 9999
!
    if (mater(17,1) .eq. un) then
!
        i1e = i1e + d13*mater(1,1)/(un-deux*mater(2,1))*epsv* ((i1e - piso)/pref)**n
!
    else if (mater(17,1).eq.deux) then
!
        e1 = mater(1,1)*((i1e -piso)/pref)**n
        e2 = mater(2,1)*((i1e -piso)/pref)**n
        e3 = mater(3,1)*((i1e -piso)/pref)**n
        nu12 = mater(4,1)
        nu13 = mater(5,1)
        nu23 = mater(6,1)
        nu21 = mater(13,1)
        nu31 = mater(14,1)
        nu32 = mater(15,1)
        delta= mater(16,1)
!
        c11 = (un - nu23*nu32)*e1/delta
        c12 = (nu21 + nu31*nu23)*e1/delta
        c13 = (nu31 + nu21*nu32)*e1/delta
        c22 = (un - nu13*nu31)*e2/delta
        c23 = (nu32 + nu31*nu12)*e2/delta
        c33 = (un - nu21*nu12)*e3/delta
!
        coef= (c11+c12+c13)*deps(1) + (c12+c22+c23)*deps(2) + (c13+&
        c23+c33)*deps(3)
        i1e = i1e + d13*coef
!
    else
        call u2mess('F', 'COMPOR1_40')
    endif
!
    if ((i1 -piso)/pref .lt. tole) tract=.true.
!
!--->  EN CAS D'ENTREE EN TRACTION, LES CONTRAINTES SONT
!      RAMENEES SUR L'AXE HYDROSTATIQUE A DES VALEURS FAIBLES
!      ( JUSTE AU-DELA DE LA TOLERANCE )
 5  continue
    if (tract) then
        do 10 i = 1, ndi
            sigf(i) = pref*tole*1.01d0 +piso
10      continue
        do 20 i = ndi+1, ndt
            sigf(i) = zero
20      continue
        goto 9999
    endif
30  continue
!
!
!---> CALCUL DU COEF  (-----------)**N ET MODULE_YOUNG A T+DT
    call lcinma(zero, hook)
!
    coef = ((i1 -piso)/pref)**n
!
    if (mod(1:2) .eq. '3D' .or. mod(1:6) .eq. 'D_PLAN' .or. mod(1:4) .eq. 'AXIS') then
!
        if (mater(17,1) .eq. un) then
!
            e = mater(1,1)*coef
            nu = mater(2,1)
            al = e*(un-nu) /(un+nu) /(un-deux*nu)
            demu = e /(un+nu)
            la = e*nu/(un+nu)/(un-deux*nu)
!
            do 32 i = 1, ndi
                do 32 j = 1, ndi
                    if (i .eq. j) hook(i,j) = al
                    if (i .ne. j) hook(i,j) = la
32              continue
            do 35 i = ndi+1, ndt
                hook(i,i) = demu
35          continue
!
        else if (mater(17,1).eq.deux) then
!
            e1 = mater(1,1)*coef
            e2 = mater(2,1)*coef
            e3 = mater(3,1)*coef
            nu12 = mater(4,1)
            nu13 = mater(5,1)
            nu23 = mater(6,1)
            g1 = mater(7,1)*coef
            g2 = mater(8,1)*coef
            g3 = mater(9,1)*coef
            nu21 = mater(13,1)
            nu31 = mater(14,1)
            nu32 = mater(15,1)
            delta= mater(16,1)
!
            hook(1,1) = (un - nu23*nu32)*e1/delta
            hook(1,2) = (nu21 + nu31*nu23)*e1/delta
            hook(1,3) = (nu31 + nu21*nu32)*e1/delta
            hook(2,2) = (un - nu13*nu31)*e2/delta
            hook(2,3) = (nu32 + nu31*nu12)*e2/delta
            hook(3,3) = (un - nu21*nu12)*e3/delta
            hook(2,1) = hook(1,2)
            hook(3,1) = hook(1,3)
            hook(3,2) = hook(2,3)
            hook(4,4) = g1
            hook(5,5) = g2
            hook(6,6) = g3
!
        else
            call u2mess('F', 'COMPOR1_40')
        endif
!
    else if (mod(1:6) .eq. 'C_PLAN' .or. mod(1:2) .eq. '1D') then
!
        call u2mess('F', 'COMPOR1_4')
!
    endif
!
!
!--->   INCREMENTATION DES CONTRAINTES  SIGF = SIGD + HOOK DEPS
    call lcprmv(hook, deps, dsig)
    call lcsove(sigd, dsig, sigf)
!
9999  continue
end subroutine
