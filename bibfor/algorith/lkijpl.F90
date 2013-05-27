subroutine lkijpl(nmat, mater, sigf, nr, drdy,&
                  dsde)
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
    implicit none
! person_in_charge: alexandre.foucault at edf.fr
!       ----------------------------------------------------------------
!       MATRICE COHERENTE DE LETK A T+DT
!       IN  NMAT   :  DIMENSION MATER
!           MATER  :  COEFFICIENTS MATERIAU
!           SIGF   :  ETAT DE CONTRAINTES A T+DT
!           NR     :  DIMENSION MATRICE JACOBIENNE
!           DRDY   :  MATRICE JACOBIENNE (NR*NR)
!       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
!       ----------------------------------------------------------------
    include 'asterc/r8prem.h'
    include 'asterfort/lcdima.h'
    include 'asterfort/lceqma.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcopli.h'
    include 'asterfort/lcprmm.h'
    include 'asterfort/lcprsm.h'
    include 'asterfort/mgauss.h'
    include 'asterfort/prmama.h'
    include 'asterfort/r8inir.h'
    integer :: nmat, nr
    real(kind=8) :: dsde(6, 6), mater(nmat, 2)
    real(kind=8) :: drdy(nr, nr), sigf(6)
!
    integer :: i, j, iret, ier, ndt, ndi
    real(kind=8) :: jss(6, 6), jsz(6, 3), jzs(3, 6), jzz(3, 3)
    real(kind=8) :: hook(6, 6), hooknl(6, 6), i1, coefnl
    real(kind=8) :: patm, nelas, invjzz(3, 3), j3x6(3, 6)
    real(kind=8) :: det, j6x6(6, 6), dijaco(6, 6), invdij(6, 6)
    real(kind=8) :: maxi, mini, mue, mu
!
!       --------------------------------------------------------------
    common /tdim/   ndt  , ndi
!       --------------------------------------------------------------
! === =================================================================
! --- INITIALISATION MATRICES A ZERO
! === =================================================================
    call lcinma(0.d0, jss)
    call r8inir(18, 0.d0, jsz, 1)
    call r8inir(18, 0.d0, jzs, 1)
    call r8inir(9, 0.d0, jzz, 1)
! === =================================================================
! --- RECHERCHE DU MAXIMUM DE DRDY
! === =================================================================
    maxi = 0.d0
    do 1 i = 1, nr
        do 2 j = 1, nr
            if(abs(drdy(i,j)).gt.maxi)maxi = abs(drdy(i,j))
 2      continue
 1  continue
! === =================================================================
! --- DIMENSIONNEMENT A R8PREM
! === =================================================================
    mini = r8prem()*maxi
    do 3 i = 1, nr
        do 4 j = 1, nr
            if(abs(drdy(i,j)).lt.mini)drdy(i,j) = 0.d0
 4      continue
 3  continue
!
! === =================================================================
! --- SEPARATION DES TERMES DU JACOBIEN
! === =================================================================
    do 5 i = 1, ndt
        do 6 j = 1, ndt
            jss(i,j) = drdy(i,j)
 6      continue
 5  continue
!
    do 7 i = 1, 3
        do 8 j = 1, ndt
            jsz(j,i)=drdy(j,ndt+i)
            jzs(i,j)=drdy(ndt+i,j)
 8      continue
 7  continue
!
    do 9 i = 1, 3
        do 10 j = 1, 3
            jzz(i,j) = drdy(ndt+i,ndt+j)
10      continue
 9  continue
! === =================================================================
! --- CONSTRUCTION TENSEUR RIGIDITE ELASTIQUE A T+DT
! === =================================================================
    call lcopli('ISOTROPE', '3D      ', mater, hook)
! --- PRISE EN COMPTE DU TERME NON LINEAIRE E(I1+) = E0*(I1+/PA)**NE
! --- AVEC I1 = -TRACE(SIGMA), CAR EQUATIONS DU MODELE LETK
! --- SONT EXPRIMEES EN CONVENTION MECANIQUE DES SOLS
    i1 = -(sigf(1)+sigf(2)+sigf(3))
    patm = mater(1,2)
    nelas = mater(2,2)
    coefnl = (i1/(3.d0*patm))**nelas
!
    mue = mater(4,1)
    mu = -mue*coefnl
!
! === =================================================================
! --- MISE A L'ECHELLE DU NUMERATEUR DR(1:6)/DEPS
! === =================================================================
    coefnl = coefnl/mu
!
    call lcprsm(coefnl, hook, hooknl)
! === =================================================================
! --- CONSTRUCTION TENSEUR CONSTITUTIF TANGENT DSDE
! === =================================================================
! --- INVERSION DU TERME JZZ
    call r8inir(9, 0.d0, invjzz, 1)
    do 11 i = 1, 3
        invjzz(i,i) = 1.d0
11  continue
!
    call mgauss('NCVP', jzz, invjzz, 3, 3,&
                3, det, iret)
    if (iret .gt. 0) call r8inir(9, 0.d0, invjzz, 1)
!
! --- PRODUIT DU TERME (JZZ)^-1*JZS = J3X6
    call prmama(1, invjzz, 3, 3, 3,&
                jzs, 3, 3, ndt, j3x6,&
                3, 3, ndt, ier)
    if (ier .gt. 0) write(6,*)'ECHEC AVEC PRMAMA 1'
!
! --- PRODUIT DU TERME JSZ*(JZZ)^-1*JZS = JSZ*J3*6 = J6X6
    call prmama(1, jsz, 6, ndt, 3,&
                j3x6, 3, 3, ndt, j6x6,&
                6, ndt, ndt, ier)
    if (ier .gt. 0) write(6,*)'ECHEC AVEC PRMAMA 2'
!
! --- DIFFERENCE DE MATRICE (JSS - J6X6) = DIJACO
    call lcdima(jss, j6x6, dijaco)
!
! --- INVERSION DU TERME (DIJACO)^-1 = INVDIJ
    call lcinma(0.d0, invdij)
    do 12 i = 1, ndt
        invdij(i,i) = 1.d0
12  continue
    call mgauss('NCVP', dijaco, invdij, 6, ndt,&
                ndt, det, iret)
    if (iret .gt. 1) call lceqma(hook, dsde)
!
! --- CONSTRUCTION DSDE = INVDIJ*HOOKNL
    call lcprmm(invdij, hooknl, dsde)
!
end subroutine
