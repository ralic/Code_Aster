subroutine lcoptg(nmat, mater, nr, nvi, drdy,&
                  sigeps, dsde, iret)
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
! TOLE CRS_1404
!     ----------------------------------------------------------------
!     CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY(DY)
!     POUR LE MODELE BETON_BURGER_FP
!     IN  NR     :  DIMENSION JACOBIEN
!         NMAT   :  DIMENSION MATER
!         MATER  :  COEFFICIENTS MATERIAU
!         NR     :  DIMENSION MATRICE JACOBIENNE
!         DRDY   :  MATRICE JACOBIENNE
!         SIGEPS :  =1 si DY=(DSIG, DZ) et =0 si DY=(DEPSE, DZ)
!     OUT DSDE   :  MATRICE TANGENTE EN VITESSE
!     ----------------------------------------------------------------
    implicit none
!     ----------------------------------------------------------------
    include 'asterc/r8prem.h'
    include 'asterfort/lceqma.h'
    include 'asterfort/lcopli.h'
    include 'asterfort/mgauss.h'
    include 'asterfort/promat.h'
    include 'asterfort/r8inir.h'
    common /tdim/   ndt  , ndi
!     ----------------------------------------------------------------
    integer :: nmat, nr, iret, ndt, ndi, i, j, k, nvi, norm, sigeps
    real(kind=8) :: hook(6, 6), drdy(nr, nr), dsde(6, 6), mater(nmat, 2)
    real(kind=8) :: y0(ndt, ndt), y1(ndt, nvi), y2(nvi, ndt), y3(nvi, nvi)
    real(kind=8) :: y4(ndt, ndt), y5(ndt, ndt), det, maxi, mini
    real(kind=8) :: dsdeb(ndt, ndt)
    character(len=4) :: cargau
! === =================================================================
! --- RECHERCHE DU MAXIMUM DE DRDY
! === =================================================================
!
    norm=0
    if (norm .eq. 0) goto 30
!
    maxi = 0.d0
    do 1 i = 1, nr
        do 2 j = 1, nr
            if(abs(drdy(i,j)).gt.maxi)maxi = abs(drdy(i,j))
 2      end do
 1  end do
!
! === =================================================================
! --- DIMENSIONNEMENT A R8PREM
! === =================================================================
    mini = r8prem()*maxi
    do 3 i = 1, nr
        do 4 j = 1, nr
            if(abs(drdy(i,j)).lt.mini)drdy(i,j) = 0.d0
 4      end do
 3  end do
!
30  continue
!
! === =================================================================
! --- SEPARATION DES TERMES DU JACOBIEN
! === =================================================================
    do 5 i = 1, ndt
        do 6 j = 1, ndt
            y0(i,j) = drdy(i,j)
 6      continue
 5  end do
    do 51 i = 1, ndt
        do 61 j = 1, nvi
            y1(i,j) = drdy(i,j+ndt)
61      continue
51  continue
    do 52 i = 1, nvi
        do 62 j = 1, ndt
            y2(i,j) = drdy(i+ndt,j)
62      continue
52  continue
    do 53 i = 1, nvi
        do 63 j = 1, nvi
            y3(i,j) = drdy(i+ndt,j+ndt)
63      continue
53  continue
!
! === =================================================================
! --- CONSTRUCTION TENSEUR RIGIDITE ELASTIQUE A T+DT
! === =================================================================
    call lcopli('ISOTROPE', '3D      ', mater, hook)
!
!     CHOIX DES PARAMETRES DE LANCEMENT DE MGAUSS
!     METHODE 'S' : SURE
    cargau = 'NCSP'
!     METHODE 'W' : RATEAU
!      CARGAU = 'NCWP'
! === =================================================================
! --- CONSTRUCTION TENSEUR CONSTITUTIF TANGENT DSDE
! === =================================================================
!     Y2=INVERSE(Y3)*Y2
    call mgauss(cargau, y3, y2, nvi, nvi,&
                ndt, det, iret)
    if (iret .gt. 1) then
        call lceqma(hook, dsde)
        goto 9999
    endif
! --- PRODUIT DU TERME (Y3)^-1 * Y2 = Y4
    call promat(y1, ndt, ndt, nvi, y2,&
                nvi, nvi, ndt, y4)
!
! --- DIFFERENCE DE MATRICE (DR1DY1 - Y4) = Y5
    do 13 i = 1, ndt
        do 14 j = 1, ndt
            y5(i,j)=y0(i,j)-y4(i,j)
14      continue
13  end do
!
! --- INVERSION DU TERME Y5
    call r8inir(ndt*ndt, 0.d0, dsdeb, 1)
    do 8 i = 1, ndt
        dsdeb(i,i) = 1.d0
 8  end do
    call mgauss(cargau, y5, dsdeb, ndt, ndt,&
                ndt, det, iret)
!
    if (iret .gt. 1) then
        call lceqma(hook, dsde)
    else
        call r8inir(36, 0.d0, dsde, 1)
        if (sigeps .eq. 1) then
            do 22 i = 1, ndt
                do 21 j = 1, ndt
                    do 20 k = 1, ndt
                        dsde(i,j)=dsdeb(i,j)
20                  continue
21              continue
22          continue
        else
            do 12 i = 1, ndt
                do 11 j = 1, ndt
                    do 10 k = 1, ndt
                        dsde(i,j)=dsde(i,j)+hook(i,k)*dsdeb(k,j)
10                  continue
11              continue
12          continue
        endif
!
    endif
!
9999  continue
end subroutine
