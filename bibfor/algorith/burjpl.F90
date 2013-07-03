subroutine burjpl(nmat, mater, nr, drdy, dsde)
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
! person_in_charge: alexandre.foucault at edf.fr
!     ----------------------------------------------------------------
!     CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY(DY)
!     POUR LE MODELE BETON_BURGER_FP
!     IN  NR     :  DIMENSION JACOBIEN
!         NMAT   :  DIMENSION MATER
!         MATER  :  COEFFICIENTS MATERIAU
!         NR     :  DIMENSION MATRICE JACOBIENNE
!         DRDY   :  MATRICE JACOBIENNE
!     OUT DSDE   :  MATRICE TANGENTE EN VITESSE
!     ----------------------------------------------------------------
    implicit none
!     ----------------------------------------------------------------
#include "asterc/r8prem.h"
#include "asterfort/lcdima.h"
#include "asterfort/lceqma.h"
#include "asterfort/lcinma.h"
#include "asterfort/lcopli.h"
#include "asterfort/lcprmm.h"
#include "asterfort/mgauss.h"
    common /tdim/   ndt  , ndi
!     ----------------------------------------------------------------
    integer :: nmat, nr, iret, ndt, ndi, i, j
    real(kind=8) :: drdy(nr, nr), dsde(6, 6), mater(nmat, 2)
    real(kind=8) :: hook(6, 6), y0(6, 6), y1(6, 6), y2(6, 6), y3(6, 6)
    real(kind=8) :: invela(6, 6), drdyt(6, 6), det, maxi, mini
!
! === =================================================================
! --- INITIALISATION MATRICES A ZERO
! === =================================================================
    call lcinma(0.d0, y0)
    call lcinma(0.d0, y1)
    call lcinma(0.d0, y2)
    call lcinma(0.d0, y3)
! === =================================================================
! --- RECHERCHE DU MAXIMUM DE DRDY
! === =================================================================
    maxi = 0.d0
    do 1 i = 1, nr
        do 2 j = 1, nr
            if(abs(drdy(i,j)).gt.maxi)maxi = abs(drdy(i,j))
 2      end do
 1  end do
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
! === =================================================================
! --- SEPARATION DES TERMES DU JACOBIEN
! === =================================================================
    do 5 i = 1, ndt
        do 6 j = 1, ndt
            y0(i,j) = drdy(i,j)
            y1(i,j) = drdy(i,j+ndt)
            y2(i,j) = drdy(i+ndt,j)
            y3(i,j) = drdy(i+ndt,j+ndt)
 6      continue
 5  end do
!
! === =================================================================
! --- CONSTRUCTION TENSEUR RIGIDITE ELASTIQUE A T+DT
! === =================================================================
    call lcopli('ISOTROPE', '3D      ', mater, hook)
! === =================================================================
! --- CONSTRUCTION TENSEUR CONSTITUTIF TANGENT DSDE
! === =================================================================
! --- INVERSION DU TERME Y3
    call lcinma(0.d0, invela)
    do 7 i = 1, ndt
        invela(i,i) = 1.d0
 7  end do
!
    call mgauss('NCVP', y3, invela, 6, ndt,&
                ndt, det, iret)
    if (iret .gt. 0) call lcinma(0.d0, invela)
!
! --- PRODUIT DU TERME (Y3)^-1 * Y2 = DRDYT
    call lcprmm(invela, y2, drdyt)
!
! --- PRODUIT DU TERME (Y1) * DRDYT = INVELA
    call lcprmm(y1, drdyt, invela)
!
! --- DIFFERENCE DE MATRICE (DR1DY1 - INVELA) = DRDYT
    call lcdima(y0, invela, drdyt)
!
! --- INVERSION DU TERME DRDYT
    call lcinma(0.d0, dsde)
    do 8 i = 1, ndt
        dsde(i,i) = -1.d0
 8  end do
    call mgauss('NCVP', drdyt, dsde, 6, ndt,&
                ndt, det, iret)
    if (iret .gt. 1) call lceqma(hook, dsde)
!
end subroutine
