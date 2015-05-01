subroutine shiftc(craid, cmass, ndim, valshi)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!***********************************************************************
!    P. RICHARD     DATE 20/03/91
!-----------------------------------------------------------------------
!  BUT:  PROCEDER A UN SHIFT DES MATRICES COMPLEXES POUR CALCUL AUX
!   VALEURS PROPRES
    implicit none
!
!-----------------------------------------------------------------------
!
! CRAID    /M/: MATRICE COMPLEXE DE RAIDEUR
! CMASS    /I/: MATRICE COMPLEXE DE MASSE
! NDIM     /I/: DIMENSION DES MATRICES CARREES COMPLEXES
! VALSHI   /I/: VALEUR COMPLEXE DU SHIFT
!
!-----------------------------------------------------------------------
    complex(kind=8) :: craid(*), cmass(*), czero, valshi
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ndim
!-----------------------------------------------------------------------
    czero=dcmplx(0.d0,0.d0)
!
    if (valshi .eq. czero) goto 9999
!
    do 10 i = 1, ndim*(ndim+1)/2
        craid(i)=craid(i)+valshi*cmass(i)
10  end do
!
9999  continue
end subroutine
