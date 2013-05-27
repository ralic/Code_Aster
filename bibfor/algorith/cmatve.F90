subroutine cmatve(mat, vectin, vectou, ndim)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
!***********************************************************************
!    B. GUIGON    P. RICHARD                   DATE 06/04/92
!-----------------------------------------------------------------------
!  BUT:  < PRODUIT MATRICE VECTEUR COMPLEXE >
!
!   CETTE ROUTINE CALCULE LE PRODUIT D'UNE MATRICE COMPLEXE PAR UNE
! VECTEUR COMPLEXE
!
!-----------------------------------------------------------------------
!
! MAT      /I/: MATRICE COMPLEXE
! VECTIN   /I/: VECTEUR COMPLEXE D'ENTREE
! VECTOUT  /I/: VECTEUR COMPLXE DE SORTIE
! NDIM     /I/: DIMENSION DU VECTEUR ET DE LA MATRICE
!
!-----------------------------------------------------------------------
!
    integer :: ndim
    integer :: i, j
    complex(kind=8) :: mat(ndim, ndim), vectin(ndim), vectou(ndim)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
    do 10 i = 1, ndim
        vectou(i) = dcmplx(0.d0,0.d0)
        do 20 j = 1, ndim
            vectou(i) = vectou(i) + mat(i,j)*vectin(j)
20      continue
10  end do
    goto 9999
!
9999  continue
end subroutine
