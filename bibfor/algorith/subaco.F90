subroutine subaco(nno, dff, geom, cova)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
    implicit none
!
    integer :: nno
    real(kind=8) :: dff(2, nno), geom(3, nno)
    real(kind=8) :: cova(3, 3)
!
!.......................................................................
!     CALCUL DE LA BASE COVARIANTE POUR UN ELEMENT SURFACIQUE
!.......................................................................
! IN  NNO     NOMBRE DE NOEUDS
! IN  DFF     DERIVEE DES F. DE FORME
! IN  GEOM    COORDONNEES DES NOEUDS
! OUT COVA    COORDONNEES DES VECTEURS DE LA BASE COVARAINTE
!.......................................................................
!
    integer :: n, i
    real(kind=8) :: norme
!
!
    do 10 i = 1, 3
        cova(i,1) = 0.d0
        cova(i,2) = 0.d0
10  end do
!
!
!    CALCUL DES VECTEURS TANGENTS
    do 20 n = 1, nno
        do 25 i = 1, 3
            cova(i,1) = cova(i,1) + dff(1,n)*geom(i,n)
            cova(i,2) = cova(i,2) + dff(2,n)*geom(i,n)
25      continue
20  end do
!
!
!    CALCUL DE LA NORMALE (PRODUIT VECTORIEL DES VECTEURS TANGENTS)
    cova(1,3) = cova(2,1)*cova(3,2) - cova(3,1)*cova(2,2)
    cova(2,3) = cova(3,1)*cova(1,2) - cova(1,1)*cova(3,2)
    cova(3,3) = cova(1,1)*cova(2,2) - cova(2,1)*cova(1,2)
!
    norme = sqrt(cova(1,3)**2 + cova(2,3)**2 + cova(3,3)**2)
    cova(1,3) = cova(1,3) / norme
    cova(2,3) = cova(2,3) / norme
    cova(3,3) = cova(3,3) / norme
!
end subroutine
