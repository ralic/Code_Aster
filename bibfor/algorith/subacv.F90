subroutine subacv(cova, metr, jac, cnva, a)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
!
    real(kind=8) :: cova(3, 3), metr(2, 2), jac
    real(kind=8) :: cnva(3, 2), a(2, 2)
!
!.......................................................................
!     CALCUL DE LA BASE CONTRAVARIANTE (EN DIMENSION 3)
!.......................................................................
! IN  COVA    COORDONNEES DES VECTEURS DE LA BASE COVARAINTE
! IN  METR    TENSEUR METRIQUE (2X2)
! IN  JAC     JACOBIEN DE LA METRIQUE
! OUT CNVA    COORDONNEES DES DEUX VECTEURS CONTRAVARIANTS
!     A       MATRICE PRODUIT SCALAIRE
!.......................................................................
!
    integer :: i
    real(kind=8) :: det
!
!
!    CALCUL DE LA METRIQUE CONTRAVARIANTE
    det = jac**2
    a(1,1) = metr(2,2) / det
    a(2,2) = metr(1,1) / det
    a(1,2) = - metr(2,1) / det
    a(2,1) = a(1,2)
!
!
!    CALCUL DES VECTEURS CONTRAVARIANTS
    do 10 i = 1, 3
        cnva(i,1) = a(1,1)*cova(i,1) + a(1,2)*cova(i,2)
        cnva(i,2) = a(2,1)*cova(i,1) + a(2,2)*cova(i,2)
10  end do
!
!
end subroutine
