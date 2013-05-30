subroutine intenc(nbna, jac, vectx, vecty, mat11,&
                  mat22, mat12, nx, ny, inte)
    implicit none
    integer :: nbna
    real(kind=8) :: jac(3), vectx(3), vecty(3), mat11(3), mat22(3), mat12(3)
    real(kind=8) :: nx(3), ny(3), inte
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     BUT:
!         CALCUL DES TERMES PAR NOEUD POUR UNE INTEGRATION
!         DE NEWTON-COTES À 2 OU 3 POINTS DU TYPE :
!              (
!              |     (VECT-MAT.N)**2 dBORD
!              )BORD
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NBNA   : NOMBRE DE POINTS D'INTEGRATION
! IN   JAC    : VECTEUR DES JACOBIENS DE LA TRANSFORMATION AUX NOEUDS
! IN   VECTX  : COMPOSANTES EN X DU VECTEUR AUX NOEUDS
! IN   VECTY  : COMPOSANTES EN Y DU VECTEUR AUX NOEUDS
! IN   MAT11  : COMPOSANTES 11 DE LA MATRICE AUX NOEUDS
! IN   MAT22  : COMPOSANTES 22 DE LA MATRICE AUX NOEUDS
! IN   MAT12  : COMPOSANTES 12 DE LA MATRICE AUX NOEUDS
! IN   NX     : VECTEUR DES ABSCISSES DES NORMALES AUX NOEUDS
! IN   NY     : VECTEUR DES ORDONNEES DES NORMALES AUX NOEUDS
!
!      SORTIE :
!-------------
! OUT  INTE   : VALEUR DE L'INTEGRALE
!
! ----------------------------------------------------------------------
    real(kind=8) :: inte1, inte2, inte3, poids(3)
! ----------------------------------------------------------------------
!
!        ON REALISE UNE INTEGRATION DE NEWTON-COTES AVEC 2 OU 3 POINTS
!        D'INTEGRATION SELON LE NOMBRE DE POINTS DE L'ARETE CONSIDEREE
!        SOIT NBNA = 2 OU 3
!
    inte1=jac(1)*&
     &      ((vectx(1)-mat11(1)*nx(1)-mat12(1)*ny(1))**2&
     &      +(vecty(1)-mat12(1)*nx(1)-mat22(1)*ny(1))**2)
!
    inte2=jac(2)*&
     &      ((vectx(2)-mat11(2)*nx(2)-mat12(2)*ny(2))**2&
     &      +(vecty(2)-mat12(2)*nx(2)-mat22(2)*ny(2))**2)
!
    poids(1)=1.d0
    poids(2)=1.d0
!
    inte=inte1*poids(1)+inte2*poids(2)
!
    if (nbna .eq. 3) then
!
        inte3=jac(3)* ((vectx(3)-mat11(3)*nx(3)-mat12(3)*ny(3))**2&
        +(vecty(3)-mat12(3)*nx(3)-mat22(3)*ny(3))**2)
!
        poids(1)=1.d0/3.d0
        poids(2)=1.d0/3.d0
        poids(3)=4.d0/3.d0
!
        inte=inte1*poids(1)+inte2*poids(2)+inte3*poids(3)
!
    endif
!
end subroutine
