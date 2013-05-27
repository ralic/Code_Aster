subroutine intega(npgf, jac, poidsf, vectx, vecty,&
                  vectz, mat11, mat22, mat33, mat12,&
                  mat13, mat23, nx, ny, nz,&
                  inte)
    implicit none
    integer :: npgf
    real(kind=8) :: jac(9), poidsf(9)
    real(kind=8) :: vectx(9), vecty(9), vectz(9)
    real(kind=8) :: mat11(9), mat22(9), mat33(9), mat12(9), mat13(9), mat23(9)
    real(kind=8) :: nx(9), ny(9), nz(9)
    real(kind=8) :: inte
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!         CALCUL DES TERMES PAR POINT DE GAUSS POUR UNE INTEGRATION
!         DE GAUSS DU TYPE :
!              (
!              |     (VECT-MAT.N)**2 DFACE
!              )FACE
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NPGF   : NOMBRE DE POINTS D'INTEGRATION
! IN   JAC    : VECTEUR DES JACOBIENS DE LA TRANSFORMATION AUX NOEUDS
! IN   POIDSF : VECTEUR DES POIDS AUX NOEUDS POUR LA FACE
! IN   VECTX  : COMPOSANTES EN X DU VECTEUR AUX NOEUDS
! IN   VECTY  : COMPOSANTES EN Y DU VECTEUR AUX NOEUDS
! IN   VECTZ  : COMPOSANTES EN Z DU VECTEUR AUX NOEUDS
! IN   MATIJ  : COMPOSANTES IJ DE LA MATRICE AUX NOEUDS
! IN   NX     : COMPOSANTES EN X DES NORMALES AUX NOEUDS
! IN   NY     : COMPOSANTES EN Y DES NORMALES AUX NOEUDS
! IN   NZ     : COMPOSANTES EN Z DES NORMALES AUX NOEUDS
!
!      SORTIE :
!-------------
! OUT  INTE  : TERME INTEGRE POUR UNE FACE
!
! ......................................................................
    integer :: ipgf
! ----------------------------------------------------------------------
!
    inte=0.0d0
!
    do 10 , ipgf = 1 , npgf
!
    inte=inte+((vectx(ipgf)-mat11(ipgf)*nx(ipgf) -mat12(ipgf)*ny(&
        ipgf)-mat13(ipgf)*nz(ipgf))**2 +(vecty(ipgf)-mat12(ipgf)*nx(&
        ipgf) -mat22(ipgf)*ny(ipgf)-mat23(ipgf)*nz(ipgf))**2 +(vectz(&
        ipgf)-mat13(ipgf)*nx(ipgf) -mat23(ipgf)*ny(ipgf)-mat33(ipgf)*&
        nz(ipgf))**2) *poidsf(ipgf)*jac(ipgf)
!
    10 end do
!
end subroutine
