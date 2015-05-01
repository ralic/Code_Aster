subroutine dxsiro(ne, t2iu, tensav, tensap)
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
    implicit none
#include "asterfort/utbtab.h"
    integer, intent(in) :: ne
    real(kind=8), intent(in) :: t2iu(2, 2)
    real(kind=8), intent(in) :: tensav(*)
    real(kind=8), intent(out) :: tensap(*)
!     ------------------------------------------------------------------
!     PASSAGE DES CONTRAINTES (OU DEFORMATIONS) DU REPERE INTRINSEQUE DE
!     L'ELEMENT AU REPERE UTILISATEUR (OU L'INVERSE)
!     ------------------------------------------------------------------
!     IN  NE    I      NOMBRE DE POINTS (OU SOUS-POINTS) A TRAITER
!     IN  T2IU  R  2,2  MATRICE DE PASSAGE (OBTENUE PAR COQREP) :
!                       T2IU : INTRINSEQUE -> UTILISATEUR
!                       T2UI : UTILISATEUR -> INTRINSEQUE
!     IN  TENSAV R    *   XX  YY  ZZ  XY  XZ  YZ
!     OUT TENSAP R    *   XX  YY  ZZ  XY  XZ  YZ
!
!  REMARQUE : ON PEUT APPELER CETTE ROUTINE AVEC UN TABLEAU EN IN/OUT
!     ------------------------------------------------------------------
    real(kind=8) :: sigmav(2, 2), sigmap(2, 2), tampon(2)
    real(kind=8) :: xab(2, 2)
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
!
    do i = 1, ne
        sigmav(1,1) = tensav(1+6*(i-1))
        sigmav(2,1) = tensav(4+6*(i-1))
        sigmav(1,2) = tensav(4+6*(i-1))
        sigmav(2,2) = tensav(2+6*(i-1))
!
        call utbtab('ZERO', 2, 2, sigmav, t2iu,&
                    xab, sigmap)
!
        tensap(1+6*(i-1)) = sigmap(1,1)
        tensap(2+6*(i-1)) = sigmap(2,2)
        tensap(3+6*(i-1)) = tensav(3+6*(i-1))
        tensap(4+6*(i-1)) = sigmap(2,1)
!
        tampon(1)=tensav(5+6*(i-1))
        tampon(2)=tensav(6+6*(i-1))
        tensap(5+6*(i-1)) = tampon(1) * t2iu(1,1) + tampon(2) * t2iu( 2,1)
        tensap(6+6*(i-1)) = tampon(1) * t2iu(1,2) + tampon(2) * t2iu( 2,2)
!
    enddo
!
end subroutine
