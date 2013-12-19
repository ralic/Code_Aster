subroutine tpsivp(p, sigmav)
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
#include "asterfort/assert.h"
#include "blas/dgemm.h"
#include "blas/dsymm.h"
    real(kind=8), dimension(:, :), intent(in) :: p
    real(kind=8), dimension(:), intent(inout) :: sigmav
!     ------------------------------------------------------------------
!     Appliquer un Changement de Base à un tenseur Sigma (contraintes ou 
!                                                         déformation) 
!     SIGMA <- P^T * SIGMA * P        
!     ------------------------------------------------------------------
!     IN     P(3,3)     R   Matrice de passage de la base 1 à la base 2 
!     INOUT  SIGMAV(6)  R   Tenseur de contraintes ou de déformations
!                           6 composantes : XX  YY  ZZ  XY  XZ  YZ 
!                           en entrée: sigmav est exprimé dans la base 1
!                           en sortie: sigmav est exprimé dans la base 2
    !
!     Routine in place  
!     ------------------------------------------------------------------
    integer :: ld
    real(kind=8) :: alpha, beta
    real(kind=8), dimension(3, 3) :: sigma, temp
!     ------------------------------------------------------------------
    ASSERT(size(p,1)==3)
!      ASSERT(size(sigmav)==6)
!     On décompacte le tenseur pour le stocker comme une matrice pleine symétrique 'L'  
    sigma(:,:) = 0.d0
    sigma(1,1) = sigmav(1)
    sigma(2,2) = sigmav(2)
    sigma(3,3) = sigmav(3)
    sigma(2,1) = sigmav(4)
    sigma(3,1) = sigmav(5)
    sigma(3,2) = sigmav(6)
    !
! temp = sigma*P
    ld=3
    alpha = 1.d0
    beta = 0.d0
    call dsymm('L', 'L', ld, ld, alpha,&
               sigma, ld, p, ld, beta,&
               temp, ld)
! sigma <- P^T*temp
    call dgemm('T', 'N', 3, 3, 3,&
               alpha, p, 3, temp, 3,&
               beta, sigma(1, 1), 3)
! On re-compacte le tenseur
!xx
    sigmav(1) = sigma(1,1)
! yy
    sigmav(2) = sigma(2,2)
! zz
    sigmav(3) = sigma(3,3)
! xy
    sigmav(4) = sigma(2,1)
! xz
    sigmav(5) = sigma(3,1)
! yz
    sigmav(6) = sigma(3,2)
    !
end subroutine tpsivp
