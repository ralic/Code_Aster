subroutine cylrep(ndim, x, axe_z, orig, pgcyl,&
                  ipaxe)
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
!
    implicit none
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
!
    integer, intent(in) :: ndim
    real(kind=8), dimension(:), intent(in) :: x, axe_z, orig
    real(kind=8), dimension(:, :), intent(inout) :: pgcyl
    integer, intent(inout), optional :: ipaxe
!   ---------------------------------------------------------------------
!         CALCUL DE LA MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE CYLINDRIQUE
!         AU POINT X 
!   ------------------------------------------------------------------
!     IN    NDIM                I  dimension du problème (2 ou 3)     
!     IN    X(3)                R  coordonnées du point X 
!     IN    AXE_Z(3), ORIG(3)   R  axe z et origine du repère cylindrique
!     INOUT PGCYL(3,3)          R  matrice de passage du repère global 
!                                  au repère cylindrique 
!     INOUT IPAXE              I  compteur (pt sur l'axe z => ipaxe = ipaxe + 1) 
!
!     PGCYL est telle que V_{global} = PGCYL * V_{cyl}
!   -------------------------------------------------------------------
    real(kind=8) :: xnorm
    real(kind=8), dimension(3) :: axe_r, axe_t
!
    pgcyl(:,:) = 0.d0
!
!   Calcul du premier vecteur axe_r du repère cylindrique 
!
    axe_r(:) = x(:)-orig(:)
!   Pour être conforme à chrpel 
    if (ndim == 2) then
        axe_r(3) = 0.0d0
    endif
    axe_r(:) = axe_r(:)-dot_product(axe_r,axe_z)*axe_z(:)
!   Pour être conforme à chrpel 
    if (ndim == 2) then
        axe_r(3) = 0.0d0
    endif
    call normev(axe_r, xnorm)
!
    if (xnorm .lt. r8prem()) then
!   si le point x appartient à l'axe z, alors l'axe r n'est pas défini par 
!   le calcul précédent
!   on prend un axe arbitraire orthogonal 
!   à l'axe z 
        if (axe_z(1) .ne. 0.d0 .or. axe_z(2) .ne. 0.d0) then
            axe_r(1) = axe_z(2)
            axe_r(2) = -axe_z(1)
            axe_r(3) = 0.0d0
        else
!   si axe_z = ez, axe_r = ex  
            axe_r(1) = 1.0d0
            axe_r(2) = 0.0d0
            axe_r(3) = 0.0d0
        endif
        if (present(ipaxe)) then
            ipaxe = ipaxe + 1
        endif
    endif
!  Calcul du second vecteur axe_t du repère cylindrique     
!  axe_t = axe_z x axe_r
    call provec(axe_z, axe_r, axe_t)
    call normev(axe_t, xnorm)
!   pgcyl:  (e_r, e_z, e_{\theta})
    pgcyl(:,1) = axe_r
    pgcyl(:,2) = axe_z
    pgcyl(:,3) = axe_t
!  pour tests 
! pgcyl(:,1) = axe_z
! pgcyl(:,2) = axe_t
! pgcyl(:,3) = -axe_r
!
end subroutine cylrep
