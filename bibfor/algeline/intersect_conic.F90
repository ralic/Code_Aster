subroutine intersect_conic(m, line, nb, point1, point2)
!
      implicit none
#include "asterc/r8prem.h"
#include "asterfort/get_line_points.h"
!
      real(kind=8), intent(in) :: m(3,3)
      real(kind=8), intent(in) :: line(3)
      integer, intent(out) :: nb
      real(kind=8), intent(out) :: point1(3)
      real(kind=8), intent(out) :: point2(3)
!
!-----------------------------------------------------------------------
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
!======================================================================
!
!     INTERSECTION D UNE CONIQUE AVEC UNE DROITE
!     ECRIT EN GEOMETRIE PROJECTIVE
!
! IN  M  : MATRICE DE LA CONIQUE
! IN LINE : DROITE EN COORDONNEES HOMOGENES
! OUT NB : NOMBRE DE POINTS D INTERSECTION
! OUT POINT1, POINT2 : POINT D INTERSECTIONS 
!
      real(kind=8) :: coefA, coefB, coefC, pt1(3), pt2(3)
      real(kind=8) :: prec, mult1(3), mult2(3), ref_coef
      real(kind=8) :: rac, rac1, rac2, delta, ref_delta, prec2
      integer :: i, j
!
      call get_line_points(line, pt1, pt2)
!
      point1 = (/ 0.d0, 0.d0, 0.d0/)
      point2 = (/ 0.d0, 0.d0, 0.d0/)
!
      prec = 1.d-13
!
!     construction equation ordre 2 a resoudre
!     A X*X + B*X + C = 0
      coefA = 0.d0
      do i=1,3
         mult1(i) = 0.d0
         mult2(i) = 0.d0
         do j=1,3
             mult1(i) = mult1(i) + m(i,j)*pt1(j)
             mult2(i) = mult2(i) + m(i,j)*pt2(j)
         end do
      end do
!
      coefA = dot_product(pt2,mult2)
      coefB = dot_product(pt1,mult2) + dot_product(pt2,mult1)
      coefC = dot_product(pt1,mult1)
!
      ref_coef = max(abs(coefA),abs(coefB),abs(coefC))
!
!     on pourrait se poser la question de mettre une tolerance
!     tres serree pour ces tests
      prec2 = 1.d-13
      if(abs(coefA).lt.prec2*ref_coef) then
          if(abs(coefB).lt.prec2*ref_coef) then
              nb = 0
              goto 99
          else
              nb = 1
              rac = -coefC/coefB
              point1(:) = pt1(:) + rac*pt2(:)
          endif
      else
          delta = coefB*coefB-4.d0*coefA*coefC
          !
          ! ordre de grandeur pour delta
          ref_delta = max(coefB*coefB,4.d0*coefA*coefC)
          !
          if(delta.ge.prec*ref_delta) then
              nb = 2
              rac1 = (-coefB-sqrt(delta))/(2.d0*coefA)
              point1(:) = pt1(:) + rac1*pt2(:)
              rac2 = (-coefB+sqrt(delta))/(2.d0*coefA)
              point2(:) = pt1(:) + rac2*pt2(:)
          else if(abs(delta).lt.prec*ref_delta) then
              nb = 1
              rac = -coefB/(2.d0*coefA)
              point1(:) = pt1(:) + rac*pt2(:)
          else
              nb = 0
          endif
      endif
99    continue
!
end subroutine
