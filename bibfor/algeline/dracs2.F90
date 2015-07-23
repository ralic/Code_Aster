subroutine dracs2(a0, b0, c0, d0, e0,&
                  f0, nbroot, x, y)
!
      implicit none
      real(kind=8), intent(in) :: a0(2), b0(2), c0(2)
      real(kind=8), intent(in) :: d0(2), e0(2), f0(2)
      integer, intent(out) :: nbroot
      real(kind=8), intent(out) :: x(4), y(4)
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
!     EVALUE LES RACINES D UN SYSTEME DE DEUX EQUATIONS POLYNOMIALES
!     DE DEGRE 2 A 2 VARIABLES (SYSTEME DE 2 FORMES QUADRATIQUES)
!     GEOMETRIQUEMENT : INTERSECTION DE DEUX CONIQUES
!
!     ON UTILISE LA GEOMETRIE PROJECTIVE
!
!     A(1) + B(1)*x + C(1)*y + D(1)*x*y + E(1)*x*x + F(1)*y*y = 0
!     A(2) + B(2)*x + C(2)*y + D(2)*x*y + E(2)*x*x + F(2)*y*y = 0
!
! IN  A0 : PARAMETRES DU SYSTEME
! IN  B0 : PARAMETRES DU SYSTEME
! IN  C0 : PARAMETRES DU SYSTEME
! IN  D0 : PARAMETRES DU SYSTEME
! IN  E0 : PARAMETRES DU SYSTEME
! IN  F0 : PARAMETRES DU SYSTEME
!
! OUT NBROOT : NOMBRE DE COUPLES SOLUTIONS
! OUT X ET Y : COUPLES SOLUTIONS
!
#include "asterc/r8prem.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/decompose_conic.h"
#include "asterfort/det_mat.h"
#include "asterfort/find_Cardan_roots.h"
#include "asterfort/intersect_conic.h"
#include "asterfort/mat_inv.h"
#include "asterfort/num_rank_mat33.h"
#include "asterfort/trace_mat.h"
      integer :: i, rank_m1, rank_m2, nroots, nline
      integer :: i_other, nb1, nb2
      real(kind=8) :: ref_m1, ref_m2, m0(3,3), m1(3,3), m2(3,3)
      real(kind=8) :: prec, min_m2(3,3), inv_min_m2(3,3)
      real(kind=8) :: matrice(3,3), coef(4), roots(3)
      real(kind=8) :: line1(3), line2(3), other_conic(3,3)
      real(kind=8) :: mat_swap(3,3), indic_1, indic_2, indic_min
      real(kind=8) :: degener_conic(3,3), point1(3), point2(3)
      real(kind=8) :: line1_swap(3), line2_swap(3), indic
      integer :: nline_swap
!
!
!     On construit les matrices m1 et m2 representant les coniques
      ref_m1 = max(abs(a0(1)),abs(b0(1)),abs(c0(1)),&
                   abs(d0(1)),abs(e0(1)),abs(f0(1)))
      if(ref_m1.eq.0.d0) then
          ASSERT(.false.)
      endif
      m1(1,1) = e0(1)/ref_m1
      m1(1,2) = d0(1)/ref_m1/2.d0
      m1(1,3) = b0(1)/ref_m1/2.d0
      m1(2,1) = d0(1)/ref_m1/2.d0
      m1(2,2) = f0(1)/ref_m1
      m1(2,3) = c0(1)/ref_m1/2.d0
      m1(3,1) = b0(1)/ref_m1/2.d0
      m1(3,2) = c0(1)/ref_m1/2.d0
      m1(3,3) = a0(1)/ref_m1
  !
      ref_m2 = max(abs(a0(2)),abs(b0(2)),abs(c0(2)),&
                   abs(d0(2)),abs(e0(2)),abs(f0(2)))
      if(ref_m2.eq.0.d0) then
          ASSERT(.false.)
      endif
      m2(1,1) = e0(2)/ref_m2
      m2(1,2) = d0(2)/ref_m2/2.d0
      m2(1,3) = b0(2)/ref_m2/2.d0
      m2(2,1) = d0(2)/ref_m2/2.d0
      m2(2,2) = f0(2)/ref_m2
      m2(2,3) = c0(2)/ref_m2/2.d0
      m2(3,1) = b0(2)/ref_m2/2.d0
      m2(3,2) = c0(2)/ref_m2/2.d0
      m2(3,3) = a0(2)/ref_m2
  !
  !   On regarde le rang des matrices
      prec = 1.d-13
      rank_m1 = num_rank_mat33(m1, prec, indic_1)
      rank_m2 = num_rank_mat33(m2, prec, indic_2)
  !
  !   m2 = la mieux conditionnee
      if(rank_m1.eq.rank_m2) then
  !
          if(indic_1.lt.indic_2) then
              mat_swap = m1
              m1 = m2
              m2 = mat_swap
          endif
  !
      endif
  !
      if(rank_m1.eq.3.and.rank_m2.eq.3) then
  !
  !       On cherche une conique degeneree dans le pencil forme
  !           par ces deux coniques
  !       Pencil = combinaison lineaires a*M1 + b*M2 où a et b sont des réels
  !       On utilise le polynome caracteristique :
  !       on cherche un reel "root" tel que det(M1+root*M2)=0
  !
  !       On inverse la mieux conditionnee : m2
  !
          min_m2 = -m2
          inv_min_m2 = mat_inv(3,min_m2)
          matrice = matmul(m1,inv_min_m2)
  !
  !       coefficients polynome caracteristique (degre 3)
  !       on commence par le plus haut degre
          coef(1) = -1.d0
          coef(2) = trace_mat(3,matrice)
          coef(3) = -(det_mat(2,matrice(1:2,1:2)) +&
                      det_mat(2,matrice(2:3,2:3)) +&
                      det_mat(2,matrice((/1,3/),(/1,3/))))
          coef(4) = det_mat(3,matrice)
  !
  !       on va chercher les racines reelles de ce polynome
          call find_Cardan_roots(coef, roots, nroots)
  !
          if(nroots.gt.0) then
              !
              ! la meilleure configuration = la plus "secante"
              indic_min = r8maem()
              do i=1, nroots
                  m0(:,:) = m1(:,:) + roots(i)*m2(:,:)
                  !
                  ! lignes secantes dont est composee la conique degeneree
                  call decompose_conic(m0, nline_swap, line1_swap,&
                                       line2_swap, indic=indic)
                  if(nline_swap.eq.2) then
                      nline = 2
                      if(indic.lt.indic_min) then
                          line1 = line1_swap
                          line2 = line2_swap
                          indic_min = indic
                      endif
                  endif
              end do
          else
              ! normalement, cette situation ne doit pas arriver!!
              ! un polynome d ordre trois a tjs au moins une racine reelle
              nline = 0
          endif
          !
          ! intersection cherchee avec la mieux conditionnee
          other_conic = m2
  !
      else
           ! une des deux coniques est degeneree a la base
           ! on peut l utiliser directement pour trouver l intersection
           ! On decompose la plus degeneree
          if(rank_m1.lt.3) then
              degener_conic = m1
              other_conic = m2
              i_other = 2
          else
              degener_conic = m2
              other_conic = m1
              i_other = 1
          endif
          !
          call decompose_conic(degener_conic, nline, line1, line2, indic=indic)
          if(nline.eq.0) then
              if((i_other.eq.1.and.rank_m1.lt.3).or.&
                 (i_other.eq.2.and.rank_m2.lt.3)) then
                  call decompose_conic(other_conic, nline, line1, line2, indic=indic)
                  other_conic = degener_conic
              endif
          endif
      endif
  !
  !   L intersection de la conique degeneree est la meme que celle avec la conique
  !       initiale, car elles appartiennent au meme pencil!
  !   Or, on sait faire une intersection conique/droite
      nbroot = 0
  !
      if(nline.gt.0) then
  !
          call intersect_conic(other_conic,line1,nb1,point1,point2)
  !
  !       on enregistre les solutions en repassant des coordonnées
  !       homogenes aux coordonnees normales

          if(nb1.gt.0) then
              ! faut il mettre une tolerance plutot que 0 ici?
              ! a instruire
              ! on ne veut pas un point presque a l infini
              ! ms comment definir cette notion??
              if(abs(point1(3)).gt.0.d0) then
                  nbroot = nbroot + 1
                  x(nbroot) = point1(1)/point1(3)
                  y(nbroot) = point1(2)/point1(3)
               endif
          endif
  !
          if(nb1.gt.1) then
              if(abs(point2(3)).gt.0.d0) then
                  nbroot = nbroot + 1
                  x(nbroot) = point2(1)/point2(3)
                  y(nbroot) = point2(2)/point2(3)
              endif
          endif
  !
          call intersect_conic(other_conic,line2,nb2,point1,point2)
  !
          if(nb2.gt.0) then
              if(abs(point1(3)).gt.0.d0) then
                  nbroot = nbroot + 1
                  x(nbroot) = point1(1)/point1(3)
                  y(nbroot) = point1(2)/point1(3)
              endif
          endif
  !
          if(nb2.gt.1) then
              if(abs(point2(3)).gt.0.d0) then
                  nbroot = nbroot + 1
                  x(nbroot) = point2(1)/point2(3)
                  y(nbroot) = point2(2)/point2(3)
              endif
          endif
  !
      endif

end subroutine
