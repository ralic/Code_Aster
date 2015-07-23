subroutine decompose_conic(m0, nline, line1, line2, indic)
!
      implicit none
#include "asterc/r8prem.h"
#include "asterfort/mat_com.h"
#include "asterfort/num_rank_mat33.h"
!
      real(kind=8), intent(in) :: m0(3,3)
      integer, intent(out) :: nline
      real(kind=8), intent(out) :: line1(3)
      real(kind=8), intent(out) :: line2(3)
      real(kind=8), intent(out), optional :: indic
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
!    TROUVER LES DROITES CORRESPONDANTS A UNE CONIQUE DEGENEREE
!    EN UTILISANT LA GEOMETRIE PROJECTIVE (COORDONNES HOMOGENES)
!
! IN   M0  : MATRICE TAILLE 3*3 DE LA CONIQUE DEGENEREE
! OUT  NLINE : NOMBRE DE LIGNES TROUVEES
! OUT  LINE1 : COORDONNEES HOMOGENES DE LA PREMIERE LIGNE TROUVEE
! OUT  LINE2 : COORDONNEES HOMOGENES DE LA DEUXIEME LIGNE TROUVEE
! OUT  INDIC : DISTANCE AU CENTRE DU POINT D INTERSECTION ENTRE LES LIGNES
!  PLUS IL EST PROCHE DU CENTRE, MIEUX C'EST POUR LA RESOLUTION
!  (ON VEUT EVITER LES DROITES QUASIMENT PARALLELES)
!
      real(kind=8) :: prec, mat_b(3,3), matrice(3,3)
      real(kind=8) :: max_di, b, p(3), Mp(3,3), test, val_max, condit
      integer :: rank_m0, imax, jmax, i, j
!
!     Initialisations
      line1 = (/ 0.d0, 0.d0, 0.d0 /)
      line2 = (/ 0.d0, 0.d0, 0.d0 /)
      prec = 1.e-13
      rank_m0 = num_rank_mat33(m0,prec,condit)
      if(rank_m0.lt.2) then
          matrice = m0
          ! deux droites paralleles
          ! on prend en dernier recours seulement
          indic = 1.d20
      else
          mat_b = -mat_com(3,m0)
          !
          max_di = 0.d0
          imax = 0
          !
          ! mat_b(3,3) doit etre > 0 pour lignes s intersectant
          ! en fait tous les coefficients
          do i=1,3
              if(abs(mat_b(i,i)).gt.max_di) then
                  max_di = abs(mat_b(i,i))
                  imax = i
              endif
          end do
          !
          ! Verification
          ! Comme on a pris le max, on peut se contenter de regarder par rapport a 0
          !
          ! on peut ajouter un test sur b(3,3) pour etre sur d avoir des droites secantes
          if(mat_b(imax,imax).le.0.d0.or.&
             mat_b(3,3).le.0.d0) then
              nline = 0
              goto 99
          endif
          !
          ! on peut ajouter un test sur b(3,3) pour etre sur d avoir des droites secantes
          b = sqrt(mat_b(imax,imax))
          p(:) = mat_b(:,imax)/b
          !
          ! matrice produit vectoriel
          do i=1,3
              Mp(i,i) = 0.d0
          end do
          Mp(1,2) = p(3)
          Mp(1,3) = -p(2)
          Mp(2,1) = -p(3)
          Mp(2,3) = p(1)
          Mp(3,1) = p(2)
          Mp(3,2) = -p(1)
          !
          !
          matrice = m0 + Mp
          !
          indic = sqrt((p(1)*p(1)+p(2)*p(2))/(p(3)*p(3)))
      endif
!
!     La matrice etant de rang 1
!     On recupere la valeur max de toute la matrice
      val_max = 0.d0
      imax = 0
      jmax = 0
      do  i=1,3
          do j=1,3
              test = abs(matrice(i,j))
              if(test.gt.val_max) then
                  val_max = test
                  imax = i
                  jmax = j
              endif
          end do
      end do
!
!     On construit la decomposition ligne colonne a partir de cet element max
      nline = 2
      line1(:) = matrice(imax,:)
      line2(:) = matrice(:,jmax)
99    continue
!
end subroutine
