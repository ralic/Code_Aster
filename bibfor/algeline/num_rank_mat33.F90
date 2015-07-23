function num_rank_mat33(m, prec, indic)
!
      implicit none
#include "asterc/r8prem.h"
#include "asterfort/cubic_root.h"
#include "asterfort/det_mat.h"
#include "asterfort/norm_mat.h"
#include "asterfort/mat_com.h"
#include "asterfort/trace_mat.h"
!
      integer :: num_rank_mat33
      real(kind=8), intent(in) :: m(3,3)
      real(kind=8), intent(in) :: prec
      real(kind=8), intent(out) :: indic
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
!     RANG D UNE MATRICE DE TAILLE 3*3
!
! IN  M  : MATRICE DE TAILLE 3*3
! IN PREC : PRECISION EN DECA DE LAQUELLE ON CONSIDERE
!           QUE LA MATRICE EST SINGULIERE
! OUT INDIC : ESTIMATION DU CONDITIONNEMENT
!
      integer :: ndim
      real(kind=8) :: det_m, norm_m, com_m(3,3), sec_inv
!
      ndim = 3
      indic = 0.d0
      det_m = det_mat(ndim, m)
      norm_m = norm_mat(ndim, m)
      if(norm_m.eq.0.d0) then
          num_rank_mat33 = 0
      else if(cubic_root(abs(det_m)).le.prec*norm_m) then
          com_m = mat_com(ndim, m)
          sec_inv = trace_mat(ndim, com_m)
          if(sqrt(abs(sec_inv)).le.prec*norm_m) then
              num_rank_mat33 = 1
          else
              num_rank_mat33 = 2
          endif
      else
          num_rank_mat33 = 3
      endif
!
      if(det_m.ne.0.d0) then
          indic = norm_m/cubic_root(abs(det_m))
      endif
!
end function
