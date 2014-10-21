subroutine dfdm2b(nno, poids, dfrdk, coor, jacp, normal)
! ======================================================================
! person_in_charge: daniele.colombo at ifpen.fr
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
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/provec.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/xnormv.h"
    integer :: nno
    real(kind=8) :: dfrdk(1), coor(*)
    real(kind=8) :: jacp, poids, normal(3)
! ......................................................................
!    - BUTS:  CALCULER LA VALEUR DU POIDS D'INTEGRATION EN 1 POINT DE
!             GAUSS POUR UNE FACE PLONGEE DANS UN ELEMENT VOLUMIQUE 
!
!    - ARGUMENTS:
!        DONNEES:     NNO           -->  NOMBRE DE NOEUDS DE LA FACE
!                     POIDS         -->  POIDS DE GAUSS
!                     DFRDK         -->  DERIVEES FONCTIONS DE FORME DE LA FACE
!                     COOR          -->  COORDONNEES DES NOEUDS DE LA FACE
!
!        RESULTATS:   JACP          <--  PRODUIT DU JACOBIEN ET DU POIDS
!                     NORMAL        <--  NORMALE A LA FACETTE AU POINT DE GAUSS

!
!  REMARQUE :
!    - LA FACE N'EST PAS NECESSAIREMENT PLANE
!
    character(len=8) :: nomail
    integer :: i, j
    integer :: iadzi, iazk24
    real(kind=8) :: da(3), db(3), jac
!-----------------------------------------------------------------------
    call vecini(3, 0.d0, da)
    call vecini(3, 0.d0, db)
    do i = 1, nno
       do j = 1, 3
          da(j) = da(j) + coor(3*(i-1)+j) * dfrdk(2*i-1)
          db(j) = db(j) + coor(3*(i-1)+j) * dfrdk(2*i)
       end do
    end do
    call provec(da, db, normal)
    call xnormv(3, normal, jac)
!
    if (abs(jac) .le. 1.d0/r8gaem()) then
        call tecael(iadzi, iazk24)
        nomail= zk24(iazk24-1+3)(1:8)
        call utmess('F', 'ALGORITH2_59', sk=nomail)
    endif
!
    jacp = jac * poids
end subroutine
