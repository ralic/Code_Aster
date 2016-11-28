subroutine coor_cyl(ndim, nnop, basloc, geom, ff,&
                    p_g, invp_g, rg, tg, l_not_zero,&
                    courb, dfdi, lcourb)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
#include "asterfort/xbasgl.h"
#include "asterfort/xcoocy.h"
#include "asterfort/provec.h"
#include "asterfort/xnormv.h"
#include "asterc/r8prem.h"
#include "asterfort/matinv.h"
!
    integer :: ndim, nnop
    real(kind=8) :: basloc(*), ff(*), geom(*)
    real(kind=8) :: p_g(ndim,ndim), invp_g(ndim,ndim), rg, tg
    aster_logical :: l_not_zero
    aster_logical, optional :: lcourb
    real(kind=8), optional :: courb(3,3,3), dfdi(:,:)
!
!
!     BUT:  CALCUL DES COORDONNEES CYLINDRIQUES EN FOND DE FISSURE
!            * MUTUALISATION DE LA DEFINITION DES BASES LOCALES AU PT DE GAUSS
!            * EN S APPUYANT SEULEMENT SUR L INFORMATION GEOMETRIQUE 
!                 SUR LA PROJECTION SUR LE FRONT DE FISSURE FOURNIE DANS BASLO
!
! IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE (3*NDIM*NNOP)
! IN  FF      : FONCTIONS DE FORMES DE L ELEMENT PARENT
! IN  GEOM    : COORDONNEES GEOMETRIQUES DES NOEUDS PARENTS
!
! OUT P_G     : MATRICE DE PASSAGE LOC > GLOB
! OUT INVP_G  : INVERSE DE LA MATRICE ORTHONORMEE
! OUT RG      : DISTANCE AU FOND
! OUT TG      : ANGLE
!
!----------------------------------------------------------------
!
    integer :: i, ino
    real(kind=8) :: baslog(3*ndim)
    real(kind=8) :: pt(ndim)
!
!----------------------------------------------------------------
!
    baslog(1:(3*ndim))=0.
    do i = 1, ndim*3
      do ino = 1, nnop
        baslog(i) = baslog(i) + basloc(3*ndim*(ino-1)+i) * ff(ino)
      end do
    end do
!
    call xbasgl(ndim, baslog, 1, p_g, invp_g)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   * SI ON DISPOSAIT DU PROJETE DU POINT DE GAUSS SUR LE FOND 
!       LE CALCUL SERAIT TRIVIAL / ON BRICOLE POUR LE MOMENT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    pt(:)= 0.d0
    do ino = 1, nnop
       do i =1,ndim
         pt(i)=pt(i)+ff(ino)*geom(ndim*(ino-1)+i)
       enddo
    end do
    call xcoocy(ndim, pt, baslog(1:ndim), p_g, rg, tg, l_not_zero)
!
    if (present(lcourb)) then
       if (lcourb) then
!          maintien de ce bloc conditionnel suite à la suppression
!          d'une routine pour couverture (issue25665)
           ASSERT(.false.)
       endif
    endif
!
end subroutine
