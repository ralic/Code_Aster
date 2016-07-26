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
#include "asterfort/xbasgl.h"
#include "asterfort/xcoocy.h"
#include "asterfort/provec.h"
#include "asterfort/xnormv.h"
#include "asterfort/dfdmxx.h"
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
    integer :: j, k
    real(kind=8) :: dfdgl(nnop, 3), p(3,3), invp(3,3)
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
!    e1(:)=0.d0
!    e2(:)=0.d0
!    e3(:)=0.d0
!    do ino = 1, nnop
!      do  i = 1, ndim     
!        e1(i) = basloc(3*ndim*(ino-1)+i+ndim)
!        e2(i) = basloc(3*ndim*(ino-1)+i+2*ndim)
!      end do
!      call xnormv(3, e1, norme)
!      call xnormv(3, e2, norme)
!      call provec(e1, e2, e3)
!      call xnormv(3, e3, norme)
!      call provec(e2, e3, e1)
!      do i = 1, ndim
!        baslog(i+ndim) = baslog(i+ndim) + e1(i)*ff(ino)
!        baslog(i+2*ndim) = baslog(i+2*ndim) + e2(i)*ff(ino)
!      end do
!    enddo
!    do i = ndim+1, 3*ndim
!      baslog(i)=0.d0 
!      do ino = 1, nnop
!        baslog(i) = baslog(i) + basloc(3*ndim*(ino-1)+i)
!      end do
!      baslog(i)=baslog(i)/nnop
!    end do
!
    call xbasgl(ndim, baslog, 1, p_g, invp_g)
!
!     p_g(1:ndim,1:ndim)=0.
!     invp_g(1:ndim,1:ndim)=0.
!     do ino = 1, nnop
!       call xbasgl(ndim, basloc, ino, p(1:ndim,1:ndim),&
!                   invp(1:ndim,1:ndim))
!       do i =1,ndim
!         do j=1,ndim
!           p_g(i,j)=p_g(i,j)+ff(ino)*p(i,j)
!           invp_g(i,j)=invp_g(i,j)+ff(ino)*invp(i,j)
!         enddo
!       enddo
!     enddo
!     call matinv('C', ndim, p_g, invp_g, det)
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
         ASSERT(present(courb))
         ASSERT(present(dfdi))
         ASSERT(ndim.eq.3)
         call dfdmxx(nnop, dfdi, geom, dfdgl) 
         courb=0.
         do ino = 1, nnop
           call xbasgl(3, basloc, ino, p, invp)
           do i =1,3
             do j=1,3
               do k=1,3
                 courb(i,j,k)=courb(i,j,k)+dfdgl(ino,k)*p(i,j)
               enddo
             enddo
           enddo
         enddo
!      print*,' *** KOR ***'        
!      print*,' - cour(e1)=',courb(1,1:3,1:3)    
!      print*,' - cour(e2)=',courb(2,1:3,1:3)
!      print*,' ***********'        
       endif
    endif
!
end subroutine
