subroutine vfgefa(maxdim, ndim, nbnos, xs, t,&
                  xg, surf, norm, xgf, d,&
                  iret)
    implicit none
#include "asterfort/assert.h"
#include "asterfort/provec.h"
#include "asterfort/vfgetr.h"
    integer :: maxdim, ndim, nbnos, iret
    real(kind=8) :: xs(maxdim, nbnos), t(maxdim, nbnos)
    real(kind=8) :: xg(ndim), surf, norm(maxdim), xgf(maxdim), d
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!  CALCUL DES ELEMENTS GEOMETRIQUES D UNE FACE SUPPOSEE COPLANAIRE
!
! IDIM  : 1,NDIM
! IN
!     MAXDIM
!     NDIM
!     NBNOS NBRE DE SOMMETS = NOMBRE DE ARETE
!     XS(MAXDIM,J) COORD SOMMET J
!     T(MAXDIM,J)  COORD DU VECTEUR DE L J EME ARETE
!     XG  COORD D UN POINT QUI PEMET D ORIENTER LA FACE
!        (XGF-XG).N >0
! OUT
!     SURF SURFACE DE LA FACE
!     NORM NORMALE
!     XGF BARYCENTRE FACE
!     D = (XGF-XG).N
!
! ----------------------------------------------------------------------
!
    integer :: idim, is
    real(kind=8) :: xs1(3, 3), t1(3, 2)
    real(kind=8) :: surf1, norm1(3), xgf1(3), d1
    real(kind=8) :: xs2(3, 3), t2(3, 2)
    real(kind=8) :: surf2, norm2(3), xgf2(3), d2, n1vn2(3)
    real(kind=8) :: xn1n2
!
! ----------------------------------------------------------------------
!
    ASSERT(ndim.eq.3)
    ASSERT(maxdim.ge.3)
    ASSERT((nbnos.ge.4).and.(nbnos.ge.3))
!
    if (nbnos .eq. 3) then
        call vfgetr(maxdim, ndim, nbnos, xs, t,&
                    xg, surf, norm, xgf, d)
    else
        do 10 idim = 1, ndim
            xgf(idim)=0.d0
            do 11 is = 1, nbnos
                xgf(idim)=xgf(idim)+xs(idim,is)
11          continue
            xgf(idim)=xgf(idim)/nbnos
10      continue
!
        do 20 idim = 1, ndim
            xs1(idim,1)=xs(idim,1)
            xs1(idim,2)=xs(idim,2)
            xs1(idim,3)=xs(idim,3)
            t1(idim,1)=t(idim,1)
            t1(idim,2)=t(idim,2)
20      continue
!
        call vfgetr(3, ndim, 3, xs1, t1,&
                    xg, surf1, norm1, xgf1, d1)
!
        do 30 idim = 1, ndim
            xs2(idim,1)=xs(idim,3)
            xs2(idim,2)=xs(idim,4)
            xs2(idim,3)=xs(idim,1)
            t2(idim,1)=t(idim,3)
            t2(idim,2)=t(idim,4)
30      continue
!
        call vfgetr(3, ndim, 3, xs2, t2,&
                    xg, surf2, norm2, xgf2, d2)
!
!   ON VERIFIRE QUE NORM1 ET NORM2 SONT PARALLELLES
!
        call provec(norm1, norm2, n1vn2)
        xn1n2=sqrt(n1vn2(1)**2+n1vn2(2)**2+n1vn2(3)**2)
        if (xn1n2 .gt. 1.d-6) then
            iret = 1
        else
            iret = 0
            surf = surf1+surf2
            norm(1)=norm2(1)
            norm(2)=norm2(2)
            norm(3)=norm2(3)
            d =(xgf(1)-xg(1))*norm(1)+ (xgf(2)-xg(2))*norm(2)+&
            (xgf(3)-xg(3))*norm(3)
            ASSERT(d.gt.0.d0)
        endif
    endif
end subroutine
