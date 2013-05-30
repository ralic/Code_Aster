subroutine vfgetr(maxdim, ndim, nbnos, xs, t,&
                  xg, surf, norm, xgf, d)
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
!  CALCUL DES ELEMENTS GEOMETRIQUES D UNE FACE TRIANGULAIRE
!
! IDIM ( : 1,NDIM
! IN
!     MAXDIM
!     NDIM
!     NBNOS NBRE DE SOMMETS = NOMBRE DE ARETE
!     XS(1:MAXDIM,J) COORD SOMMET J
!     T(1:MAXDIM,2)  COORD DU VECTEUR DES DEUX PREMIRES ARETES
!     XG  COORD D UN POINT QUI PEMET D ORIENTER LA FACE
!        (XGF-XG).N >0
! OUT
!     SURF SURFACE DE LA FACE
!     NORM NORMALE
!     XGF BARYCENTRE FACE
!     D = (XGF-XG).N
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/provec.h'
    integer :: maxdim, ndim, nbnos, idim, is
    real(kind=8) :: xs(1:maxdim, nbnos), t(1:maxdim, 2)
    real(kind=8) :: xg(1:maxdim), surf, norm(1:maxdim), xgf(1:maxdim), d
    real(kind=8) :: xnorm
    call assert(ndim.eq.3)
    call assert(maxdim.ge.3)
    call assert(nbnos.eq.3)
    do 10 idim = 1, ndim
        xgf(idim)=0.d0
        do 11 is = 1, nbnos
            xgf(idim)=xgf(idim)+xs(idim,is)
11      continue
        xgf(idim)=xgf(idim)/nbnos
10  end do
!
    call provec(t(1, 1), t(1, 2), norm)
    xnorm=sqrt(norm(1)**2+norm(2)**2+norm(3)**2)
    norm(1)=norm(1)/xnorm
    norm(2)=norm(2)/xnorm
    norm(3)=norm(3)/xnorm
    surf=xnorm/2.d0
    d =(xgf(1)-xg(1))*norm(1)+&
     &   (xgf(2)-xg(2))*norm(2)+&
     &   (xgf(3)-xg(3))*norm(3)
    if (d .lt. 0.d0) then
        d=-d
        norm(1)=-norm(1)
        norm(2)=-norm(2)
        norm(3)=-norm(3)
    endif
end subroutine
