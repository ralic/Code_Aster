subroutine pjefmi(elrefp, nnop, coor, xg, ndim,&
                  x1, x2, lext, xmi, distv)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "asterfort/assert.h"
#include "asterfort/elrfvf.h"
#include "asterfort/vecini.h"
    character(len=8) :: elrefp
    integer :: nnop, ndim
    real(kind=8) :: coor(ndim*nnop)
    real(kind=8) :: xg(ndim), x1(ndim), x2(ndim), xmi(ndim)
    real(kind=8), intent(out) :: distv
!
! ----------------------------------------------------------------------
! but :
!   Determiner les meilleures coordonnees barycentriques entre x1 et x2
!   permet de verifier que la routine reereg a ameliore la precision
!   des coordonnees barycentriques grossieres de la 1ere etape de
!   proj_champ.
!  Retourner la distance entre le point et son projete.
! ----------------------------------------------------------------------
!
!
! in  elrefp : type de l'element
! in  nnop   : nombre de noeuds de l'element
! in  coor   : coordonnees ds espace reel des noeuds de l'element
! in  xg     : coordonnees du point dans l'espace reel
! in  ndim   : dimension de l'espace
! in  x1     : coordonnees du point 1 dans l'espace para de l'element
!              (resultat de la projection "grossiere")
! in  x2     : coordonnees du point 2 dans l'espace para de l'element
!              (resultat du raffinement reereg.f)
! in  lext   : le point x2 est "exterieur" a la maille
! out xmi    : "x mieux" : recopie de x1 ou x2 (selon le cas)
! out distv  : distance entre le point 1 et son projete
!
! ----------------------------------------------------------------------
    integer :: nbnomx
    parameter(nbnomx=27)
    real(kind=8) :: xr1(3), xr2(3), d1, d2
    real(kind=8) :: ff(nbnomx)
    integer :: k, idim, ino, nno
    logical :: lext
! ----------------------------------------------------------------------

!     -- Si le point est exterieur, on ne tient pas compte de x2
!        => on choisit xmi=x1
!     -----------------------------------------------------------


!   -- calcul de xr1 : geometrie reelle de x1 :
!   --------------------------------------------
    call elrfvf(elrefp, x1, nbnomx, ff, nno)
    ASSERT(nno.eq.nnop)
    xr1(1:ndim)=0.d0
    do 20 idim = 1, ndim
        do 10 ino = 1, nno
            xr1(idim)=xr1(idim)+ff(ino)*coor(ndim*(ino-1)+idim)
10      continue
20  end do


!   -- calcul de xr2 : geometrie reelle de x2 :
!   --------------------------------------------
    if (.not.lext) then
        call elrfvf(elrefp, x2, nbnomx, ff, nno)
        xr2(1:ndim)=0.d0
        do 40 idim = 1, ndim
            do 30 ino = 1, nno
                xr2(idim)=xr2(idim)+ff(ino)*coor(ndim*(ino-1)+idim)
30          continue
40      end do
    endif


!   -- calcul de distv
!   -- quelle est la meilleure approximation de xg ?
!   -------------------------------------------------
    d1=0.d0
    d2=0.d0
    do 50 k = 1, ndim
        d1=d1+(xr1(k)-xg(k))**2
        if (.not.lext) d2=d2+(xr2(k)-xg(k))**2
50  end do

    if (lext) then
        xmi(1:ndim)=x1(1:ndim)
        distv=sqrt(d1)
    else
        if (d1 .le. d2) then
            xmi(1:ndim)=x1(1:ndim)
            distv=sqrt(d1)
        else
            xmi(1:ndim)=x2(1:ndim)
            distv=sqrt(d2)
        endif
    endif

end subroutine
