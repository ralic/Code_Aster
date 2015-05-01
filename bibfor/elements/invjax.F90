subroutine invjax(stop, nno, ndim, nderiv, dff,&
                  coor, invjac, ipb)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=W1306
    implicit   none
#include "asterfort/assert.h"
#include "asterfort/matinv.h"
    integer :: nno, ndim, nderiv, ipb
    real(kind=8) :: coor(ndim*nno)
    real(kind=8) :: dff(3, nno), invjac(3, 3), inv(ndim, ndim)
    character(len=1) :: stop
!
! ----------------------------------------------------------------------
!
! CALCUL DE L'INVERSE DE LA JACOBIENNE EN XE
!             ***           ***           *
!
! ----------------------------------------------------------------------
!
! IN  STOP   : /'S' : ON S'ARRETE EN ERREUR <F> EN CAS D'ECHEC
!              /'C' : ON CONTINUE EN CAS D'ECHEC (IPB=1)
! IN  NNO    : NOMBRE DE NOEUDS DE L'ELT
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  DFF    : DÉRIVÉES DES FONCTION DES FORMES AU POINT XE
! IN  COOR   : COORDONNEES DES NOEUDS DE L'ÉLÉMENT
! OUT INVJAC : INVERSE DE LA JACOBIENNE AU POINT XE
! OUT IPB    : =0 SI TOUT S'EST BIEN PASSE
!              =1 SI MATRICE SINGULIERE
!
! ----------------------------------------------------------------------
!
    integer :: idim, jdim, ino, i, j
    real(kind=8) :: jacobi(ndim, ndim), det
!
! ----------------------------------------------------------------------
!
    ipb = 0
!
! --- JACOBIENNE EN XE
!
    do jdim = 1, nderiv
        do idim = 1, ndim
            jacobi(idim,jdim) = dff(jdim,1) * coor(idim)
        end do
    end do
!
    do ino = 2, nno
        do jdim = 1, nderiv
            do idim = 1, ndim
                jacobi(idim,jdim) = jacobi(idim,jdim) + dff(jdim,ino) * coor(ndim*(ino-1)+idim)
            end do
        end do
    end do
    if (ndim .ne. nderiv) then
        ASSERT(ndim.eq.nderiv+1)
        if (nderiv .eq. 1) then
            jacobi(1,2) = -1*jacobi(2,1)
            jacobi(2,2) = jacobi(1,1)
        else if (nderiv.eq.2) then
            jacobi(1,3) = jacobi(2,1)*jacobi(3,2)-jacobi(3,1)*jacobi( 2,2)
            jacobi(2,3) = jacobi(3,1)*jacobi(1,2)-jacobi(1,1)*jacobi( 3,2)
            jacobi(3,3) = jacobi(1,1)*jacobi(2,2)-jacobi(2,1)*jacobi( 1,2)
        endif
    endif
!
! --- INVERSE DE LA JACOBIENNE
!
    call matinv(stop, ndim, jacobi, inv, det)
    if (det .eq. 0.d0) ipb = 1
!
    do i = 1, 3
        do j = 1, 3
            invjac(i,j)=0.d0
        end do
    end do
    do i = 1, ndim
        do j = 1, ndim
            invjac(i,j)=inv(i,j)
        end do
    end do
!
end subroutine
