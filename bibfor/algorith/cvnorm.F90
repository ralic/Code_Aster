subroutine cvnorm(mat, vect, ndim, iretou)
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
!
!***********************************************************************
!    B. GUIGON    P. RICHARD                   DATE 06/04/92
!-----------------------------------------------------------------------
!  BUT:  < NORME VECTEUR >
!
!   CETTE ROUTINE NORME UN VECTEUR COMPLEXE PAR RAPPORT A UNE MATRICE
!   COMPLEXE
!
!-----------------------------------------------------------------------
!
! MAT      /I/: MATRICE COMPLEXE DEFINISSANT LE PRODUIT SCALAIRE
! VECT     /M/: VECTEUR COMPLEXE A NORMER
! NDIM     /I/: DIMENSION DU VECTEUR ET DE LA MATRICE
! IRETOU
!-----------------------------------------------------------------------
!
    include 'asterfort/sesqui.h'
    integer :: ndim
    complex(kind=8) :: mat(*), vect(ndim)
    real(kind=8) :: zero
    integer :: i, iretou
    complex(kind=8) :: normec
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    data zero /0.d0/
!
!-----------------------------------------------------------------------
!
    call sesqui(mat, vect, ndim, normec)
    iretou = 0
    if (abs(normec) .eq. zero) then
        iretou = 1
        goto 9999
    endif
    normec=dcmplx(sqrt(abs(dble(normec))),0.d0)
    do 10 i = 1, ndim
        vect(i)=vect(i)/normec
10  end do
9999  continue
end subroutine
