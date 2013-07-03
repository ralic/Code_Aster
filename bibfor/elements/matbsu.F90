subroutine matbsu(nb1, xr, npgsr, intsn, b1mnc,&
                  b2mnc, b1mni, b2mni, b1mri, b2mri,&
                  b1src, b2src, b1su, b2su)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
!
#include "asterfort/r8inir.h"
    integer :: nb1
!
    integer :: intsn
    integer :: npgsr
!
    integer :: i, j, k, l
    integer :: i1
!
    real(kind=8) :: xr ( * )
!
    real(kind=8) :: b1su ( 5 , 51 )
    real(kind=8) :: b2su ( 5 , 51 )
!
    real(kind=8) :: b1mnc ( 3 , 51 )
    real(kind=8) :: b2mnc ( 3 , 51 )
!
    real(kind=8) :: b1mni ( 3 , 51 )
    real(kind=8) :: b2mni ( 3 , 51 )
!
    real(kind=8) :: b1mri ( 3 , 51 , 4 )
    real(kind=8) :: b2mri ( 3 , 51 , 4 )
!
!
    real(kind=8) :: b1src ( 2 , 51 , 4 )
    real(kind=8) :: b2src ( 2 , 51 , 4 )
!
!
!DEB
!
!---- INITIALISATION
!
    call r8inir(5 * 51, 0.d0, b1su, 1)
!
    call r8inir(5 * 51, 0.d0, b2su, 1)
!
!---- ADRESSES POUR EXTRAPOLATION DES B
!
    l = 702
!
    i1 = l + 4 * ( intsn - 1 )
!
!---- REMPLISSAGE
!
    do 100 j = 1, 6 * nb1 + 3
        do 110 i = 1, 3
!
!---------- OPERATEURS DE FLEXION
!
            b1su ( i , j ) = b1mnc ( i , j ) - b1mni ( i , j )
            b2su ( i , j ) = b2mnc ( i , j ) - b2mni ( i , j )
!
!---------- EXTRAPOLATION
!
            do 120 k = 1, npgsr
!
!---------- OPERATEURS DE MEMBRANE
!
                b1su ( i , j ) = b1su ( i , j ) + xr ( i1 + k ) *&
                b1mri ( i , j , k )
!
                b2su ( i , j ) = b2su ( i , j ) + xr ( i1 + k ) *&
                b2mri ( i , j , k )
!
                if (i .le. 2) then
!
!------------------- OPERATEURS DE SHEAR
!
                    b1su ( i + 3 , j ) = b1su ( i + 3 , j ) + xr ( i1&
                    + k ) * b1src ( i , j , k )
!
                    b2su ( i + 3 , j ) = b2su ( i + 3 , j ) + xr ( i1&
                    + k ) * b2src ( i , j , k )
!
                endif
!
120          continue
110      continue
100  end do
!
!
!
!
!FIN
!
end subroutine
