subroutine matbmr(nb1, vectt, dudxri, intsr, jdn1ri,&
                  b1mri, b2mri)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'asterfort/hsame.h'
    include 'asterfort/promat.h'
    include 'asterfort/r8inir.h'
    integer :: i, j
!
    integer :: nb1
!
    integer :: intsr
!
    real(kind=8) :: vectt ( 3 , 3 )
!
    real(kind=8) :: dudxri ( 9 )
!
    real(kind=8) :: jdn1ri ( 9 , 51 )
!
    real(kind=8) :: b1mri ( 3 , 51 , 4 )
    real(kind=8) :: b2mri ( 3 , 51 , 4 )
!
    real(kind=8) :: tmp ( 3 , 51 )
!
    real(kind=8) :: hsm1 ( 3 , 9 )
!
    real(kind=8) :: hsm2 ( 3 , 9 )
!
!DEB
!
    call hsame(vectt, dudxri, hsm1, hsm2)
!
! --- POUR LA DEFORMATION TOTALE   B1MRI
!
    call promat(hsm1, 3, 3, 9, jdn1ri,&
                9, 9, 6 * nb1 + 3, tmp)
!
!
    do 100 j = 1, 6 * nb1 + 3
        do 110 i = 1, 3
!
            b1mri ( i , j , intsr ) = tmp ( i , j )
!
110      continue
100  end do
!
!
!---- POUR LA DEFORMATION DIFFERENTIELLE   B2MRI
!
!---- INITIALISATION
!
!
    call r8inir(3 * 51, 0.d0, tmp, 1)
!
!
!
!
    call promat(hsm2, 3, 3, 9, jdn1ri,&
                9, 9, 6 * nb1 + 3, tmp)
!
!
    do 200 j = 1, 6 * nb1 + 3
        do 210 i = 1, 3
!
            b2mri ( i , j , intsr ) = tmp ( i , j )
!
210      continue
200  end do
!
!
!FIN
!
end subroutine
