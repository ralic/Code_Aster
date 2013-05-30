subroutine predia(a, b, diag, nno)
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
    integer :: i, ic, j, nno
!-----------------------------------------------------------------------
!
!    ESTIMATEUR ZZ (2-EME VERSION 92)
!
!      PRECONDITIONNEMENT PAR LA DIAGONALE DU SYSTEME LOCAL
!
    real(kind=8) :: a(9, 9), b(9, 4), diag(9)
    do 1 i = 1, nno
        diag(i) = 1.d0/sqrt(a(i,i))
 1  continue
    do 2 i = 1, nno
        do 3 j = 1, i
            a(i,j) = a(i,j) * diag(i) *diag(j)
 3      continue
 2  end do
    do 4 ic = 1, 4
        do 5 i = 1, nno
            b(i,ic) = b(i,ic) * diag(i)
 5      continue
 4  end do
    do 6 i = 1, nno
        do 7 j = i+1, nno
            a(i,j) = a(j,i)
 7      continue
 6  end do
end subroutine
