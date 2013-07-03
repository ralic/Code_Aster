subroutine asbgl2(bglob, b)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! Transformer B(3,20) dans global en BGLOB(6,60) en global
!
    implicit none
#include "asterfort/r8inir.h"
    real(kind=8) :: bglob(6, 60), b(3, 20)
!
!-----------------------------------------------------------------------
    integer :: j
!-----------------------------------------------------------------------
    call r8inir(360, 0.d0, bglob, 1)
!
! Premiere ligne
!
    do 10 j = 1, 20
        bglob(1,j)=b(1,j)
10  end do
!
! Deuxieme ligne
!
    do 20 j = 21, 40
        bglob(2,j)=b(2,j-20)
20  end do
!
! Troisieme ligne
!
    do 30 j = 41, 60
        bglob(3,j)=b(3,j-40)
30  end do
!
! Quatrieme ligne
!
    do 40 j = 1, 20
        bglob(4,j)=b(2,j)
40  end do
!
    do 50 j = 21, 40
        bglob(4,j)=b(1,j-20)
50  end do
!
! Cinquieme ligne
!
    do 60 j = 21, 40
        bglob(5,j)=b(3,j-20)
60  end do
!
    do 70 j = 41, 60
        bglob(5,j)=b(2,j-40)
70  end do
!
! Sixieme ligne
!
    do 80 j = 1, 20
        bglob(6,j)=b(3,j)
80  end do
!
    do 90 j = 41, 60
        bglob(6,j)=b(1,j-40)
90  end do
!
end subroutine
