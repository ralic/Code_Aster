subroutine assebg(bglob, b, xff)
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
! Transformer B(3,6) dans global en BGLOB(6,18) en global
!
    implicit none
    include 'asterfort/r8inir.h'
    real(kind=8) :: bglob(6, 18), b(3, 6), xff(18)
    integer :: j
!
    call r8inir(108, 0.d0, bglob, 1)
!
! Premiere ligne
!
    do 10 j = 1, 6
        bglob(1,j)=b(1,j)*xff(1)
10  end do
!
! Deuxieme ligne
!
    do 20 j = 7, 12
        bglob(2,j)=b(2,j-6)*xff(5)
20  end do
!
! Troisieme ligne
!
    do 30 j = 13, 18
        bglob(3,j)=b(3,j-12)*xff(9)
30  end do
!
! Quatrieme ligne
!
    do 40 j = 1, 6
        bglob(4,j)=b(2,j)*xff(10)
40  end do
!
    do 50 j = 7, 12
        bglob(4,j)=b(1,j-6)*xff(11)
50  end do
!
! Cinquieme ligne
!
    do 60 j = 7, 12
        bglob(5,j)=b(3,j-6)*xff(14)
60  end do
!
    do 70 j = 13, 18
        bglob(5,j)=b(2,j-12)*xff(15)
70  end do
!
! Sixieme ligne
!
    do 80 j = 1, 6
        bglob(6,j)=b(3,j)*xff(16)
80  end do
!
    do 90 j = 13, 18
        bglob(6,j)=b(1,j-12)*xff(18)
90  end do
!
end subroutine
