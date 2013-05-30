subroutine shasbg(bglob, b, p)
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
!--------------------------------------------------------
! ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
!-------------------------------------------------------
! TRANSFORME B(6,24) DANS LOCAL EN BGLOB(6,24) DANS GLOBAL
! AVEC P(3,3) MATRICE DE PASSAGE
    implicit none
    include 'asterfort/r8inir.h'
    real(kind=8) :: bglob(6, 24), b(3, 8), p(3, 3), btmp(6, 24)
    integer :: j
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      CALL ZDANUL(BGLOB,144)
!      CALL ZDANUL(BTMP,144)
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    call r8inir(144, 0.d0, bglob, 1)
    call r8inir(144, 0.d0, btmp, 1)
!
! PREMIERE LIGNE:
    do 10 j = 1, 8
! "MULTIPLICATION AVEC X" ---> UNE PARTIE DE EPS11
        btmp(1,j) = b(1,j)*p(1,1)*p(1,1) + b(2,j)*p(1,1)*p(2,1) + b(3,j)*p(3,1)*p(1,1)
10  end do
    do 20 j = 9, 16
! "MULTIPLICATION AVEC Y" ---> UNE PARTIE DE EPS11
        btmp(1,j) = p(2,1)*b(1,j-8)*p(1,1) + p(2,1)*b(2,j-8)*p(2,1) + p(2,1)*b(3,j-8)*p(3,1)
20  end do
    do 30 j = 17, 24
! "MULTIPLICATION AVEC Z" ---> UNE PARTIE DE EPS11
        btmp(1,j) = p(3,1)*b(1,j-16)*p(1,1) + p(3,1)*b(2,j-16)*p(2,1) + p(3,1)*b(3,j-16)*p(3,1)
30  end do
! 2IEME LIGNE:
    do 40 j = 1, 8
! "MULTIPLICATION AVEC X" ---> UNE PARTIE DE EPS22
        btmp(2,j) = p(1,2)*b(1,j)*p(1,2) + p(1,2)*b(2,j)*p(2,2) + p(1,2)*b(3,j)*p(3,2)
40  end do
    do 50 j = 9, 16
! "MULTIPLICATION AVEC Y" ---> UNE PARTIE DE EPS22
        btmp(2,j) = p(2,2)*b(1,j-8)*p(1,2) + p(2,2)*b(2,j-8)*p(2,2) + p(2,2)*b(3,j-8)*p(3,2)
50  end do
    do 60 j = 17, 24
! "MULTIPLICATION AVEC Z" ---> UNE PARTIE DE EPS22
        btmp(2,j) = p(3,2)*b(1,j-16)*p(1,2) + p(3,2)*b(2,j-16)*p(2,2) + p(3,2)*b(3,j-16)*p(3,2)
60  end do
! 3IEME LIGNE:
    do 70 j = 1, 8
! "MULTIPLICATION AVEC X" ---> UNE PARTIE DE EPS33
        btmp(3,j) = p(1,3)*b(1,j)*p(1,3) + p(1,3)*b(2,j)*p(2,3) + p(1,3)*b(3,j)*p(3,3)
70  end do
    do 80 j = 9, 16
! "MULTIPLICATION AVEC Y" ---> UNE PARTIE DE EPS33
        btmp(3,j) = p(2,3)*b(1,j-8)*p(1,3) + p(2,3)*b(2,j-8)*p(2,3) + p(2,3)*b(3,j-8)*p(3,3)
80  end do
    do 90 j = 17, 24
! "MULTIPLICATION AVEC Z" ---> UNE PARTIE DE EPS33
        btmp(3,j) = p(3,3)*b(1,j-16)*p(1,3) + p(3,3)*b(2,j-16)*p(2,3) + p(3,3)*b(3,j-16)*p(3,3)
90  end do
! 4IEME LIGNE:
    do 100 j = 1, 8
! "MULTIPLICATION AVEC X" ---> UNE PARTIE DE EPS12
        btmp(4,j) = b(1,j)*p(1,1)*p(1,2)*2.d0 + b(2,j)*p(1,2)*p(2,1) + b(2,j)*p(1,1)*p(2,2) + b(3&
                    &,j)*p(1,2)*p(3,1) + b(3,j)*p(1,1)*p( 3,2)
100  end do
    do 110 j = 9, 16
! "MULTIPLICATION AVEC Y" ---> UNE PARTIE DE EPS12
        btmp(4,j) = b(1,j-8)*p(1,2)*p(2,1) + b(1,j-8)*p(1,1)*p(2,2) + b(2,j-8)*p(2,1)*p(2,2)*2.d0&
                    &+b(3,j-8)*p(2,2)*p(3,1)+ b(3,j-8)* p(2,1)*p(3,2)
110  end do
    do 120 j = 17, 24
! "MULTIPLICATION AVEC Z" ---> UNE PARTIE DE EPS12
        btmp(4,j) = b(1,j-16)*p(1,2)*p(3,1) + b(2,j-16)*p(2,2)*p(3,1) + b(1,j-16)*p(1,1)*p(3,2) +&
                    & b(2,j-16)*p(2,1)*p(3,2) + b(3,j- 16)*p(3,1)*p(3,2)*2.d0
120  end do
! 5IEME LIGNE:
    do 130 j = 1, 8
! "MULTIPLICATION AVEC X" ---> UNE PARTIE DE EPS23
        btmp(5,j) = b(1,j)*p(1,2)*p(1,3)*2.d0 + b(2,j)*p(1,3)*p(2,2) + b(2,j)*p(1,2)*p(2,3) + b(3&
                    &,j)*p(1,3)*p(3,2) + b(3,j)*p(1,2)*p( 3,3)
130  end do
    do 140 j = 9, 16
! "MULTIPLICATION AVEC Y" ---> UNE PARTIE DE EPS23
        btmp(5,j) = b(1,j-8)*p(1,3)*p(2,2) + b(1,j-8)*p(1,2)*p(2,3) + b(2,j-8)*p(2,2)*p(2,3)*2.d0&
                    & + b(3,j-8)*p(2,3)*p(3,2) + b(3,j-8)*p(2,2)*p(3,3)
140  end do
    do 150 j = 17, 24
! "MULTIPLICATION AVEC Z" ---> UNE PARTIE DE EPS23
        btmp(5,j) = b(1,j-16)*p(1,3)*p(3,2) + b(2,j-16)*p(2,3)*p(3,2) + b(1,j-16)*p(1,2)*p(3,3) +&
                    & b(2,j-16)*p(2,2)*p(3,3) + b(3,j- 16)*p(3,2)*p(3,3)*2.d0
150  end do
!
! 6IEME LIGNE:
!
    do 160 j = 1, 8
! "MULTIPLICATION AVEC X" ---> UNE PARTIE DE EPS13
        btmp(6,j) = b(1,j)*p(1,1)*p(1,3)*2.d0 + b(2,j)*p(1,3)*p(2,1) + b(2,j)*p(1,1)*p(2,3) + b(3&
                    &,j)*p(1,3)*p(3,1) + b(3,j)*p(1,1)*p( 3,3)
160  end do
    do 170 j = 9, 16
! "MULTIPLICATION AVEC Y" ---> UNE PARTIE DE EPS13
        btmp(6,j) = b(1,j-8)*p(1,3)*p(2,1) + b(1,j-8)*p(1,1)*p(2,3) + b(2,j-8)*p(2,1)*p(2,3)*2.d0&
                    & + b(3,j-8)*p(2,3)*p(3,1) + b(3,j-8)*p(2,1)*p(3,3)
170  end do
    do 180 j = 17, 24
! "MULTIPLICATION AVEC Z" ---> UNE PARTIE DE EPS13
        btmp(6,j) = b(1,j-16)*p(1,3)*p(3,1) + b(2,j-16)*p(2,3)*p(3,1) + b(1,j-16)*p(1,1)*p(3,3) +&
                    & b(2,j-16)*p(2,1)*p(3,3) + b(3,j- 16)*p(3,1)*p(3,3)*2.d0
180  end do
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    do 200 j = 1, 24
        do 190 i = 1, 6
            bglob(i,j) = btmp(i,j)
190      continue
200  end do
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
end subroutine
