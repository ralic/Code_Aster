subroutine shakst(kstab, k11, k22, k33, k12,&
                  k21, k13, k23, k31, k32)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit none
    include 'asterfort/r8inir.h'
    real(kind=8) :: kstab(24, 24), k11(8, 8), k22(8, 8), k33(8, 8), k12(8, 8)
    real(kind=8) :: k21(8, 8), k13(8, 8), k23(8, 8), k31(8, 8), k32(8, 8)
    integer :: i, j
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! ASSEMBLAGE DE LA MATRICE DE STABILISATION
! AVEC LES 3 MATRICES 8*8 K11 K22 ET K33
!
!      CALL ZDANUL(KSTAB,576)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call r8inir(576, 0.d0, kstab, 1)
    do 20 i = 1, 8
        do 10 j = 1, 8
            kstab(i,j) = k11(i,j)
10      continue
20  end do
    do 40 i = 9, 16
        do 30 j = 9, 16
            kstab(i,j) = k22(i-8,j-8)
30      continue
40  end do
    do 60 i = 17, 24
        do 50 j = 17, 24
            kstab(i,j) = k33(i-16,j-16)
50      continue
60  end do
    do 80 i = 1, 8
        do 70 j = 9, 16
            kstab(i,j) = k12(i,j-8)
70      continue
80  end do
    do 100 i = 9, 16
        do 90 j = 1, 8
            kstab(i,j) = k21(i-8,j)
90      continue
100  end do
    do 120 i = 1, 8
        do 110 j = 17, 24
            kstab(i,j) = k13(i,j-16)
110      continue
120  end do
    do 140 i = 9, 16
        do 130 j = 17, 24
            kstab(i,j) = k23(i-8,j-16)
130      continue
140  end do
    do 160 i = 17, 24
        do 150 j = 1, 8
            kstab(i,j) = k31(i-16,j)
150      continue
160  end do
    do 180 i = 17, 24
        do 170 j = 9, 16
            kstab(i,j) = k32(i-16,j-8)
170      continue
180  end do
end subroutine
