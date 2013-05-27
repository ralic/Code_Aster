subroutine mulmat(k, n, m, a, b,&
                  c)
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
    real(kind=8) :: a(k, n), b(n, m), c(k, m)
!-----------------------------------------------------------------------
    integer :: ia, ib, k, k1, m, m1, n
    integer :: n1
!-----------------------------------------------------------------------
    do 20 ib = 1, m
        do 10 ia = 1, k
            c(ia,ib) = 0.d0
10      continue
20  end do
    do 50 k1 = 1, k
        do 40 n1 = 1, n
            do 30 m1 = 1, m
                c(k1,m1) = c(k1,m1) + a(k1,n1)*b(n1,m1)
30          continue
40      continue
50  end do
end subroutine
