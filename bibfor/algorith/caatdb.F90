subroutine caatdb(nno, a, d, b, jac,&
                  matuu)
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
!-----------------------------------------------------------------------
!     CALCUL DE TADB POUR LE HEXA8 STABILISE
!-----------------------------------------------------------------------
    implicit none
    integer :: kk, kkd, n, i, m, j, k, kl, nno, j1
    real(kind=8) :: matuu(1)
    real(kind=8) :: d(6, 6), jac, tmp, sig(6)
    real(kind=8) :: a(6, 3, 8)
    real(kind=8) :: b(6, 3, 8)
!
    do 1 n = 1, nno
        do 2 i = 1, 3
            do 3 kl = 1, 6
                tmp = 0.d0
                do 4 k = 1, 6
                    tmp = tmp + a(k,i,n)*d(k,kl)
 4              continue
                sig(kl) = tmp
 3          continue
!
            kkd = (3* (n-1)+i-1)* (3* (n-1)+i)/2
            do 5 j = 1, 3
                do 6 m = 1, n
                    if (m .eq. n) then
                        j1 = i
                    else
                        j1 = 3
                    endif
!
                    tmp = 0.d0
                    do 7 k = 1, 6
                        tmp = tmp + sig(k)*b(k,j,m)
 7                  continue
!
!   STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
!
                    if (j .le. j1) then
                        kk = kkd + 3* (m-1) + j
                        matuu(kk) = matuu(kk) + tmp*jac
                    endif
!
 6              continue
 5          continue
 2      continue
 1  end do
!
end subroutine
