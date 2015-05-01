subroutine sspmvc(n, m, mat, ad, t1,&
                  y)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     VERSION COMPLEXE DE SSPMVA
    integer :: n, m, ad(*)
    complex(kind=8) :: mat(*), t1(*), y(*)
    integer :: i, j, jmin, jmax, rest, k0, k1, k2, k3, k4, k5, k6, k7
    rest=m - (m/8)* 8
    if (rest .le. 3) then
!
        if (rest .le. 1) then
!
            if (rest .eq. 1) then
                k1 = ad(1)
                do 1 i = 1, n
                    y(i) = (y(i) ) - t1(1)*mat(k1)
                    k1 = k1 +1
 1              continue
            endif
        else
            if (rest .eq. 2) then
                k1 = ad(1)
                k2 = ad(2)
                do 2 i = 1, n
                    y(i) = ( (y(i) ) - t1(1)*mat(k1) )- t1(2)*mat(k2)
                    k1 = k1 +1
                    k2 = k2 +1
 2              continue
            else
                k1 = ad(1)
                k2 = ad(2)
                k3 = ad(3)
                do 3 i = 1, n
                    y(i) = ( ((y(i) ) - t1(1)*mat(k1)) - t1(2)*mat(k2)) - t1(3 )*mat(k3 )
                    k1 = k1 +1
                    k2 = k2 +1
                    k3 = k3 +1
 3              continue
            endif
        endif
    else
        if (rest .le. 5) then
!
            if (rest .eq. 4) then
                k1 = ad(1)
                k2 = ad(2)
                k3 = ad(3)
                k4 = ad(4)
                do 4 i = 1, n
                    y(i) = (&
                           ((( y(i) ) - t1(1)*mat(k1)) - t1(2)*mat(k2) ) - t1(3)*mat(k3 )) - t1(4&
                           )*mat(k4&
                           )
                    k1 = k1 +1
                    k2 = k2 +1
                    k3 = k3 +1
                    k4 = k4 +1
 4              continue
            else
                k1 = ad(1)
                k2 = ad(2)
                k3 = ad(3)
                k4 = ad(4)
                k5 = ad(5)
                do 5 i = 1, n
                    y(i) = (&
                           (&
                           (&
                           (&
                           (y(i) ) - t1(1)*mat(k1)) - t1(2)*mat( k2)) - t1(3)*mat(k3)) - t1(4&
                           )*mat(k4&
                           )&
                           ) - t1(5&
                           )* mat(k5&
                           )
                    k1 = k1 +1
                    k2 = k2 +1
                    k3 = k3 +1
                    k4 = k4 +1
                    k5 = k5 +1
 5              continue
            endif
        else
            if (rest .eq. 6) then
                k1 = ad(1)
                k2 = ad(2)
                k3 = ad(3)
                k4 = ad(4)
                k5 = ad(5)
                k6 = ad(6)
                do 6 i = 1, n
                    y(i) = (&
                           (&
                           (&
                           (&
                           (&
                           (&
                           y(i) ) - t1(1)*mat(k1)) - t1(2)*mat( k2)) - t1(3)*mat(k3)) - t1(4)*mat&
                           &(k4)&
                           ) - t1(5&
                           )* mat(k5&
                           )&
                           ) - t1(6&
                           )*mat(k6&
                           )
                    k1 = k1 +1
                    k2 = k2 +1
                    k3 = k3 +1
                    k4 = k4 +1
                    k5 = k5 +1
                    k6 = k6 +1
 6              continue
!
            else
                k1 = ad(1)
                k2 = ad(2)
                k3 = ad(3)
                k4 = ad(4)
                k5 = ad(5)
                k6 = ad(6)
                k7 = ad(7)
                do 7 i = 1, n
                    y(i) = (&
                           (&
                           (&
                           (&
                           (&
                           (&
                           (&
                           y(i) ) - t1(1)*mat(k1)) - t1(2)*mat( k2)) - t1(3)*mat(k3)) - t1(4)*mat&
                           &(k4)) - t1(5)* mat(k5&
                           )&
                           ) - t1(6&
                           )*mat(k6&
                           )&
                           ) - t1(7&
                           )*mat(k7&
                           )
                    k1 = k1 +1
                    k2 = k2 +1
                    k3 = k3 +1
                    k4 = k4 +1
                    k5 = k5 +1
                    k6 = k6 +1
                    k7 = k7 +1
 7              continue
            endif
        endif
    endif
    jmin= rest+8
    jmax = m
    if (jmax .ge. jmin) then
        do 100 j = jmin, jmax, 8
            k0 = ad(j)
            k1 = ad(j-1)
            k2 = ad(j-2)
            k3 = ad(j-3)
            k4 = ad(j-4)
            k5 = ad(j-5)
            k6 = ad(j-6)
            k7 = ad(j-7)
            do 8 i = 1, n
                y(i) = (&
                       (&
                       (&
                       (&
                       (&
                       (&
                       (&
                       (&
                       y(i) ) - t1(j)* mat(k0)) - t1(j-1)* mat(k1)) - t1(j-2)*mat(k2)) - t1(j-3)*&
                       &mat(k3)) - t1(j-4)*mat(k4)) - t1(j-5&
                       )*mat(k5&
                       )&
                       ) - t1(j-6&
                       )*mat( k6&
                       )&
                       ) - t1(j-7&
                       )*mat(k7&
                       )
                k0 = k0 +1
                k1 = k1 +1
                k2 = k2 +1
                k3 = k3 +1
                k4 = k4 +1
                k5 = k5 +1
                k6 = k6 +1
                k7 = k7 +1
 8          continue
!
100      end do
    endif
end subroutine
