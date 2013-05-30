subroutine inicsu(valcen, valcev, valfac, valfav, maxfa)
    implicit none
    include 'asterc/r8maem.h'
    integer :: maxfa
    real(kind=8) :: valcen(14, 6), valcev(14, 6, maxfa)
    real(kind=8) :: valfac(maxfa, 14, 6), valfav(maxfa, 14, 6, maxfa)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001 EDF R&D WWW.CODE-ASTER.ORG
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!
! ----------------------------------------------------------------------
!
    integer :: i, j, k, l
!
! ----------------------------------------------------------------------
!
    do 10 i = 1, 14
        do 11 j = 1, 6
            valcen(i,j)= r8maem()
            do 13 l = 1, maxfa
                valcev(i,j,l)= r8maem()
13          continue
            do 12 k = 1, maxfa
                valfac(k,i,j)= r8maem()
                do 14 l = 1, maxfa
                    valfav(k,i,j,l)= r8maem()
14              continue
12          continue
11      continue
10  end do
end subroutine
