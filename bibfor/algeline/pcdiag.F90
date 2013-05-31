subroutine pcdiag(n, icpl, icpc, icpd)
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
!  CALCULE LE POINTEUT ICPD=ADRESSE DANS CA DU DERNIER COEFF
!  DE L (DIAGONALE A PART)
! aslint: disable=W1304
    implicit none
    integer(kind=4) :: icpc(*)
    integer :: icpd(n), icpl(0:n)
!
!-----------------------------------------------------------------------
    integer :: i, k, k1, k2, n
!-----------------------------------------------------------------------
    k1 = 1
    do 30 i = 1, n
        k2 = icpl(i)
        icpd(i) = k1 - 1
        do 10 k = k1, k2
            if (icpc(k) .lt. i) then
                icpd(i) = k
            else
                goto 20
            endif
10      continue
20      continue
        k1 = k2 + 1
30  end do
!
end subroutine
