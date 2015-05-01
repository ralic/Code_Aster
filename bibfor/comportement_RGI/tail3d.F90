subroutine tail3d(l3, t33, n33, vsige33)
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
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!=====================================================================
    implicit none
!     declaration externes
    real(kind=8) :: l3(3), t33(3, 3), n33(3, 3), vsige33(3, 3)
!     declaration locale
    real(kind=8) :: x(3), sn, a(3, 3)
    integer :: i, j, k
!
    a(1,1) = t33(1,1)
    a(1,2) = t33(2,2)
    a(1,3) = t33(3,3)
    a(2,1) = t33(1,2)
    a(2,2) = t33(1,3)
    a(2,3) = t33(2,3)
    a(3,1) = n33(1,1)
    a(3,2) = n33(2,2)
    a(3,3) = n33(3,3)
    do k = 1, 3
        sn=0.d0
        do i = 1, 3
            x(i)=0.d0
            do j = 1, 3
                x(i)=x(i)+a(i,j)*vsige33(j,k)
            end do
!        sn=sn+x(i)*vsige33(i,k)
            sn=sn+x(i)*x(i)
        end do
!
!       l3(k)=2.d0/sqrt(abs(sn))
        l3(k)=2.d0/sqrt(sn)
!      print*,'l3(',k,')=',l3(k)
    end do
end subroutine
