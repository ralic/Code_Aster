subroutine shbrot(x, rr)
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
!--------------------------------------------------------
! ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
!-------------------------------------------------------
    implicit none
! ---   VARIABLES GLOBALES
    real(kind=8) :: x(24)
    real(kind=8) :: rr(3, 3)
! ---   VARIABLES LOCALES
    real(kind=8) :: aux, aa
!      DATA VB/-1.D0, 1.D0, 1.D0,-1.D0,-1.D0, 1.D0, 1.D0,-1.D0,
!     &        -1.D0,-1.D0, 1.D0, 1.D0,-1.D0,-1.D0, 1.D0, 1.D0,
!     &        -1.D0,-1.D0,-1.D0,-1.D0, 1.D0, 1.D0, 1.D0, 1.D0/
    rr(1,1)= - x(1)  +  x(4)  +  x(7)  -   x(10)&
     &                  - x(13) +  x(16) +  x(19) -   x(22)
    rr(1,2)= - x(2)  +  x(5)  +  x(8)  -   x(11)&
     &                  - x(14) +  x(17) +  x(20) -   x(23)
    rr(1,3)= - x(3)  +  x(6)  +  x(9)  -   x(12)&
     &                  - x(15) +  x(18) +  x(21) -   x(24)
!
    rr(2,1)= - x(1)  -  x(4)  +  x(7)  +   x(10)&
     &                  - x(13) -  x(16) +  x(19) +  x(22)
    rr(2,2)= - x(2)  -  x(5)  +  x(8)  +   x(11)&
     &                  - x(14) -  x(17) +  x(20) +  x(23)
    rr(2,3)= - x(3)  -  x(6)  +  x(9)  +   x(12)&
     &                  - x(15) -  x(18) +  x(21) +  x(24)
!
    aux= rr(1,1)*rr(1,1)+rr(1,2)*rr(1,2)+rr(1,3)*rr(1,3)
    aa = rr(1,1)*rr(2,1)+rr(1,2)*rr(2,2)+rr(1,3)*rr(2,3)
    aa = -(aa/aux)
!
    rr(2,1)= rr(2,1) + aa*rr(1,1)
    rr(2,2)= rr(2,2) + aa*rr(1,2)
    rr(2,3)= rr(2,3) + aa*rr(1,3)
!
    rr(3,1)= rr(1,2)*rr(2,3) - rr(1,3)*rr(2,2)
    rr(3,2)= rr(1,3)*rr(2,1) - rr(1,1)*rr(2,3)
    rr(3,3)= rr(1,1)*rr(2,2) - rr(1,2)*rr(2,1)
!
    aux = sqrt(aux)
    rr(1,1)= rr(1,1)/aux
    rr(1,2)= rr(1,2)/aux
    rr(1,3)= rr(1,3)/aux
!
    aux = sqrt(rr(2,1)*rr(2,1)+rr(2,2)*rr(2,2)+rr(2,3)*rr(2,3))
    rr(2,1)= rr(2,1)/aux
    rr(2,2)= rr(2,2)/aux
    rr(2,3)= rr(2,3)/aux
!
    aux = sqrt(rr(3,1)*rr(3,1)+rr(3,2)*rr(3,2)+rr(3,3)*rr(3,3))
    rr(3,1)= rr(3,1)/aux
    rr(3,2)= rr(3,2)/aux
    rr(3,3)= rr(3,3)/aux
!
end subroutine
