!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
!
interface
    subroutine wp2ayc(lmatra, lmasse, lamor, sigma, lbloq,&
                      yh, yb, zh, zb, u1,&
                      u2, u3, n, solveu)
        integer :: lmatra
        integer :: lmasse
        integer :: lamor
        complex(kind=8) :: sigma
        integer :: lbloq(*)
        complex(kind=8) :: yh(*)
        complex(kind=8) :: yb(*)
        complex(kind=8) :: zh(*)
        complex(kind=8) :: zb(*)
        complex(kind=8) :: u1(*)
        complex(kind=8) :: u2(*)
        complex(kind=8) :: u3(*)
        integer :: n
        character(len=19) :: solveu
    end subroutine wp2ayc
end interface
