!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine calcmi(np1, nbm, dt0, dt, vitg,&
                      depg, vitg0, depg0, fmod, fmod0,&
                      amor, amor0, puls, puls0, trans,&
                      pulsd, s0, z0, sr0, za1,&
                      za2, za3, zin)
        integer :: np1
        integer :: nbm
        real(kind=8) :: dt0
        real(kind=8) :: dt
        real(kind=8) :: vitg(*)
        real(kind=8) :: depg(*)
        real(kind=8) :: vitg0(*)
        real(kind=8) :: depg0(*)
        real(kind=8) :: fmod(*)
        real(kind=8) :: fmod0(*)
        real(kind=8) :: amor(*)
        real(kind=8) :: amor0(*)
        real(kind=8) :: puls(*)
        real(kind=8) :: puls0(*)
        real(kind=8) :: trans(2, 2, *)
        real(kind=8) :: pulsd(*)
        complex(kind=8) :: s0(*)
        complex(kind=8) :: z0(*)
        complex(kind=8) :: sr0(*)
        complex(kind=8) :: za1(*)
        complex(kind=8) :: za2(*)
        complex(kind=8) :: za3(*)
        complex(kind=8) :: zin(*)
    end subroutine calcmi
end interface
