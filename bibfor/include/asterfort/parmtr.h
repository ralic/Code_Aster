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
    subroutine parmtr(np4, nfour, nbm, ttrans, amor,&
                      puls, pulsd, s0, z0, omegaf,&
                      za4, za5)
        integer :: np4
        integer :: nfour
        integer :: nbm
        real(kind=8) :: ttrans
        real(kind=8) :: amor(*)
        real(kind=8) :: puls(*)
        real(kind=8) :: pulsd(*)
        complex(kind=8) :: s0(*)
        complex(kind=8) :: z0(*)
        real(kind=8) :: omegaf(*)
        complex(kind=8) :: za4(np4, *)
        complex(kind=8) :: za5(np4, *)
    end subroutine parmtr
end interface
