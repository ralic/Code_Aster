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
    subroutine lcfrge(ndim, typmod, imate, epsm, deps,&
                      vim, option, sig, vip, dsidpt,&
                      proj)
        integer :: ndim
        character(len=8) :: typmod(*)
        integer :: imate
        real(kind=8) :: epsm(12)
        real(kind=8) :: deps(12)
        real(kind=8) :: vim(2)
        character(len=16) :: option
        real(kind=8) :: sig(6)
        real(kind=8) :: vip(2)
        real(kind=8) :: dsidpt(6, 6, 2)
        real(kind=8) :: proj(6, 6)
    end subroutine lcfrge
end interface
