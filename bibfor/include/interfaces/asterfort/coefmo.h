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
    subroutine coefmo(typflu, zrigi, nbm, nmode, indic,&
                      x, pulsc, vgap, xsi0, veci1,&
                      vecr1, vecr2, vecr3, vecr4, vecr5,&
                      xmf, xkf, xcf)
        character(len=8) :: typflu
        logical :: zrigi
        integer :: nbm
        integer :: nmode
        integer :: indic
        real(kind=8) :: x(2)
        real(kind=8) :: pulsc
        real(kind=8) :: vgap
        real(kind=8) :: xsi0
        integer :: veci1(*)
        real(kind=8) :: vecr1(*)
        real(kind=8) :: vecr2(*)
        real(kind=8) :: vecr3(*)
        real(kind=8) :: vecr4(*)
        real(kind=8) :: vecr5(*)
        real(kind=8) :: xmf
        complex(kind=8) :: xkf
        real(kind=8) :: xcf
    end subroutine coefmo
end interface
