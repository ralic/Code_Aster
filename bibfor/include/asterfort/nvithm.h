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
    subroutine nvithm(compor, meca, thmc, ther, hydr,&
                      nvim, nvit, nvih, nvic, advime,&
                      advith, advihy, advico, vihrho, vicphi,&
                      vicpvp, vicsat, vicpr1, vicpr2)
        character(len=16) :: compor(*)
        character(len=16) :: meca
        character(len=16) :: thmc
        character(len=16) :: ther
        character(len=16) :: hydr
        integer :: nvim
        integer :: nvit
        integer :: nvih
        integer :: nvic
        integer :: advime
        integer :: advith
        integer :: advihy
        integer :: advico
        integer :: vihrho
        integer :: vicphi
        integer :: vicpvp
        integer :: vicsat
        integer :: vicpr1
        integer :: vicpr2
    end subroutine nvithm
end interface
