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
    subroutine calmdg(model, modgen, nugene, num, nu,&
                      ma, mate, moint, moflui, ndble,&
                      itxsto, itysto, itzsto, iprsto, nbmo,&
                      iadirg)
        character(len=2) :: model
        character(len=8) :: modgen
        character(len=14) :: nugene
        character(len=14) :: num
        character(len=14) :: nu
        character(len=8) :: ma
        character(*) :: mate
        character(len=8) :: moint
        character(len=8) :: moflui
        integer :: ndble
        integer :: itxsto
        integer :: itysto
        integer :: itzsto
        integer :: iprsto
        integer :: nbmo
        integer :: iadirg
    end subroutine calmdg
end interface
