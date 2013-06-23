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
    subroutine pjxxut(dim, mocle, moa1, moa2, nbma1,&
                      lima1, nbno2, lino2, ma1, ma2,&
                      nbtmx, nbtm, nutm, elrf)
        integer :: nbtmx
        character(len=2) :: dim
        character(*) :: mocle
        character(len=8) :: moa1
        character(len=8) :: moa2
        integer :: nbma1
        integer :: lima1(*)
        integer :: nbno2
        integer :: lino2(*)
        character(len=8) :: ma1
        character(len=8) :: ma2
        integer :: nbtm
        integer :: nutm(nbtmx)
        character(len=8) :: elrf(nbtmx)
    end subroutine pjxxut
end interface
