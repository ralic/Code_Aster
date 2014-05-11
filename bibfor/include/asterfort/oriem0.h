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
    subroutine oriem0(kdim, type, coor, lino1, nbno1,&
                      lino2, nbno2, lino3, nbno3, ipos,&
                      indmai)
        character(len=2), intent(in) :: kdim
        character(len=8), intent(in) :: type
        real(kind=8), intent(in) :: coor(*)
        integer, intent(in) :: lino1(*)
        integer, intent(in) :: nbno1
        integer, intent(in) :: lino2(*)
        integer, intent(in) :: nbno2
        integer, intent(in) :: lino3(*)
        integer, intent(in) :: nbno3
        integer, intent(out) :: ipos
        integer, intent(out) :: indmai
    end subroutine oriem0
end interface
