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
    subroutine nulili(nb_ligr, list_ligr, lili, base , gran_name,&
                      igds   , mesh     , nec , nlili, modelocz)
        integer, intent(in) :: nb_ligr
        character(len=24), pointer, intent(in) :: list_ligr(:)
        character(len=24), intent(in):: lili
        character(len=1), intent(in):: base
        character(len=8), intent(out) :: gran_name
        integer, intent(out) :: igds
        character(len=8), intent(out) :: mesh
        integer, intent(out) :: nec
        integer, intent(out) :: nlili
        character(len=*), optional, intent(in) :: modelocz
    end subroutine nulili
end interface
