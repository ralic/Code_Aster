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
    subroutine pteddl(typesd   , resuz    , nb_cmp, list_cmp, nb_equa,&
                      tabl_equa, list_equa)
        integer, intent(in) :: nb_cmp
        integer, intent(in) :: nb_equa
        character(len=*), intent(in) :: typesd
        character(len=*), intent(in) :: resuz
        character(len=8), target, intent(in) :: list_cmp(nb_cmp)
        integer, target, optional, intent(inout) :: tabl_equa(nb_equa, nb_cmp)
        integer, target, optional, intent(inout) :: list_equa(nb_equa)
    end subroutine pteddl
end interface
