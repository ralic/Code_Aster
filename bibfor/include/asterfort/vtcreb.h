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
    subroutine vtcreb(field_nodez , base      , type_scalz,&
                      nume_ddlz   ,&
                      meshz       , prof_chnoz, idx_gdz, nb_equa_inz,&
                      nb_equa_outz)
        character(len=*), intent(in) :: field_nodez
        character(len=1), intent(in) :: base
        character(len=*), intent(in) :: type_scalz
        character(len=*), optional, intent(in) :: nume_ddlz
        character(len=*), optional, intent(in) :: meshz
        character(len=*), optional, intent(in) :: prof_chnoz
        integer, optional, intent(in) :: nb_equa_inz
        integer, optional, intent(in) :: idx_gdz
        integer, optional, intent(out) :: nb_equa_outz
    end subroutine vtcreb
end interface
