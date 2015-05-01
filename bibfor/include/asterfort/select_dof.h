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
    subroutine select_dof(list_equa, tabl_equa , list_idx_dof,&
                          nume_ddlz, chamnoz   ,&
                          nb_nodez , list_nodez,&
                          nb_cmpz  , list_cmpz)
        integer, pointer, optional, intent(inout) :: list_equa(:)
        integer, pointer, optional, intent(inout) :: tabl_equa(:,:)
        integer, pointer, optional, intent(inout) :: list_idx_dof(:)
        character(len=*), optional, intent(in) :: nume_ddlz
        character(len=*), optional, intent(in) :: chamnoz
        integer, optional, intent(in) :: nb_nodez
        integer, optional, pointer, intent(in) :: list_nodez(:)
        integer, optional, intent(in) :: nb_cmpz
        character(len=8), optional, pointer, intent(in) :: list_cmpz(:)
    end subroutine select_dof
end interface
