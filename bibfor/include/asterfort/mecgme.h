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
    subroutine mecgme(modelz   , cara_elemz    , matez    , list_load, inst_curr,&
                      disp_prev, disp_cumu_inst, inst_prev, compor   , carcri,&
                      matr_elem)
        character(len=*), intent(in) :: modelz
        character(len=*), intent(in) :: cara_elemz
        character(len=*), intent(in) :: matez
        character(len=19), intent(in) :: list_load
        real(kind=8), intent(in) :: inst_prev
        real(kind=8), intent(in) :: inst_curr
        character(len=19), intent(in) :: disp_prev
        character(len=19), intent(in) :: disp_cumu_inst
        character(len=24), intent(in) :: compor
        character(len=24), intent(in) :: carcri
        character(len=19), intent(in) :: matr_elem
    end subroutine mecgme
end interface
