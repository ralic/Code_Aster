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
    subroutine vechme(stop     , modelz, lload_namez, lload_infoz, inst        ,&
                      cara_elem, mate  , vect_elemz , varc_currz , ligrel_calcz,&
                      nharm)
        character(len=1), intent(in) :: stop
        character(len=*), intent(in) :: modelz
        character(len=*), intent(in) :: lload_namez
        character(len=*), intent(in) :: lload_infoz
        real(kind=8), intent(in) :: inst(3)
        character(len=*), intent(in) :: cara_elem
        character(len=*), intent(in) :: mate
        character(len=*), intent(inout) :: vect_elemz
        character(len=*), optional, intent(in) :: varc_currz
        character(len=*), optional, intent(in) :: ligrel_calcz
        integer, optional, intent(in) :: nharm
    end subroutine vechme
end interface
