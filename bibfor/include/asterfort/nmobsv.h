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
    subroutine nmobsv(meshz     , modelz, sddisc, sd_obsv  , nume_time,&
                      cara_elemz, matez , compor, varc_refe, valinc   ,&
                      sd_inout)
        character(len=*), intent(in) :: meshz
        character(len=19), intent(in) :: sd_obsv
        integer, intent(in) :: nume_time
        character(len=19), intent(in) :: sddisc
        character(len=*), intent(in) :: cara_elemz
        character(len=*), intent(in) :: matez
        character(len=*), intent(in) :: modelz
        character(len=19), intent(in) :: compor
        character(len=*), intent(in) :: varc_refe
        character(len=19), intent(in) :: valinc(*)
        character(len=24), optional, intent(in) :: sd_inout
    end subroutine nmobsv
end interface
