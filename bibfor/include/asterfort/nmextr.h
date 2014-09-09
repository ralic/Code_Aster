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
    subroutine nmextr(meshz       , modelz    , sdextrz   , sd_inout , keyw_fact,&
                      nb_keyw_fact, nb_extr   ,&
                      cara_elemz  , matez     , compor    , disp_curr, strx_curr,&
                      varc_curr   , varc_refe , time      )
        character(len=*), intent(in) :: meshz
        character(len=*), intent(in) :: modelz
        character(len=*), intent(in) :: sdextrz
        character(len=24), intent(in) :: sd_inout
        integer, intent(in) :: nb_keyw_fact
        character(len=16), intent(in) :: keyw_fact
        integer, intent(out) :: nb_extr  
        character(len=*), optional, intent(in) :: cara_elemz
        character(len=*), optional, intent(in) :: matez
        character(len=19), optional, intent(in) :: compor
        character(len=*), optional, intent(in) :: disp_curr
        character(len=*), optional, intent(in) :: strx_curr
        character(len=*), optional, intent(in) :: varc_curr
        character(len=*), optional, intent(in) :: varc_refe
        real(kind=8), optional, intent(in) :: time
    end subroutine nmextr
end interface
