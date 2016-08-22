!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine dtmforc_rede(nl_ind , sd_dtm_, sd_nl_, buffdtm, buffnl,&
                            depl  , fext)
        integer               , intent(in)  :: nl_ind
        character(len=*)      , intent(in)  :: sd_dtm_
        character(len=*)      , intent(in)  :: sd_nl_
        integer     , pointer , intent(in)  :: buffdtm  (:)
        integer     , pointer , intent(in)  :: buffnl   (:)
        real(kind=8), pointer , intent(in)  :: depl     (:)
        real(kind=8), pointer , intent(out) :: fext     (:)
    end subroutine dtmforc_rede
end interface
