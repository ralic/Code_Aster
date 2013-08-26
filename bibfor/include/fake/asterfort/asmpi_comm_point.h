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
    subroutine asmpi_comm_point(optmpi, typsca, nudest, numess, nbval, &
                                vi, vi4, vr, sci, sci4, &
                                scr)
        character(len=*), intent(in) :: optmpi
        character(len=*), intent(in) :: typsca
        integer, intent(in) :: nudest
        integer, intent(in) :: numess
        integer, intent(in), optional :: nbval
        integer, intent(inout), optional :: vi(*)
        integer(kind=4), intent(inout), optional :: vi4(*)
        real(kind=8), intent(inout), optional :: vr(*)
        integer, intent(inout), optional :: sci
        integer(kind=4), intent(inout), optional :: sci4
        real(kind=8), intent(inout), optional :: scr
    end subroutine asmpi_comm_point
end interface
