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
    subroutine parglr(nlit, elb, ea, nua, liner,&
                      omx, omy, rx, ry, hh,&
                      bn11, bn12, bn22, bn33, bm11,&
                      bm12, bm22, bc11, bc22)
        integer :: nlit
        real(kind=8) :: elb(*)
        real(kind=8) :: ea(*)
        real(kind=8) :: nua(*)
        real(kind=8) :: liner(*)
        real(kind=8) :: omx(*)
        real(kind=8) :: omy(*)
        real(kind=8) :: rx(*)
        real(kind=8) :: ry(*)
        real(kind=8) :: hh
        real(kind=8) :: bn11
        real(kind=8) :: bn12
        real(kind=8) :: bn22
        real(kind=8) :: bn33
        real(kind=8) :: bm11
        real(kind=8) :: bm12
        real(kind=8) :: bm22
        real(kind=8) :: bc11
        real(kind=8) :: bc22
    end subroutine parglr
end interface
