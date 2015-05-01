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
    subroutine eccook(acook, bcook, ccook, npuis, mpuis,&
                      epsp0, troom, tmelt, tp, dinst,&
                      pm, dp, rp, rprim)
        real(kind=8) :: acook
        real(kind=8) :: bcook
        real(kind=8) :: ccook
        real(kind=8) :: npuis
        real(kind=8) :: mpuis
        real(kind=8) :: epsp0
        real(kind=8) :: troom
        real(kind=8) :: tmelt
        real(kind=8) :: tp
        real(kind=8) :: dinst
        real(kind=8) :: pm
        real(kind=8) :: dp
        real(kind=8) :: rp
        real(kind=8) :: rprim
    end subroutine eccook
end interface
