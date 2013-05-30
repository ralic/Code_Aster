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
    subroutine compt(nbpt, fn, offset, t, elapse,&
                     nbchoc, tchocm, tchmax, tchmin, nbrebo,&
                     trebom, tchoct, nbinst)
        integer :: nbpt
        real(kind=8) :: fn(*)
        real(kind=8) :: offset
        real(kind=8) :: t(*)
        real(kind=8) :: elapse
        integer :: nbchoc
        real(kind=8) :: tchocm
        real(kind=8) :: tchmax
        real(kind=8) :: tchmin
        integer :: nbrebo
        real(kind=8) :: trebom
        real(kind=8) :: tchoct
        integer :: nbinst
    end subroutine compt
end interface
