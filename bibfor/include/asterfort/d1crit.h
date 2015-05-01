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
    subroutine d1crit(zimat, nmnbn, nmplas, nmdpla, nmprox,&
                      cnbn, cplas, rpara, cief, cdeps,&
                      cdtg, cier, cdepsp, dc, bend)
        integer :: zimat
        real(kind=8) :: nmnbn(6)
        real(kind=8) :: nmplas(2, 3)
        real(kind=8) :: nmdpla(2, 2)
        integer :: nmprox(2)
        real(kind=8) :: cnbn(6)
        real(kind=8) :: cplas(2, 3)
        real(kind=8) :: rpara(3)
        integer :: cief
        real(kind=8) :: cdeps(6)
        real(kind=8) :: cdtg(6, 6)
        integer :: cier
        real(kind=8) :: cdepsp(6)
        real(kind=8) :: dc(6, 6)
        integer :: bend
    end subroutine d1crit
end interface
