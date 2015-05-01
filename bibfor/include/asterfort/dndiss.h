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
    subroutine dndiss(ipara, nmnbn, nmplas, nmdpla, nmddpl,&
                      nmprox, deps, newnbn, newpla, newdpl,&
                      newddp, newzfg, despit, ddisit, dc1,&
                      dc2, dtg, normm, normn)
        integer :: ipara(4)
        real(kind=8) :: nmnbn(*)
        real(kind=8) :: nmplas(2, *)
        real(kind=8) :: nmdpla(2, *)
        real(kind=8) :: nmddpl(2, *)
        integer :: nmprox(*)
        real(kind=8) :: deps(*)
        real(kind=8) :: newnbn(*)
        real(kind=8) :: newpla(2, *)
        real(kind=8) :: newdpl(2, *)
        real(kind=8) :: newddp(2, *)
        real(kind=8) :: newzfg(2)
        real(kind=8) :: despit(*)
        real(kind=8) :: ddisit
        real(kind=8) :: dc1(6, 6)
        real(kind=8) :: dc2(6, 6)
        real(kind=8) :: dtg(6, 6)
        real(kind=8) :: normm
        real(kind=8) :: normn
    end subroutine dndiss
end interface
