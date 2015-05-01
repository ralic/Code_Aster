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
    subroutine genmmd(neqns, neqp1, nadj, xadj, adjncy,&
                      maxint, delta, invp, perm, nbsn,&
                      supnd, adress, parent, gssubs, fctnzs,&
                      fctops, dhead, qsize, llist, marker)
        integer :: nadj
        integer :: neqp1
        integer :: neqns
        integer :: xadj(neqp1)
        integer :: adjncy(nadj)
        integer :: maxint
        integer :: delta
        integer :: invp(neqns)
        integer :: perm(neqns)
        integer :: nbsn
        integer :: supnd(neqp1)
        integer :: adress(neqp1)
        integer :: parent(neqns)
        integer :: gssubs
        integer :: fctnzs
        real(kind=8) :: fctops
        integer :: dhead(neqns)
        integer :: qsize(neqns)
        integer :: llist(neqns)
        integer :: marker(neqns)
    end subroutine genmmd
end interface
