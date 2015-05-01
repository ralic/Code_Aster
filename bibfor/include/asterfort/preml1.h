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
#include "asterf_types.h"
!
interface
    subroutine preml1(neq, n2, diag, delg, col,&
                      xadj, adjncy, parent, adress, supnd,&
                      nnz, qsize, llist, suiv, p,&
                      q, invp, perm, lgind, ddlmoy,&
                      nbsn, optnum, lgadjn, nrl, deb,&
                      vois, suit, ier, nec, prno,&
                      deeq, noeud, ddl, invpnd, permnd,&
                      spndnd, xadjd, matgen)
        integer :: lgadjn
        integer :: n2
        integer :: neq
        integer :: diag(0:neq)
        integer :: delg(neq)
        integer :: col(*)
        integer :: xadj(neq+1)
        integer :: adjncy(lgadjn)
        integer :: parent(neq)
        integer :: adress(neq)
        integer :: supnd(neq)
        integer :: nnz(1:neq)
        integer :: qsize(neq)
        integer :: llist(neq)
        integer :: suiv(neq)
        integer :: p(neq)
        integer :: q(n2)
        integer :: invp(neq)
        integer :: perm(neq)
        integer :: lgind
        integer :: ddlmoy
        integer :: nbsn
        integer :: optnum
        integer :: nrl
        integer :: deb(*)
        integer :: vois(*)
        integer :: suit(*)
        integer :: ier
        integer :: nec
        integer :: prno(*)
        integer :: deeq(*)
        integer :: noeud(*)
        integer :: ddl(*)
        integer :: invpnd(*)
        integer :: permnd(*)
        integer :: spndnd(*)
        integer :: xadjd(*)
        aster_logical :: matgen
    end subroutine preml1
end interface
