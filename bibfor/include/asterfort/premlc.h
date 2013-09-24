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
    subroutine premlc(n1, diag, col, parent, parend,&
                      anc, nouv, supnd, supnd2, nouvsn,&
                      ancsn, p, q, lbd1, lbd2,&
                      rl, rl1, rl2, nrl, invp,&
                      perm, lgind, ddlmoy, nbsnd)
        integer :: n1
        integer :: diag(0:*)
        integer :: col(*)
        integer :: parent(*)
        integer :: parend(*)
        integer :: anc(n1)
        integer :: nouv(n1)
        integer :: supnd(n1)
        integer :: supnd2(n1)
        integer :: nouvsn(0:n1)
        integer :: ancsn(*)
        integer :: p(*)
        integer :: q(*)
        integer :: lbd1(n1)
        integer :: lbd2(n1)
        integer :: rl(4, *)
        integer :: rl1(*)
        integer :: rl2(*)
        integer :: nrl
        integer :: invp(n1)
        integer :: perm(n1)
        integer :: lgind
        integer :: ddlmoy
        integer :: nbsnd
    end subroutine premlc
end interface
