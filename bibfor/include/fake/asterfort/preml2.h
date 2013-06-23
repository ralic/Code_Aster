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
    subroutine preml2(n1, diag, col, delg, xadj1,&
                      adjnc1, estim, adress, parend, fils,&
                      frere, anc, nouv, supnd, dhead,&
                      qsize, llist, marker, invsup, local,&
                      global, lfront, nblign, decal, lgsn,&
                      debfac, debfsn, seq, lmat, adpile,&
                      chaine, suiv, place, nbass, ncbloc,&
                      lgbloc, nbloc, lgind, nbsnd, ier)
        integer :: n1
        integer :: diag(0:n1)
        integer :: col(*)
        integer :: delg(*)
        integer :: xadj1(n1+1)
        integer :: adjnc1(*)
        integer :: estim
        integer :: adress(*)
        integer :: parend(*)
        integer :: fils(n1)
        integer :: frere(n1)
        integer :: anc(n1)
        integer :: nouv(n1)
        integer :: supnd(n1)
        integer :: dhead(*)
        integer :: qsize(*)
        integer :: llist(*)
        integer :: marker(*)
        integer :: invsup(n1)
        integer(kind=4) :: local(*)
        integer(kind=4) :: global(*)
        integer :: lfront(n1)
        integer :: nblign(n1)
        integer :: decal(*)
        integer :: lgsn(n1)
        integer :: debfac(n1+1)
        integer :: debfsn(n1)
        integer :: seq(n1)
        integer :: lmat
        integer :: adpile(n1)
        integer :: chaine(n1)
        integer :: suiv(n1)
        integer :: place(n1)
        integer :: nbass(n1)
        integer :: ncbloc(*)
        integer :: lgbloc(*)
        integer :: nbloc
        integer :: lgind
        integer :: nbsnd
        integer :: ier
    end subroutine preml2
end interface
