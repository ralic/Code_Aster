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
    subroutine mltpas(nbnd, nbsn, supnd, xadj, adjncy,&
                      anc, nouv, seq, global, adress,&
                      nblign, lgsn, nbloc, ncbloc, lgbloc,&
                      diag, col, lmat, place)
        integer :: nbsn
        integer :: nbnd
        integer :: supnd(nbsn+1)
        integer :: xadj(nbnd+1)
        integer :: adjncy(*)
        integer :: anc(nbnd)
        integer :: nouv(nbnd)
        integer :: seq(nbsn)
        integer(kind=4) :: global(*)
        integer :: adress(nbsn+1)
        integer :: nblign(nbsn)
        integer :: lgsn(nbsn)
        integer :: nbloc
        integer :: ncbloc(*)
        integer :: lgbloc(*)
        integer :: diag(0:nbnd)
        integer :: col(*)
        integer :: lmat
        integer :: place(nbnd)
    end subroutine mltpas
end interface
