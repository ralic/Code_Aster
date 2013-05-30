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
    subroutine facsmb(nbnd, nbsn, supnd, invsup, parent,&
                      xadj, adjncy, anc, nouv, fils,&
                      frere, local, global, adress, lfront,&
                      nblign, lgsn, debfac, debfsn, chaine,&
                      place, nbass, delg, lgind, ier)
        integer :: lgind
        integer :: nbsn
        integer :: nbnd
        integer :: supnd(nbsn+1)
        integer :: invsup(nbnd)
        integer :: parent(nbsn)
        integer :: xadj(nbnd+1)
        integer :: adjncy(*)
        integer :: anc(nbnd)
        integer :: nouv(nbnd)
        integer :: fils(nbsn)
        integer :: frere(nbsn)
        integer(kind=4) :: local(lgind)
        integer(kind=4) :: global(lgind)
        integer :: adress(nbsn+1)
        integer :: lfront(nbsn)
        integer :: nblign(nbsn)
        integer :: lgsn(nbsn)
        integer :: debfac(nbnd+1)
        integer :: debfsn(nbsn+1)
        integer :: chaine(nbnd)
        integer :: place(nbnd)
        integer :: nbass(nbsn)
        integer :: delg(nbnd)
        integer :: ier
    end subroutine facsmb
end interface
