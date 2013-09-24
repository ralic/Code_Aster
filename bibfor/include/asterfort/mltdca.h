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
    subroutine mltdca(nbloc, lgbloc, ncbloc, decal, seq,&
                      nbsn, nbnd, supnd, adress, global,&
                      lgsn, factol, factou, sm, x,&
                      invp, perm, ad, trav, typsym)
        integer :: nbnd
        integer :: nbsn
        integer :: nbloc
        integer :: lgbloc(nbsn)
        integer :: ncbloc(nbnd)
        integer :: decal(nbsn)
        integer :: seq(nbsn)
        integer :: supnd(nbsn+1)
        integer :: adress(nbsn+1)
        integer(kind=4) :: global(*)
        integer :: lgsn(nbsn)
        character(len=24) :: factol
        character(len=24) :: factou
        complex(kind=8) :: sm(nbnd)
        complex(kind=8) :: x(nbnd)
        integer :: invp(nbnd)
        integer :: perm(nbnd)
        integer :: ad(nbnd)
        complex(kind=8) :: trav(nbnd)
        integer :: typsym
    end subroutine mltdca
end interface
