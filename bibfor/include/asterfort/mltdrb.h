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
    subroutine mltdrb(nbloc, ncbloc, decal, seq, nbsn,&
                      nbnd, supnd, adress, global, lgsn,&
                      factol, factou, x, temp, invp,&
                      perm, ad, trav, typsym, nbsm,&
                      s)
        integer :: nbsm
        integer :: nbnd
        integer :: nbsn
        integer :: nbloc
        integer :: ncbloc(nbnd)
        integer :: decal(nbsn)
        integer :: seq(nbsn)
        integer :: supnd(nbsn+1)
        integer :: adress(nbsn+1)
        integer(kind=4) :: global(*)
        integer :: lgsn(nbsn)
        character(len=24) :: factol
        character(len=24) :: factou
        real(kind=8) :: x(nbnd, nbsm)
        real(kind=8) :: temp(nbnd)
        integer :: invp(nbnd)
        integer :: perm(nbnd)
        integer :: ad(nbnd)
        real(kind=8) :: trav(nbnd, nbsm)
        integer :: typsym
        real(kind=8) :: s(nbsm)
    end subroutine mltdrb
end interface
