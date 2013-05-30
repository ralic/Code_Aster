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
    subroutine mltfc1(nbloc, ncbloc, decal, supnd, fils,&
                      frere, seq, lgsn, lfront, adress,&
                      local, adpile, nbass, pile, lgpile,&
                      adper, t1, t2, factol, factou,&
                      typsym, ad, eps, ier, nbb,&
                      cl, cu, diag)
        integer :: nbb
        integer :: nbloc
        integer :: ncbloc(*)
        integer :: decal(*)
        integer :: supnd(*)
        integer :: fils(*)
        integer :: frere(*)
        integer :: seq(*)
        integer :: lgsn(*)
        integer :: lfront(*)
        integer :: adress(*)
        integer(kind=4) :: local(*)
        integer :: adpile(*)
        integer :: nbass(*)
        real(kind=8) :: pile(*)
        integer :: lgpile
        integer :: adper(*)
        real(kind=8) :: t1(*)
        real(kind=8) :: t2(*)
        character(len=24) :: factol
        character(len=24) :: factou
        integer :: typsym
        integer :: ad(*)
        real(kind=8) :: eps
        integer :: ier
        real(kind=8) :: cl(nbb, nbb, *)
        real(kind=8) :: cu(nbb, nbb, *)
        real(kind=8) :: diag(*)
    end subroutine mltfc1
end interface
