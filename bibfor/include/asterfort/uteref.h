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
    subroutine uteref(chanom, typech, tyelas, nomte, nomfpg,&
                      nnos, nno, nbpg, ndim, refcoo,&
                      gscoo, wg, nochmd, codret)
        character(len=19) :: chanom
        character(len=8) :: typech
        integer :: tyelas
        character(len=16) :: nomte
        character(len=16) :: nomfpg
        integer :: nnos
        integer :: nno
        integer :: nbpg
        integer :: ndim
        real(kind=8) :: refcoo(*)
        real(kind=8) :: gscoo(*)
        real(kind=8) :: wg(*)
        character(len=64) :: nochmd
        integer :: codret
    end subroutine uteref
end interface
