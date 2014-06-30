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
    subroutine irmasu(ifc, ndim, nno, coordo, nbma,&
                      connex, point, typma, typel, codgra,&
                      codphy, codphd, permut, maxnod, lmod,&
                      noma, nbgrn, nogn, nbgrm, nogm,&
                      lmasu, nomai, nonoe, versio)
        integer :: maxnod
        integer :: ifc
        integer :: ndim
        integer :: nno
        real(kind=8) :: coordo(*)
        integer :: nbma
        integer :: connex(*)
        integer :: point(*)
        integer :: typma(*)
        integer :: typel(*)
        integer :: codgra(*)
        integer :: codphy(*)
        integer :: codphd(*)
        integer :: permut(maxnod, *)
        logical(kind=1) :: lmod
        character(len=8) :: noma
        integer :: nbgrn
        character(len=24) :: nogn(*)
        integer :: nbgrm
        character(len=24) :: nogm(*)
        logical(kind=1) :: lmasu
        character(len=8) :: nomai(*)
        character(len=8) :: nonoe(*)
        integer :: versio
    end subroutine irmasu
end interface
