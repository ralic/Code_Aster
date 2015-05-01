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
    subroutine rairep(noma, ioc, km, rigi, nbgr,&
                      ligrma, nbno, tabnoe, rignoe, rigto,&
                      amoto, rirot, ndim)
        integer :: nbgr
        character(len=8) :: noma
        integer :: ioc
        character(len=8) :: km
        real(kind=8) :: rigi(6)
        character(len=24) :: ligrma(nbgr)
        integer :: nbno
        character(len=8) :: tabnoe(*)
        real(kind=8) :: rignoe(*)
        real(kind=8) :: rigto(*)
        real(kind=8) :: amoto(*)
        real(kind=8) :: rirot(3)
        integer :: ndim
    end subroutine rairep
end interface
