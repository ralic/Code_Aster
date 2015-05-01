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
    subroutine xcface(lsn, lst, jgrlsn, igeom,&
                      enr, nfiss, ifiss, fisco, nfisc,&
                      noma, nmaabs, typdis, pinter, ninter, ainter,&
                      nface, nptf, cface, minlst)
        integer :: nfisc
        character(len=8) :: elref
        real(kind=8) :: lsn(*)
        real(kind=8) :: lst(*)
        integer :: jgrlsn
        integer :: igeom
        character(len=16) :: enr
        integer :: nfiss
        integer :: ifiss
        integer :: fisco(*)
        character(len=8) :: noma
        integer :: nmaabs
        character(len=16) :: typdis
        real(kind=8) :: pinter(*)
        integer :: ninter
        real(kind=8) :: ainter(*)
        integer :: nface
        integer :: nptf
        integer :: cface(18, 6)
        real(kind=8) :: minlst
    end subroutine xcface
end interface
