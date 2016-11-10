!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine xfacxh(elp, jpint, jmilt, jnit, jcnset,&
                  pinter, ninter, jphe, ndim, ainter,nface,nptf,&
                  cface, igeom, jlsn, jaint, jgrlsn, nfiss,&
                  ifiss, fisc, nfisc, nfisc2, ncompe, jstano,&
                  jlst, typdis, minlst)
        integer :: ninter
        integer :: nface
        integer :: cface(30,6)
        integer :: jcnset
        integer :: jnit
        integer :: jmilt
        integer :: jpint
        integer :: nptf
        integer :: ndim
        integer :: jphe
        integer :: igeom
        integer :: jlsn
        integer :: jaint
        integer :: jgrlsn
        real(kind=8) :: pinter(*)
        real(kind=8) :: ainter(*)
        character(len=8) :: elp
        integer :: nfiss
        integer :: ifiss
        integer :: fisc(*)
        integer :: nfisc
        integer :: nfisc2
        integer :: ncompe
        integer :: jstano
        integer :: jlst
        character(len=16) :: typdis
        real(kind=8) :: minlst
    end subroutine xfacxh
end interface
