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
    subroutine xdecfa(elp, nno, igeom, jlsn, jlst, npi,npis,&
                      pinter, pinref, ainter, jcnset, cooree, cooref, rainter,&
                      noeud, npts, nintar, lst ,lonref, ndim, zxain,&
                      jnit, i, face, nnose, jmilt, f, mipos)
        integer :: ndim
        integer :: jlsn
        integer :: jlst
        integer :: igeom
        integer :: nno
        integer :: npi
        integer :: npis
        integer :: jcnset
        integer :: noeud(9)
        integer :: npts
        integer :: nintar
        integer :: zxain
        integer :: jnit
        integer :: i
        integer :: nnose
        integer :: jmilt
        integer :: f(6,8)
        integer :: face
        real(kind=8) :: pinter(*)
        real(kind=8) :: pinref(*)
        real(kind=8) :: ainter(*)
        real(kind=8) :: cooree(3, ndim)
        real(kind=8) :: cooref(3, ndim)
        real(kind=8) :: rainter(3, 4)
        real(kind=8) :: lst(3)
        real(kind=8) :: lonref
        character(len=8) :: elp
        aster_logical :: mipos
    end subroutine xdecfa
end interface
