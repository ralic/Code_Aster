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
    subroutine nmdlog(fami, option, typmod, ndim, nno,&
                      npg, iw, ivf, vff, idff,&
                      geomi, dff, compor, mate, lgpg,&
                      crit, angmas, instm, instp, matsym,&
                      deplm, depld, sigm, vim, sigp,&
                      vip, fint, matuu, codret)
        integer :: lgpg
        integer :: npg
        integer :: nno
        integer :: ndim
        character(len=*) :: fami
        character(len=16) :: option
        character(len=8) :: typmod(*)
        integer :: iw
        integer :: ivf
        real(kind=8) :: vff(nno, npg)
        integer :: idff
        real(kind=8) :: geomi(*)
        real(kind=8) :: dff(nno, *)
        character(len=16) :: compor(*)
        integer :: mate
        real(kind=8) :: crit(*)
        real(kind=8) :: angmas(3)
        real(kind=8) :: instm
        real(kind=8) :: instp
        logical(kind=1) :: matsym
        real(kind=8) :: deplm(*)
        real(kind=8) :: depld(*)
        real(kind=8) :: sigm(2*ndim, npg)
        real(kind=8) :: vim(lgpg, npg)
        real(kind=8) :: sigp(2*ndim, npg)
        real(kind=8) :: vip(lgpg, npg)
        real(kind=8) :: fint(ndim*nno)
        real(kind=8) :: matuu(*)
        integer :: codret
    end subroutine nmdlog
end interface
