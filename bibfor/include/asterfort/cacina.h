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
    subroutine cacina(ndim, nno, npg, lgpg, axi,&
                      grand, compor, geomm, g, iw,&
                      vff, idff, fm, fma, depld,&
                      instm, instp, vim, rp, rpa,&
                      lambp)
        integer :: lgpg
        integer :: npg
        integer :: nno
        integer :: ndim
        logical :: axi
        logical :: grand
        character(len=16) :: compor(*)
        real(kind=8) :: geomm(3, nno)
        integer :: g
        integer :: iw
        real(kind=8) :: vff(nno, npg)
        integer :: idff
        real(kind=8) :: fm(3, 3)
        real(kind=8) :: fma(3, 3)
        real(kind=8) :: depld(81)
        real(kind=8) :: instm
        real(kind=8) :: instp
        real(kind=8) :: vim(lgpg)
        real(kind=8) :: rp(3, 3)
        real(kind=8) :: rpa(3, 3)
        real(kind=8) :: lambp(3, 3)
    end subroutine cacina
end interface
