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
    subroutine xvetth(ndim, elrefp, nnop, imate, itps,&
                      igeom, temper, lonch, cnset, jpintt,&
                      lsn, lst, basloc, heavt, nfh,&
                      nfe, vectt)
        integer :: nfe
        integer :: nfh
        integer :: nnop
        integer :: ndim
        character(len=8) :: elrefp
        integer :: imate
        integer :: itps
        integer :: igeom
        real(kind=8) :: temper(nnop*(1+nfh+nfe))
        integer :: lonch(10)
        integer :: cnset(128)
        integer :: jpintt
        real(kind=8) :: lsn(nnop)
        real(kind=8) :: lst(nnop)
        real(kind=8) :: basloc(*)
        integer :: heavt(36)
        real(kind=8) :: vectt(*)
    end subroutine xvetth
end interface
