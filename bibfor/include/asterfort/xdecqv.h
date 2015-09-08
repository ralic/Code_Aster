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
    subroutine xdecqv(nnose, it, cnset, heavt, lsn, igeom,&
                      ninter, npts, ndim, ainter, nse, cnse,&
                      heav, nsemax, pinter, pmilie, pintt, pmitt, cut,&
                      ncomp, nfisc, nfiss, ifiss, elp, fisco,&
                      lonref, txlsn, tx)
        integer :: nnose
        integer :: it
        integer :: cnset(*)
        real(kind=8) :: lsn(*)
        integer :: igeom
        integer :: ninter
        integer :: npts
        integer :: ndim
        real(kind=8) :: ainter(*)
        real(kind=8) :: pinter(*)
        real(kind=8) :: pmilie(*)
        real(kind=8) :: pintt(*)
        real(kind=8) :: pmitt(*)
        integer :: nse
        integer :: cnse(6, 10)
        real(kind=8) :: heav(*)
        integer :: nsemax
        aster_logical :: cut
        integer :: heavt(*)
        integer :: ncomp
        integer :: nfisc
        integer :: nfiss
        integer :: ifiss
        integer :: fisco(*)
        character(len=8) :: elp
        real(kind=8) :: lonref
        real(kind=8) :: txlsn(28)
        real(kind=8) :: tx(3, 7)
    end subroutine xdecqv
end interface
