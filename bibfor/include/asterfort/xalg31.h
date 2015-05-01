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
    subroutine xalg31(ndim, elrefp, nnop, it, nnose, cnset, typma, ndime,&
                      igeom, jlsn, pmilie, ninter, ainter, ar, npts, nptm, &
                      pmmax, nmilie, mfis, lonref, pinref)
        integer :: ndim
        integer :: nnop
        integer :: it
        integer :: nnose
        integer :: cnset(*)
        integer :: ndime
        integer :: igeom
        integer :: jlsn
        integer :: ninter
        integer ::  ar(12, 3)
        integer :: npts
        integer :: nptm
        integer :: nbar
        integer :: pmmax
        integer :: nmilie
        integer :: mfis
        character(len=8) :: typma
        character(len=8) :: elrefp
        real(kind=8) :: lonref
        real(kind=8) :: ainter(*)
        real(kind=8) :: pmilie(*)
        real(kind=8) :: pinref(*) 
    end subroutine xalg31
end interface 
