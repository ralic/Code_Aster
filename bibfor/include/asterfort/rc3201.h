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
    subroutine rc3201(lpmpb, lsn, lsnet, lfatig, lrocht,&
                      lieu, ig, iocs, seisme, npass,&
                      mater, snmax, snemax, spmax, kemax,&
                      spmecm, spthem, samax, utot, sm,&
                      sigpm, resuas, resuss, resuca, resucs,&
                      factus, pmmax, pbmax, pmbmax)
        logical(kind=1) :: lpmpb
        logical(kind=1) :: lsn
        logical(kind=1) :: lsnet
        logical(kind=1) :: lfatig
        logical(kind=1) :: lrocht
        character(len=4) :: lieu
        integer :: ig
        integer :: iocs
        logical(kind=1) :: seisme
        integer :: npass
        character(len=8) :: mater
        real(kind=8) :: snmax
        real(kind=8) :: snemax
        real(kind=8) :: spmax
        real(kind=8) :: kemax
        real(kind=8) :: spmecm
        real(kind=8) :: spthem
        real(kind=8) :: samax
        real(kind=8) :: utot
        real(kind=8) :: sm
        real(kind=8) :: sigpm
        real(kind=8) :: resuas(*)
        real(kind=8) :: resuss(*)
        real(kind=8) :: resuca(*)
        real(kind=8) :: resucs(*)
        real(kind=8) :: factus(*)
        real(kind=8) :: pmmax
        real(kind=8) :: pbmax
        real(kind=8) :: pmbmax
    end subroutine rc3201
end interface
