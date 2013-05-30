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
    subroutine fetggt(nbsd, matas, vsdf, vddl, lrigid,&
                      nbi, nomggt, dimgi, nomgi, stogi,&
                      lstogi, mamoy, infofe, irex, ifm,&
                      sdfeti, nbproc, rang, itps)
        integer :: nbsd
        character(len=19) :: matas
        integer :: vsdf(nbsd)
        integer :: vddl(nbsd)
        logical :: lrigid
        integer :: nbi
        character(len=24) :: nomggt
        integer :: dimgi
        character(len=24) :: nomgi
        character(len=24) :: stogi
        logical :: lstogi
        integer :: mamoy
        character(len=24) :: infofe
        integer :: irex
        integer :: ifm
        character(len=19) :: sdfeti
        integer :: nbproc
        integer :: rang
        integer :: itps
    end subroutine fetggt
end interface
