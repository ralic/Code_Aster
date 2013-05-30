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
    subroutine fetacc(option, rang, dimtet, imsmi, imsmk,&
                      nbreoa, itps, irg, irr, ivlagi,&
                      nbi, ir1, ir2, ir3, nomggt,&
                      lrigid, dimgi, sdfeti, ipiv, nbsd,&
                      vsdf, vddl, matas, nomgi, lstogi,&
                      infofe, irex, iprj, nbproc)
        integer :: nbsd
        integer :: option
        integer :: rang
        integer :: dimtet
        integer :: imsmi
        integer :: imsmk
        integer :: nbreoa
        integer :: itps
        integer :: irg
        integer :: irr
        integer :: ivlagi
        integer :: nbi
        integer :: ir1
        integer :: ir2
        integer :: ir3
        character(len=24) :: nomggt
        logical :: lrigid
        integer :: dimgi
        character(len=19) :: sdfeti
        integer :: ipiv
        integer :: vsdf(nbsd)
        integer :: vddl(nbsd)
        character(len=19) :: matas
        character(len=24) :: nomgi
        logical :: lstogi
        character(len=24) :: infofe
        integer :: irex
        integer :: iprj
        integer :: nbproc
    end subroutine fetacc
end interface
