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
    subroutine fetarp(infofe, ifm, niter, nbi, nbreor,&
                      lrigid, dimgi, sdfeti, ipiv, nomggt,&
                      nbsd, ifetf, ifeth, nomgi, lstogi,&
                      irex, iprj, ir2, ifiv, matas,&
                      nbproc, rang)
        character(len=24) :: infofe
        integer :: ifm
        integer :: niter
        integer :: nbi
        integer :: nbreor
        logical :: lrigid
        integer :: dimgi
        character(len=19) :: sdfeti
        integer :: ipiv
        character(len=24) :: nomggt
        integer :: nbsd
        integer :: ifetf
        integer :: ifeth
        character(len=24) :: nomgi
        logical :: lstogi
        integer :: irex
        integer :: iprj
        integer :: ir2
        integer :: ifiv
        character(len=19) :: matas
        integer :: nbproc
        integer :: rang
    end subroutine fetarp
end interface
