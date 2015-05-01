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
! aslint: disable=W1504
interface
    subroutine nmresi(noma, mate, numedd, sdnume, fonact,&
                      sddyna, sdconv, sdimpr, defico, resoco,&
                      matass, numins, conv, resi_glob_rela, resi_glob_maxi, eta,&
                      comref, valinc, solalg, veasse, measse,&
                      vrela, vmaxi, vchar, vresi, vrefe,&
                      vinit, vcomp, vfrot, vgeom)
        character(len=8) :: noma
        character(len=24) :: mate
        character(len=24) :: numedd
        character(len=19) :: sdnume
        integer :: fonact(*)
        character(len=19) :: sddyna
        character(len=24) :: sdconv
        character(len=24) :: sdimpr
        character(len=24) :: defico
        character(len=24) :: resoco
        character(len=19) :: matass
        integer :: numins
        real(kind=8) :: conv(*)
        real(kind=8) :: resi_glob_rela
        real(kind=8) :: resi_glob_maxi
        real(kind=8) :: eta
        character(len=24) :: comref
        character(len=19) :: valinc(*)
        character(len=19) :: solalg(*)
        character(len=19) :: veasse(*)
        character(len=19) :: measse(*)
        real(kind=8) :: vrela
        real(kind=8) :: vmaxi
        real(kind=8) :: vchar
        real(kind=8) :: vresi
        real(kind=8) :: vrefe
        real(kind=8) :: vinit
        real(kind=8) :: vcomp
        real(kind=8) :: vfrot
        real(kind=8) :: vgeom
    end subroutine nmresi
end interface
