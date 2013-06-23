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
    subroutine nmcore(sdcrit, sderro, sdconv, defico, numins,&
                      iterat, fonact, relite, eta, parcri,&
                      vresi, vrela, vmaxi, vchar, vrefe,&
                      vcomp, vfrot, vgeom)
        character(len=19) :: sdcrit
        character(len=24) :: sderro
        character(len=24) :: sdconv
        character(len=24) :: defico
        integer :: numins
        integer :: iterat
        integer :: fonact(*)
        integer :: relite
        real(kind=8) :: eta
        real(kind=8) :: parcri(*)
        real(kind=8) :: vresi
        real(kind=8) :: vrela
        real(kind=8) :: vmaxi
        real(kind=8) :: vchar
        real(kind=8) :: vrefe
        real(kind=8) :: vcomp
        real(kind=8) :: vfrot
        real(kind=8) :: vgeom
    end subroutine nmcore
end interface
