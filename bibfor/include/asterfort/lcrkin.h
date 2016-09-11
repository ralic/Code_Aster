!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine lcrkin(ndim, opt, rela_comp, materf, nbcomm,&
                      cpmono, nmat, mod, nvi, sigd,&
                      sigf, vind, vinf, nbphas, iret)
        integer :: nmat
        integer :: ndim
        character(len=16) :: opt
        character(len=16) :: rela_comp
        real(kind=8) :: materf(nmat, 2)
        integer :: nbcomm(nmat, 3)
        character(len=24) :: cpmono(5*nmat+1)
        character(len=8) :: mod
        integer :: nvi
        real(kind=8) :: sigd(*)
        real(kind=8) :: sigf(*)
        real(kind=8) :: vind(*)
        real(kind=8) :: vinf(*)
        integer :: nbphas
        integer :: iret
    end subroutine lcrkin
end interface
