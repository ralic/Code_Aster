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
    subroutine coeime(meca, imate, nomail, option, resi,&
                      rigi, ndim, dimdef, dimcon, yap1,&
                      yap2, yate, addeme, addep1, addep2,&
                      nbvari, advime, advico, npg, npi,&
                      defgep, defgem, sigm, sigp, varim,&
                      varip, ouvh, tlint, drde, kpi,&
                      vicphi, unsurn, retcom)
        integer :: nbvari
        integer :: dimcon
        integer :: dimdef
        integer :: ndim
        character(len=16) :: meca
        integer :: imate
        character(len=8) :: nomail
        character(len=16) :: option
        logical :: resi
        logical :: rigi
        integer :: yap1
        integer :: yap2
        integer :: yate
        integer :: addeme
        integer :: addep1
        integer :: addep2
        integer :: advime
        integer :: advico
        integer :: npg
        integer :: npi
        real(kind=8) :: defgep(dimdef)
        real(kind=8) :: defgem(dimdef)
        real(kind=8) :: sigm(dimcon)
        real(kind=8) :: sigp(dimcon)
        real(kind=8) :: varim(nbvari)
        real(kind=8) :: varip(nbvari)
        real(kind=8) :: ouvh
        real(kind=8) :: tlint
        real(kind=8) :: drde(dimdef, dimdef)
        integer :: kpi
        integer :: vicphi
        real(kind=8) :: unsurn
        integer :: retcom
    end subroutine coeime
end interface
