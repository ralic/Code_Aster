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
    subroutine coeihm(option, perman, resi, rigi, imate,&
                      compor, crit, instam, instap, nomail,&
                      ndim, dimdef, dimcon, nbvari, yamec,&
                      yap1, yap2, yate, &
                      addeme, adcome, addep1, adcp11, adcp12,&
                      addlh1, adcop1, addep2, adcp21, adcp22,&
                      addete, adcote, defgem, defgep,&
                      kpi, npg, npi, sigm, sigp,&
                      varim, varip, res, drde, retcom)
        integer :: nbvari
        integer :: dimcon
        integer :: dimdef
        integer :: ndim
        character(len=16) :: option
        logical(kind=1) :: perman
        logical(kind=1) :: resi
        logical(kind=1) :: rigi
        integer :: imate
        character(len=16) :: compor(*)
        real(kind=8) :: crit(*)
        real(kind=8) :: instam
        real(kind=8) :: instap
        character(len=8) :: nomail
        integer :: yamec
        integer :: yap1
        integer :: yap2
        integer :: yate
        integer :: addeme
        integer :: adcome
        integer :: addep1
        integer :: adcp11
        integer :: adcp12
        integer :: addlh1
        integer :: adcop1
        integer :: addep2
        integer :: adcp21
        integer :: adcp22
        integer :: addete
        integer :: adcote
        real(kind=8) :: defgem(1:dimdef)
        real(kind=8) :: defgep(1:dimdef)
        integer :: kpi
        integer :: npg
        integer :: npi
        real(kind=8) :: sigm(dimcon)
        real(kind=8) :: sigp(dimcon)
        real(kind=8) :: varim(nbvari)
        real(kind=8) :: varip(nbvari)
        real(kind=8) :: res(dimdef)
        real(kind=8) :: drde(dimdef, dimdef)
        integer :: retcom
    end subroutine coeihm
end interface 
