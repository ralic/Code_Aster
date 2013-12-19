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
    subroutine xcomhm(option, imate, compor, instap,&
                      ndim, dimdef, dimcon, nbvari,&
                      yamec, yap1, yap2, yate,&
                      addeme, adcome, addep1, adcp11,&
                      addep2, addete, defgem,&
                      defgep, congem, congep, vintm,&
                      vintp, dsde, pesa, retcom, kpi,&
                      npg, p10, p20, yaenrm, dimenr,&
                      adenme, idecpg, angmas)
        integer :: dimenr
        integer :: nbvari
        integer :: dimcon
        integer :: dimdef
        integer :: ndim
        character(len=16) :: option
        integer :: imate
        character(len=16) :: compor(*)
        real(kind=8) :: instap
        integer :: yamec
        integer :: yap1
        integer :: yap2
        integer :: yate
        integer :: addeme
        integer :: adcome
        integer :: addep1
        integer :: adcp11
        integer :: addep2
        integer :: addete
        real(kind=8) :: defgem(1:dimdef)
        real(kind=8) :: defgep(1:dimdef)
        real(kind=8) :: congem(1:dimcon)
        real(kind=8) :: congep(1:dimcon)
        real(kind=8) :: vintm(1:nbvari)
        real(kind=8) :: vintp(1:nbvari)
        real(kind=8) :: dsde(1:dimcon, 1:dimenr)
        real(kind=8) :: pesa(3)
        integer :: retcom
        integer :: kpi
        integer :: npg
        real(kind=8) :: p10
        real(kind=8) :: p20
        integer :: yaenrm
        integer :: adenme
        integer :: idecpg
        real(kind=8) :: angmas(3)
    end subroutine xcomhm
end interface 
