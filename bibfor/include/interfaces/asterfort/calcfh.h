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
    subroutine calcfh(option, perman, thmc, ndim, dimdef,&
                      dimcon, yamec, yate, addep1, addep2,&
                      adcp11, adcp12, adcp21, adcp22, addeme,&
                      addete, congep, dsde, p1, p2,&
                      grap1, grap2, t, grat, pvp,&
                      pad, rho11, h11, h12, r,&
                      dsatp1, pesa, permfh, permli, dperml,&
                      krel2, dkr2s, dkr2p, fick, dfickt,&
                      dfickg, fickad, dfadt, kh, cliq,&
                      alpliq, viscl, dviscl, mamolg, viscg,&
                      dviscg, mamolv, isot, dficks, vf,&
                      ifa, valfac, valcen)
        integer :: dimcon
        integer :: dimdef
        character(len=16) :: option
        logical :: perman
        character(len=16) :: thmc
        integer :: ndim
        integer :: yamec
        integer :: yate
        integer :: addep1
        integer :: addep2
        integer :: adcp11
        integer :: adcp12
        integer :: adcp21
        integer :: adcp22
        integer :: addeme
        integer :: addete
        real(kind=8) :: congep(1:dimcon)
        real(kind=8) :: dsde(1:dimcon, 1:dimdef)
        real(kind=8) :: p1
        real(kind=8) :: p2
        real(kind=8) :: grap1(3)
        real(kind=8) :: grap2(3)
        real(kind=8) :: t
        real(kind=8) :: grat(3)
        real(kind=8) :: pvp
        real(kind=8) :: pad
        real(kind=8) :: rho11
        real(kind=8) :: h11
        real(kind=8) :: h12
        real(kind=8) :: r
        real(kind=8) :: dsatp1
        real(kind=8) :: pesa(3)
        real(kind=8) :: permfh
        real(kind=8) :: permli
        real(kind=8) :: dperml
        real(kind=8) :: krel2
        real(kind=8) :: dkr2s
        real(kind=8) :: dkr2p
        real(kind=8) :: fick
        real(kind=8) :: dfickt
        real(kind=8) :: dfickg
        real(kind=8) :: fickad
        real(kind=8) :: dfadt
        real(kind=8) :: kh
        real(kind=8) :: cliq
        real(kind=8) :: alpliq
        real(kind=8) :: viscl
        real(kind=8) :: dviscl
        real(kind=8) :: mamolg
        real(kind=8) :: viscg
        real(kind=8) :: dviscg
        real(kind=8) :: mamolv
        real(kind=8) :: isot(6)
        real(kind=8) :: dficks
        logical :: vf
        integer :: ifa
        real(kind=8) :: valfac(6, 14, 6)
        real(kind=8) :: valcen(14, 6)
    end subroutine calcfh
end interface
