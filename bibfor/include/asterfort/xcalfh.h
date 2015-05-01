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
interface 
    subroutine xcalfh(option, thmc, ndim, dimcon, yamec,&
                      addep1, adcp11, addeme, congep, dsde,&
                      grap1, rho11, pesa, tperm, cliq,&
                      viscl, dviscl, dimenr,&
                      adenhy)
        integer :: dimenr
        integer :: dimcon
        character(len=16) :: option
        character(len=16) :: thmc
        integer :: ndim
        integer :: yamec
        integer :: addep1
        integer :: adcp11
        integer :: addeme
        real(kind=8) :: congep(1:dimcon)
        real(kind=8) :: dsde(1:dimcon, 1:dimenr)
        real(kind=8) :: grap1(3)
        real(kind=8) :: rho11
        real(kind=8) :: pesa(3)
        real(kind=8) :: tperm(ndim,ndim)
        real(kind=8) :: cliq
        real(kind=8) :: viscl
        real(kind=8) :: dviscl
        integer :: adenhy
    end subroutine xcalfh
end interface 
