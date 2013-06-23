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
    subroutine fonoei(ndim, dt, fnoevo, dimdef, dimcon,&
                      yamec, yap1, yap2, yate, addeme,&
                      addep1, addep2, addete, addlh1, adcome,&
                      adcp11, adcp12, adcp21, adcp22, adcote,&
                      adcop1, adcop2, nbpha1, nbpha2, congem,&
                      r)
        integer :: dimcon
        integer :: dimdef
        integer :: ndim
        real(kind=8) :: dt
        logical :: fnoevo
        integer :: yamec
        integer :: yap1
        integer :: yap2
        integer :: yate
        integer :: addeme
        integer :: addep1
        integer :: addep2
        integer :: addete
        integer :: addlh1
        integer :: adcome
        integer :: adcp11
        integer :: adcp12
        integer :: adcp21
        integer :: adcp22
        integer :: adcote
        integer :: adcop1
        integer :: adcop2
        integer :: nbpha1
        integer :: nbpha2
        real(kind=8) :: congem(dimcon)
        real(kind=8) :: r(dimdef)
    end subroutine fonoei
end interface
