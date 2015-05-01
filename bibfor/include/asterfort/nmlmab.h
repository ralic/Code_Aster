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
    subroutine nmlmab(pgl, nno, npg, nc, ugl,&
                      effnom, tempm, tempp, imate, crit,&
                      tmoins, tplus, xlong0, e, a,&
                      coelma, irram, irrap, varim, varip,&
                      kls, flc, effnoc, em, iret)
        real(kind=8) :: pgl(3, 3)
        integer :: nno
        integer :: npg
        integer :: nc
        real(kind=8) :: ugl(12)
        real(kind=8) :: effnom
        real(kind=8) :: tempm
        real(kind=8) :: tempp
        integer :: imate
        real(kind=8) :: crit(3)
        real(kind=8) :: tmoins
        real(kind=8) :: tplus
        real(kind=8) :: xlong0
        real(kind=8) :: e
        real(kind=8) :: a
        real(kind=8) :: coelma(12)
        real(kind=8) :: irram
        real(kind=8) :: irrap
        real(kind=8) :: varim(21)
        real(kind=8) :: varip(21)
        real(kind=8) :: kls(78)
        real(kind=8) :: flc
        real(kind=8) :: effnoc
        real(kind=8) :: em
        integer :: iret
    end subroutine nmlmab
end interface
