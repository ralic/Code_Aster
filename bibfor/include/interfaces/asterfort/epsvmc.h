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
    subroutine epsvmc(fami, nno, ndim, nbsig, npg,&
                      ipoids, ivf, idfde, xyz, depl,&
                      instan, mater, repere, nharm, option,&
                      epsm)
        character(len=4) :: fami
        integer :: nno
        integer :: ndim
        integer :: nbsig
        integer :: npg
        integer :: ipoids
        integer :: ivf
        integer :: idfde
        real(kind=8) :: xyz(1)
        real(kind=8) :: depl(1)
        real(kind=8) :: instan
        integer :: mater
        real(kind=8) :: repere(7)
        real(kind=8) :: nharm
        character(len=16) :: option
        real(kind=8) :: epsm(1)
    end subroutine epsvmc
end interface
