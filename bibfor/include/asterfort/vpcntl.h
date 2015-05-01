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
    subroutine vpcntl(cty, mode, option, omemin, omemax,&
                      seuil, nfreq, ipos, lmat, omecor,&
                      precdc, ier, vpinf, vpmax, freq,&
                      err, charge, typres, nblagr, solveu,&
                      nbrssa, precsh)
        integer :: nfreq
        character(len=1) :: cty
        character(len=*) :: mode
        character(len=*) :: option
        real(kind=8) :: omemin
        real(kind=8) :: omemax
        real(kind=8) :: seuil
        integer :: ipos(*)
        integer :: lmat(3)
        real(kind=8) :: omecor
        real(kind=8) :: precdc
        integer :: ier
        real(kind=8) :: vpinf
        real(kind=8) :: vpmax
        real(kind=8) :: freq(nfreq)
        real(kind=8) :: err(nfreq)
        real(kind=8) :: charge(nfreq)
        character(len=*) :: typres
        integer :: nblagr
        character(len=19) :: solveu
        integer :: nbrssa
        real(kind=8) :: precsh
    end subroutine vpcntl
end interface
