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
    subroutine lcumef(option, dep, depm, an, bn,&
                      cn, epsm, epsrm, epsrp, depsi,&
                      epsfm, sigi, nstrs, sigt)
        character(len=16) :: option(2)
        real(kind=8) :: dep(6, 6)
        real(kind=8) :: depm(6, 6)
        real(kind=8) :: an(6)
        real(kind=8) :: bn(6, 6)
        real(kind=8) :: cn(6, 6)
        real(kind=8) :: epsm(6)
        real(kind=8) :: epsrm
        real(kind=8) :: epsrp
        real(kind=8) :: depsi(6)
        real(kind=8) :: epsfm(6)
        real(kind=8) :: sigi(6)
        integer :: nstrs
        real(kind=8) :: sigt(6)
    end subroutine lcumef
end interface
