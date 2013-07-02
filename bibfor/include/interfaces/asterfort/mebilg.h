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
    subroutine mebilg(optioz, result, modele, depla1, depla2,&
                      theta, mate, lischa, symech, timeu, &
                      timev, indi, indj, nbprup, noprup)
        character(len=16) :: optioz
        character(len=8) :: result
        character(len=8) :: modele
        character(len=24) :: depla1
        character(len=24) :: depla2
        character(len=24) :: theta
        character(len=24) :: mate
        character(len=19) :: lischa
        character(len=8) :: symech
        real(kind=8) :: timeu
        real(kind=8) :: timev
        integer :: indi
        integer :: indj
        integer :: nbprup
        character(len=16) :: noprup(*)
    end subroutine mebilg
end interface
