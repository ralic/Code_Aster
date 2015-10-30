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
    subroutine dlarch(result, neq, istoc, iarchi, texte,&
                      alarm, temps, nbtyar, typear, masse,&
                      depl, vite, acce, fexte, famor,&
                      fliai)
        integer :: nbtyar
        integer :: neq
        character(len=8) :: result
        integer :: istoc
        integer :: iarchi
        character(len=*) :: texte
        integer :: alarm
        real(kind=8) :: temps
        character(len=16) :: typear(nbtyar)
        character(len=8) :: masse
        real(kind=8) :: depl(neq)
        real(kind=8) :: vite(neq)
        real(kind=8) :: acce(neq)
        real(kind=8) :: fexte(neq)
        real(kind=8) :: famor(neq)
        real(kind=8) :: fliai(neq)
    end subroutine dlarch
end interface
