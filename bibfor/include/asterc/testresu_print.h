!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
! person_in_charge: mathieu.courtois at edf.fr
!
interface
    subroutine testresu_print(refer, legend, llab, skip, rela, &
                              tole, typ, refr, valr, refi, &
                              vali, refc, valc, compare)
        implicit none
        character(len=16), intent(in) :: refer
        character(len=16), intent(in) :: legend
        integer, intent(in) :: llab
        integer, intent(in) :: skip
        integer, intent(in) :: rela
        real(kind=8), intent(in) :: tole
        integer, intent(in) :: typ
        real(kind=8), intent(in) :: refr
        real(kind=8), intent(in) :: valr
        integer, intent(in) :: refi
        integer, intent(in) :: vali
        complex(kind=8), intent(in) :: refc
        complex(kind=8), intent(in) :: valc
        real(kind=8), intent(in) :: compare
    end subroutine testresu_print
end interface
