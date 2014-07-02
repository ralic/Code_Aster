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
#include "asterf_types.h"
!
interface
    subroutine nmrebo(f, mem, sens, rho, rhoopt,&
                      ldcopt, ldccvg, fopt, fcvg, opt,&
                      act, rhomin, rhomax, rhoexm, rhoexp,&
                      stite, echec)
        real(kind=8) :: f
        real(kind=8) :: mem(2, *)
        real(kind=8) :: sens
        real(kind=8) :: rho
        real(kind=8) :: rhoopt
        integer :: ldcopt
        integer :: ldccvg
        real(kind=8) :: fopt
        real(kind=8) :: fcvg
        integer :: opt
        integer :: act
        real(kind=8) :: rhomin
        real(kind=8) :: rhomax
        real(kind=8) :: rhoexm
        real(kind=8) :: rhoexp
        aster_logical :: stite
        aster_logical :: echec
    end subroutine nmrebo
end interface
