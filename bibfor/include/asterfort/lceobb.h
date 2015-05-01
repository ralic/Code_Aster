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
#include "asterf_types.h"
!
interface
    subroutine lceobb(intmax, toler, epsm, deps, bm,&
                      dm, lambda, mu, alpha, ecrob,&
                      ecrod, rk, rk1, rk2, b,&
                      d, mult, elas, dbloq, iret)
        integer :: intmax
        real(kind=8) :: toler
        real(kind=8) :: epsm(6)
        real(kind=8) :: deps(6)
        real(kind=8) :: bm(6)
        real(kind=8) :: dm
        real(kind=8) :: lambda
        real(kind=8) :: mu
        real(kind=8) :: alpha
        real(kind=8) :: ecrob
        real(kind=8) :: ecrod
        real(kind=8) :: rk
        real(kind=8) :: rk1
        real(kind=8) :: rk2
        real(kind=8) :: b(6)
        real(kind=8) :: d
        real(kind=8) :: mult
        aster_logical :: elas
        aster_logical :: dbloq
        integer :: iret
    end subroutine lceobb
end interface
