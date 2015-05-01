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
    subroutine mrmmvr(cumul, lmat, smdi, smhc, lmatd,&
                      neq, neql, vect, xsol, nbvect,&
                      vectmp, prepos)
        integer :: nbvect
        integer :: neq
        character(len=*) :: cumul
        integer :: lmat
        integer :: smdi(*)
        integer(kind=4) :: smhc(*)
        aster_logical :: lmatd
        integer :: neql
        real(kind=8) :: vect(neq, nbvect)
        real(kind=8) :: xsol(neq, nbvect)
        real(kind=8) :: vectmp(neq)
        aster_logical :: prepos
    end subroutine mrmmvr
end interface
