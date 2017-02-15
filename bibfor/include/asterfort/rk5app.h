!
! COPYRIGHT (C) 1991 - 2017  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine rk5app(nbeq, vparam_real, vparam_int, dtemps, yinit, dyinit,&
                      rk5fct, solu, decoup)
        integer         :: nbeq
        real(kind=8)    :: vparam_real(*)
        integer         :: vparam_int(*)
        real(kind=8)    :: dtemps
        real(kind=8)    :: yinit(nbeq)
        real(kind=8)    :: dyinit(nbeq)
        real(kind=8)    :: solu(3*nbeq)
        aster_logical   :: decoup
!
        interface
            subroutine rk5fct(ppr, ppi, yy0, dy0, dyy, decoup)
                real(kind=8) :: ppr(*)
                integer      :: ppi(*)
                real(kind=8) :: yy0(*)
                real(kind=8) :: dy0(*)
                real(kind=8) :: dyy(*)
                aster_logical :: decoup
            end subroutine rk5fct
        end interface
!
    end subroutine rk5app
end interface
