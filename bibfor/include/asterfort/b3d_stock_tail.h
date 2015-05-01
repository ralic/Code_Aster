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
    subroutine b3d_stock_tail(xmat, nmatt, ifour, mfr1, nmat0,&
                              nmat1, t33, n33, local, vt33,&
                              var0, varf, nvari, erreur, gf,&
                              fr, rt, epic, beta1, gama1,&
                              nvar1)
#include "asterf_types.h"
        integer :: nvari
        integer :: nmatt
        real(kind=8) :: xmat(nmatt)
        integer :: ifour
        integer :: mfr1
        integer :: nmat0
        integer :: nmat1
        real(kind=8) :: t33(3, 3)
        real(kind=8) :: n33(3, 3)
        aster_logical :: local
        real(kind=8) :: vt33(3, 3)
        real(kind=8) :: var0(nvari)
        real(kind=8) :: varf(nvari)
        integer :: erreur
        real(kind=8) :: gf
        real(kind=8) :: fr
        real(kind=8) :: rt
        real(kind=8) :: epic
        real(kind=8) :: beta1
        real(kind=8) :: gama1
        integer :: nvar1
    end subroutine b3d_stock_tail
end interface 
