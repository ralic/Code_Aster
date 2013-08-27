subroutine dspdp1(net, bishop, signe, tbiot, sat,&
                  dsdp1)
    implicit none
!
#include "asterfort/u2mess.h"
    integer :: i
    real(kind=8) :: signe, tbiot(6), sat, dsdp1(6)
    logical :: net, bishop
! ======================================================================
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! --- CALCUL DE LA DERIVEE DE LA CONTRAINTE DE PRESSION PAR RAPPORT ----
! --- A LA PRESSION CAPILLAIRE -----------------------------------------
! ======================================================================
    do 10 i = 1, 6
        if (bishop) then
            dsdp1(i) = signe*tbiot(i)*sat
        else if (net) then
            dsdp1(i) = 0.d0
        else
            call u2mess('F', 'ALGORITH17_4')
        endif
10  end do
! ======================================================================
end subroutine
