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
    subroutine piqpiq(xp, yp, zp, x, y,&
                      z, rep, ret, rit, bet,&
                      eso, hso, h2, h3, l4,&
                      l5, zone1, zone2, zone3, zone4,&
                      zone5, zone6, zone7, zone8, typsou)
        real(kind=8) :: xp
        real(kind=8) :: yp
        real(kind=8) :: zp
        real(kind=8) :: x
        real(kind=8) :: y
        real(kind=8) :: z
        real(kind=8) :: rep
        real(kind=8) :: ret
        real(kind=8) :: rit
        real(kind=8) :: bet
        real(kind=8) :: eso
        real(kind=8) :: hso
        real(kind=8) :: h2
        real(kind=8) :: h3
        real(kind=8) :: l4
        real(kind=8) :: l5
        aster_logical :: zone1
        aster_logical :: zone2
        aster_logical :: zone3
        aster_logical :: zone4
        aster_logical :: zone5
        aster_logical :: zone6
        aster_logical :: zone7
        aster_logical :: zone8
        character(len=8) :: typsou
    end subroutine piqpiq
end interface
