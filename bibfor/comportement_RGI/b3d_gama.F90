subroutine b3d_gama(x, gama)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: etienne grimal at edf.fr
!=====================================================================
!=====================================================================
!     approximation de la fonction gamma
!     par f2 := 1.075*(x/exp(1))^x*sqrt(2)*sqrt(Pi/x)
!=====================================================================
        implicit none
        real(kind=8) :: x
        real(kind=8) :: gama,t1,t4,t5,t9
    t1 = exp(0.1d1)
    t4 = (x / t1) ** x
    t5 = sqrt(0.2d1)
    t9 = sqrt(0.3141592654D1 / x)
    gama = 0.1075d1 * t4 * t5 * t9
end subroutine
