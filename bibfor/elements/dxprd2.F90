subroutine dxprd2(dfpla1, dca, dfpla2, dfpla3, dcb,&
                  dfpla4, scal)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! ----------------------------------------------------------------------
!     REALISE LE CALCUL DES TERMES DU NUMERATEUR INTERVENANT DANS LE
!     CALCUL DE TANGENTE DANS LE CAS DE LA LOI DE COMPORTEMENT GLRC
! ----------------------------------------------------------------------
    implicit none
#include "asterfort/lcprsc.h"
#include "asterfort/pmavec.h"
    common /tdim/ n, nd
    real(kind=8) :: dfpla1(6), dfpla2(6), dfpla3(6), dfpla4(4)
    real(kind=8) :: vecta(6), vectb(6), dca(6, 6), dcb(6, 6)
    real(kind=8) :: scal1, scal2, scal
    integer :: n, nd
!
    n = 6
    call pmavec('ZERO', 6, dca, dfpla2, vecta)
    call lcprsc(dfpla1, vecta, scal1)
    call pmavec('ZERO', 6, dcb, dfpla4, vectb)
    call lcprsc(dfpla3, vectb, scal2)
    scal = scal1*scal2
!
end subroutine
