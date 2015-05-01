subroutine nmtaac(type, ndimsi, mat, sigel, vim,&
                  epm, dp, sp, xi, sigp,&
                  vip)
!
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
!
    implicit none
#include "asterfort/nmtacr.h"
    integer :: ndimsi, type
    real(kind=8) :: mat(14), sigel(ndimsi), vim(9), epm(ndimsi), dp, sp, xi
    real(kind=8) :: sigp(ndimsi), vip(9)
!
! ----------------------------------------------------------------------
!  TAHERI : ACTUALISATION DES VARIABLES INTERNES ET DES CONTRAINTES
! ----------------------------------------------------------------------
! IN  TYPE   0: ELAS, 1:PSEUDO-ELAS, 2: PLAS, 3: PLAS+G
! IN  NDIMSI DIMENSION DES TENSEURS
! IN  SIGEL  DEVIATEUR DES CONTRAINTES ELASTIQUES
! IN  VIM    VARIABLES INTERNES EN T-
! IN  EPM    DEFORMATION PLASTIQUE EN T-
! IN  DP     INCREMENT DE DEFORMATION PLASTIQUE CUMULEE
! IN  SP     CONTRAINTE DE PIC
! IN  XI     PILOTAGE DE EPN
! VAR SIGP   CONTRAINTE EN T+
! OUT VIP    VARIABLES INTERNES EN T+
! ----------------------------------------------------------------------
!
    integer :: k
    real(kind=8) :: sig(6)
    real(kind=8) :: f, g, fdp, gdp, fds, gds, fdx, gdx, dpmax, tang(6, 6)
!
!
!    ACTUALISATION DES CONTRAINTES
    if (type .ge. 2) then
        call nmtacr(4, ndimsi, mat, sigel, vim,&
                    epm, dp, sp, xi, f,&
                    g, fds, gds, fdp, gdp,&
                    fdx, gdx, dpmax, sig, tang)
        do 5 k = 1, ndimsi
            sigp(k) = sigp(k) - sig(k)
 5      continue
    endif
!
!
!    ACTUALISATION DES VARIABLES INTERNES
    vip(1) = vim(1) + dp
    vip(2) = sp
    do 10 k = 1, ndimsi
        vip(k+2) = epm(k) - xi * (epm(k) - vim(k+2))
10  end do
    vip(9) = type
!
!
end subroutine
