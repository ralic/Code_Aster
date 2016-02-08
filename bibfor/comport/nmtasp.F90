subroutine nmtasp(ndimsi, crit, mat, sigel, vim,&
                  epm, dp, sp, xi, f,&
                  iret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/zeroco.h"
    integer :: ndimsi
    real(kind=8) :: crit(3), mat(14), sigel(*), vim(9), epm(*), dp, xi
    real(kind=8) :: sp, f
! ----------------------------------------------------------------------
! TAHERI :  RESOLUTION DE L'EQUATION SCALAIRE G(P,SP,XI) = 0 PAR RT A SP
! ----------------------------------------------------------------------
! IN  NDIMSI DIMENSION DES TENSEURS
! IN  CRIT   CRITERES DE CONVERGENCE LOCAUX
! IN  MAT    TABLEAU DES CONSTANTES MATERIAUX
! IN  SIGEL  DEVIATEUR DES CONTRAINTES ELASTIQUES
! IN  VIM    VARIABLES INTERNES EN T-
! IN  EPM    DEFORMATION PLASTIQUE EN T-
! IN  DP     INCREMENT DE DEFORMATION PLASTIQUE CUMULEE
! OUT SP     CONTRAINTE DE PIC (SIGMA P) TEL QUE G(P,SP,XI)=0
! IN  XI     PILOTAGE DE EPN
! OUT F      VALEUR DU CRITERE DE PLASTICITE     F(P,SP,XI)
! OUT IRET   CODE RETOUR DE LA RESOLUTION DE L'EQUATION SCALAIRE
!               IRET=0 => PAS DE PROBLEME
!               IRET=1 => ECHEC
! ----------------------------------------------------------------------
!
    integer :: niter, iret
    real(kind=8) :: g, fds, gds, fdp, gdp, fdx, gdx, dpmax, sig(6), tang(6, 6)
    real(kind=8) :: x(4), y(4), ff(4)
!
!
!    EXAMEN DE LA SOLUTION SP = SP-
!
    sp = vim(2)
    call nmtacr(1, ndimsi, mat, sigel, vim,&
                epm, dp, sp, xi, f,&
                g, fds, gds, fdp, gdp,&
                fdx, gdx, dpmax, sig, tang)
    if (g .lt. mat(4)*crit(3)) goto 9999
    x(2) = sp
    y(2) = g
    ff(2) = f
!
!    EXAMEN DE LA SOLUTION SP = S
!
    sp = mat(11)
    call nmtacr(1, ndimsi, mat, sigel, vim,&
                epm, dp, sp, xi, f,&
                g, fds, gds, fdp, gdp,&
                fdx, gdx, dpmax, sig, tang)
    if (g .gt. -mat(4)*crit(3)) goto 9999
    x(1) = mat(11)
    y(1) = g
    ff(1) = f
!
!    CALCUL DE SP : EQUATION SCALAIRE G=0 AVEC  SP- < SP < S
!
    x(3) = x(1)
    y(3) = y(1)
    ff(3) = ff(1)
    x(4) = x(2)
    y(4) = y(2)
    ff(4) = ff(2)
!
    do 100 niter = 1, int(crit(1))
        if (abs(y(4)) .lt. mat(4)*crit(3)) goto 110
        call zeroco(x, y)
        call nmtacr(1, ndimsi, mat, sigel, vim,&
                    epm, dp, x(4), xi, ff(4),&
                    y(4), fds, gds, fdp, gdp,&
                    fdx, gdx, dpmax, sig, tang)
100  end do
    iret = 1
    goto 9999
110  continue
    sp = x(4)
    f = ff(4)
!
9999  continue
end subroutine
