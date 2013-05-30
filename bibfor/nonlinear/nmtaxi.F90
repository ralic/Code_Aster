subroutine nmtaxi(ndimsi, crit, mat, sigel, vim,&
                  epm, dp, sp, xi, g,&
                  iret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterfort/assert.h'
    include 'asterfort/nmtacr.h'
    include 'asterfort/zeroco.h'
    integer :: ndimsi, iret
    real(kind=8) :: crit(3), mat(14), sigel(*), vim(9), epm(*), dp, sp
    real(kind=8) :: xi, g
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
! IN  SP     CONTRAINTE DE PIC
! OUT XI     PILOTAGE DE EPN               TQ    F(P,SP,XI) = 0
! OUT G      VALEUR DU CRITERE                   G(P,SP,XI)
! OUT IRET   CODE RETOUR DE LA RESOLUTION DE L'EQUATION SCALAIRE
!               IRET=0 => PAS DE PROBLEME
!               IRET=1 => ECHEC
! ----------------------------------------------------------------------
!
    integer :: niter
    real(kind=8) :: f, fds, gds, fdp, gdp, fdx, gdx, dpmax, sig(6), tang(6, 6)
    real(kind=8) :: x(4), y(4), gg(4)
!
!
!    EXAMEN DE LA SOLUTION XI = 1 (PLASTIQUE)
!
    xi = 1.d0
    call nmtacr(1, ndimsi, mat, sigel, vim,&
                epm, dp, sp, xi, f,&
                g, fds, gds, fdp, gdp,&
                fdx, gdx, dpmax, sig, tang)
    if (f .gt. 0.d0) goto 9999
    x(1) = xi
    y(1) = f
    gg(1) = g
!
!
!    EXAMEN DE LA SOLUTION XI = 0 (ELASTIQUE)
!
    xi = 0.d0
    call nmtacr(1, ndimsi, mat, sigel, vim,&
                epm, dp, sp, xi, f,&
                g, fds, gds, fdp, gdp,&
                fdx, gdx, dpmax, sig, tang)
    call assert(f.gt.0.d0)
    x(2) = xi
    y(2) = f
    gg(2) = g
!
!    CALCUL DE XI : EQUATION SCALAIRE F=0 AVEC  0 < XI < 1
!
    x(3) = x(1)
    y(3) = y(1)
    gg(3) = gg(1)
    x(4) = x(2)
    y(4) = y(2)
    gg(4) = gg(2)
!
    do 100 niter = 1, int(crit(1))
        if (abs(y(4))/mat(4) .lt. crit(3) .and. x(4) .ne. 0.d0) goto 110
        call zeroco(x, y)
        call nmtacr(1, ndimsi, mat, sigel, vim,&
                    epm, dp, sp, x(4), y(4),&
                    gg(4), fds, gds, fdp, gdp,&
                    fdx, gdx, dpmax, sig, tang)
100  end do
    iret = 1
    goto 9999
110  continue
    xi = x(4)
    g = gg(4)
!
9999  continue
end subroutine
