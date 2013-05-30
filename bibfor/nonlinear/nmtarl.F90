subroutine nmtarl(mode, ndimsi, mat, sigel, vim,&
                  epm, dp, sp, xi, dirdp,&
                  dirsp, dirxi, min, rho, ener)
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
    include 'asterfort/nmtacr.h'
    include 'asterfort/zeroco.h'
    integer :: mode, ndimsi
    real(kind=8) :: mat(*), sigel(*), vim(*), epm(*), dp, sp, xi
    real(kind=8) :: dirdp, dirsp, dirxi, min
    real(kind=8) :: rho, ener
! ----------------------------------------------------------------------
! TAHERI: MINIMISATION DE F**2+G**2 RT A (P,SP,XI)+RHO(DIRP,DIRSP,DIRXI)
! ----------------------------------------------------------------------
! IN  MODE   2: (P,SP)     3: (XI,SP)
! IN  NDIMSI DIMENSION DES TENSEURS
! IN  MAT    TABLEAU DES CONSTANTES MATERIAUX
! IN  SIGEL  DEVIATEUR DES CONTRAINTES ELASTIQUES
! IN  VIM    VARIABLES INTERNES EN T-
! IN  EPM    DEFORMATION PLASTIQUE EN T-
! IN  DP     INCREMENT DE DEFORMATION PLASTIQUE CUMULEE
! IN  SP     CONTRAINTE DE PIC
! IN  XI     PILOTAGE DE EPN
! IN  DIRDP  DIRECTION POUR DP
! IN  DIRSP  DIRECTION POUR SP
! IN  DIRXI  DIRECTION POUR XI
! IN  MIN    VALEUR DE LA DERIVEE EN RHO=0
! VAR RHO    DISTANCE PARCOURUE  IN: RHOMAX  OUT: RHO
! VAR ENER   VALEUR DE (F**2+G**2)/2   IN: EN RHO=0   OUT: EN RHO
! ----------------------------------------------------------------------
!
!
!
    integer :: niter, itelin
    real(kind=8) :: f, g, fds, gds, fdp, gdp, fdx, gdx, dpmax, sig(6)
    real(kind=8) :: tang(6, 6)
    real(kind=8) :: x(4), y(4), energ(4)
    real(kind=8) :: rhomax, refe, prelin
!
    parameter (prelin = 1.d-2, itelin = 3)
!
!
!    INITIALISATION DE LA SOLUTION RHO = 0
    x(1) = 0.d0
    y(1) = min
    refe = min
    if (refe .ge. 0.d0) then
        rho = 0.d0
        goto 9999
    endif
    energ(1) = ener
!
!
!    EXAMEN DE LA SOLUTION RHO = RHOMAX
    rhomax = rho
    x(2) = rhomax
    call nmtacr(mode, ndimsi, mat, sigel, vim,&
                epm, dp+rhomax*dirdp, sp+rhomax*dirsp, xi+rhomax*dirxi, f,&
                g, fds, gds, fdp, gdp,&
                fdx, gdx, dpmax, sig, tang)
    if (mode .eq. 2) then
        y(2) = (f*fdp+g*gdp)*dirdp + (f*fds+g*gds)*dirsp
    else
        y(2) = (f*fdx+g*gdx)*dirxi + (f*fds+g*gds)*dirsp
    endif
    ener = (f**2+g**2)/2.d0
    if (y(2) .le. 0.d0) then
        rho = rhomax
        goto 9999
    endif
    energ(2) = ener
!
!
!    CALCUL DE RHO
!
    x(3) = x(1)
    y(3) = y(1)
    energ(3) = energ(1)
    x(4) = x(2)
    y(4) = y(2)
    energ(4) = energ(2)
!
    do 100 niter = 1, itelin
        if (abs( y(4)/refe ) .lt. prelin) goto 110
        call zeroco(x, y)
        call nmtacr(mode, ndimsi, mat, sigel, vim,&
                    epm, dp+x(4)*dirdp, sp+ x(4)*dirsp, xi+x(4)*dirxi, f,&
                    g, fds, gds, fdp, gdp,&
                    fdx, gdx, dpmax, sig, tang)
        if (mode .eq. 2) then
            y(4) = (f*fdp+g*gdp)*dirdp + (f*fds+g*gds)*dirsp
        else
            y(4) = (f*fdx+g*gdx)*dirxi + (f*fds+g*gds)*dirsp
        endif
        energ(4) = (f**2+g**2)/2.d0
100  end do
110  continue
    rho = x(4)
    ener = energ(4)
!
!
9999  continue
end subroutine
