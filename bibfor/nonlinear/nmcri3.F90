function nmcri3(depsv)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
!     ARGUMENTS:
!     ----------
    include 'asterc/r8gaem.h'
    include 'asterc/r8prem.h'
    real(kind=8) :: nmcri3, depsv
! ----------------------------------------------------------------------
!    BUT:  EVALUER LA FONCTION DONT ON CHERCHE LE ZERO
!          POUR LA LOI DE FLUAGE DU LMAB
!
!     IN:  DEPSV  : ACCROISSEMENT DE DEFORMATION VISQUEUSE
!    OUT:  NMCRI3 : VALEUR DU CRITERE  DE CONVERGENCE DE L'EQUATION NON
!                   LINEAIRE A RESOUDRE
!                   (DONT ON CHERCHE LE ZERO)
!
! ----------------------------------------------------------------------
    real(kind=8) :: coelma(12), epsv, depst, sig, x, x1, x2, dx, dx1, dx2
    real(kind=8) :: deltat
    real(kind=8) :: xm, xn, depsi0, p, p1, p2, xm0, rm0, y0, x0, ysat, b
    real(kind=8) :: y, f1, f2, yy, e, sigsig, depthe, depgrd, ier
!
!----- COMMONS NECESSAIRES A LMAB UNIAXIALE
!      COMMONS COMMUNS A NMCRI3 ET NMLMAB
!
    common/rconm3/coelma,epsv,depst,sig,x,x1,x2,dx,dx1,dx2,deltat,e,&
     &              sigsig,depthe,depgrd,ier
!
! ----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    real(kind=8) :: argsh, grand, prec
!-----------------------------------------------------------------------
    prec = r8prem()
    grand = r8gaem()
    xm = coelma(1)
    xn = (1.d0/coelma(2))
    depsi0= coelma(3)
    p = coelma(4)
    p1 = coelma(5)
    p2 = coelma(6)
    xm0 = coelma(7)
    rm0 = coelma(8)
!      XM1   = COELMA(9)
!      RM1   = COELMA(10)
    x0 = coelma(9)
    y0 = coelma(10)
    ysat = coelma(11)
    b = coelma(12)
!
    yy = ysat + (y0-ysat)*exp(-b*(epsv+depsv))
    dx2 = p2*( sigsig*yy - x2)*depsv/(1.d0 + p2*depsv)
    dx1 = p1*( sigsig*yy - ( x1 - x2 - dx2))*depsv/(1.d0 + p1*depsv)
!
    y = (depsv/(depsi0*deltat))**xn
!
    f1 = sig + e*(depst-sigsig*depsv-depthe-depgrd) - xm*sigsig*log(y+sqrt(1.d0+y**2)) - x
!      F2   = P*( SIGSIG*YY - ( X + F1 - ( X1 + DX1)))*DEPSV
!     &     - DELTAT * RM0*((X+F1)/ABS(X+F1))*(ABS(X+F1)/X0)**XM0
!     &     - DELTAT * RM1*((X+F1)/ABS(X+F1))*(ABS(X+F1)/X0)**XM1
    argsh = (abs(x+f1)/x0)**xm0
!
    if (argsh .gt. (0.9d0 * log(grand))) then
        ier=1.0d0
    else
        ier=0.d0
        if (abs(x+f1) .lt. prec) then
            f2 = p*( sigsig*yy - ( x + f1 - ( x1 + dx1)))*depsv
        else
            f2 = p*(&
                 sigsig*yy - (&
                 x + f1 - ( x1 + dx1)))*depsv - deltat * rm0*((x+f1)/abs(x+f1))*sinh((abs(x+f1)/x&
                 &0&
                 )** xm0&
                 )
        endif
!
        dx = f1
!
        nmcri3 = sigsig*(f2 - f1)
    endif
!
end function
