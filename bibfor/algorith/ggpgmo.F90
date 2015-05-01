subroutine ggpgmo(s, h, theta, deuxmu, g,&
                  devpkk, dgdst, dgdev, tschem)
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!DEB
!---------------------------------------------------------------
!     VITESSE DE DEF. VISQUEUSE ET SA DERIVEE PAR RAPPORT A SIGEQ
!---------------------------------------------------------------
! IN  S     :R: CONTRAINTE EQUIVALENTE SIGMA
! IN  H     :R: PRESSION HYDROSTATIQUE SIGMA
!     THETA :R: PARAMETRE DU SCHEMA D'INTEGRATION (0.5 OU 1)
!                  THETA = 0.5 -> SEMI-IMPLICITE
!                  THETA = 1.0 -> IMPLICITE
! OUT G     :R: VALEUR DE LA FONCTION G
!     DEVPKK:R: COMPOSANTE SPHERIQUE VITESSE DEF. VISCO. (GD/3)
!     DGDST :R: DERIVEE TOTALE DE G PAR RAPPORT A SIGMA
!     DGDEV :R: DERIVEE PARTIELLE DE G PAR RAPPORT A EV (I.E. DPC)
!---------------------------------------------------------------
!            DANS LE CAS DE LA LOI GATT-MONERIE,
!     CETTE ROUTINE CALCULE LES FONCTIONS G ET GD DE L'ECOULEMENT
!     VISCOPLASTIQUE
!
!     ET LA DERIVEE TOTALE DE CES FONCTIONS G ET GD PAR RAPPORT
!     A SIGEQ
!---------------------------------------------------------------
!FIN
!---- DEBUT INSERTION C3 ---------------------------------------
!     COMMON POUR LES PARAMETRES DE LA LOI GATT-MONERIE
    common / nmpagm /ak1,ak2,xn1,xn2,expa1,expa2,expab1,expab2,a1,a2,&
     &                 b1,b2,xw,xq,xh,sige,sigh,sigh0,porom,sgd
    real(kind=8) :: ak1, ak2, xn1, xn2, expa1, expa2, expab1, expab2, a1, a2, b1
    real(kind=8) :: b2, xw, xq, xh, sige, sigh, sigh0, porom, sgd
!
    real(kind=8) :: tschem, inv1
!
!-----------------------------------------------------------------------
    real(kind=8) :: ctheta, cthetp, deuxmu, devpkk, dgdev, dgdst, ex1
    real(kind=8) :: ex2, exp1, exp2, g, h, phip, psi1
    real(kind=8) :: psi1p, psi2, psi2p, s, theta, tk, tt
    real(kind=8) :: xtanh, zz1, zz2
!-----------------------------------------------------------------------
    if (s .eq. 0.d0 .and. h .eq. 0d0) then
        g = 0.d0
        devpkk = 0.d0
        dgdst = 0.d0
        dgdev = 0.d0
        goto 99
    else
!     POTENTIEL BASSE CONTRAINTE (PSI1)
        exp1= xn1 + 1.d0
        ex1 = 0.5d0*exp1
        zz1 = a1*((1.5d0*h)**2) + b1*(s**2)
        psi1= (ak1/exp1)*(zz1**ex1)
!     POTENTIEL FORTE CONTRAINTE (PSI12)
        exp2= xn2 + 1.d0
        ex2 = 0.5d0*exp2
        zz2 = a2*((1.5d0*h)**2) + b2*(s**2)
        psi2=(ak2/exp2)*(zz2**ex2)
!     FONCTION COUPLAGE (CTHETA)
        tk = tschem + 273.d0
        inv1 = sqrt(2.d0*s*s/3.d0+3.d0*h*h)
        tt = xw*(inv1**xq)
        xtanh = tanh( (tk-tt)/xh )
        ctheta = 0.5d0*(1 + xtanh )
!     CALCUL DE G
        if (inv1 .ne. 0.d0) then
            psi1p = s*ak1*b1*(zz1**(ex1-1.d0))
            psi2p = s*ak2*b2*(zz2**(ex2-1.d0))
            phip = - 2.d0*xw*xq*s*(inv1**(xq-2.d0))/(3.d0*xh)
            cthetp = 0.5d0*(1.d0-xtanh**2)*phip
            g = (1.d0-ctheta)*psi1p + ctheta*psi2p + (psi2-psi1)* cthetp
            dgdst = (1.d0-ctheta)*( ak1*b1*(zz1**(ex1-1.d0)) +(xn1- 1.d0)*ak1*((b1*s)**2)*(zz1**(&
                    &ex1-2.d0)) ) + ctheta*( ak2* b2*(zz2**(ex2-1.d0)) +(xn2-1.d0)*ak2*((b2*s)**2&
                    &)*(zz2**( ex2-2.d0)) ) + 2.d0*cthetp*(psi2p-psi1p) + (psi2-psi1)*(-( 1.d0-xt&
                    &anh**2)*xw*xq*(inv1**(xq-2.d0)) /(3.d0*xh) + cthetp*(2.d0*(xq-2.d0)*s/(3.d0*&
                    &inv1*inv1) - 2.d0*phip* xtanh ) )
        else
            g = 0.d0
            dgdst = (1.d0-ctheta)*( ak1*b1*(zz1**(ex1-1.d0)) ) + ctheta*( ak2*b2*(zz2**(ex2-1.d0)&
                    &) )
        endif
        devpkk = (1.d0-ctheta)*h*ak1*a1*(zz1**(ex1-1.d0)) + ctheta*h* ak2*a2*(zz2**(ex2-1.d0))
!---  CORRECTION RM 24/02/2005
        devpkk = devpkk*3.d0/4.d0 + (psi2-psi1)*0.5d0*(1.d0-xtanh**2) *( -xw*xq*h*(inv1**(xq-2.d0&
                 &))/xh )
!---- FIN INSERTION C3 -------------------------------------------
        dgdev = 0.d0
    endif
    g = g*theta
    dgdst = dgdst*theta
    dgdev = dgdev*theta
    devpkk = devpkk*theta
    if (dgdst .lt. 0.d0) then
        write(*,*) 'GATT-MONE, DGDSEQ<0', dgdst,s,h
    endif
99  continue
!
end subroutine
