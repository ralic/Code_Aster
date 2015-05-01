function vpagm2(x)
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
! aslint: disable=
    implicit none
    real(kind=8) :: x
    real(kind=8) :: vpagm2
!
!DEB
!---------------------------------------------------------------
!     FONCTION A(X)  :  CAS DE LA LOI GATT-MONERIE
!---------------------------------------------------------------
! IN  X     :R: ARGUMENT RECHERCHE LORS DE LA RESOLUTION SCALAIRE
!---------------------------------------------------------------
!     L'ETAPE LOCALE DU CALCUL VISCOPLASTIQUE (CALCUL DU TERME
!       ELEMENTAIRE DE LA MATRICE DE RIGIDITE TANGENTE) COMPORTE
!       LA RESOLUTION D'UNE EQUATION SCALAIRE NON LINEAIRE:
!
!           A(X) = 0
!
!       (DPC,SIELEQ,DEUXMU,DELTAT JOUENT LE ROLE DE PARAMETRES)
!---------------------------------------------------------------
!FIN
!     COMMON POUR LES PARAMETRES DES LOIS VISCOPLASTIQUES
    common / nmpavp / sieleq,deuxmu,troisk,deltat,tschem,prec,theta,&
     &                  niter
    real(kind=8) :: sieleq, deuxmu, troisk, deltat, tschem, prec, theta, niter
!
!     COMMON POUR LES PARAMETRES DE LA LOI GATT-MONERIE
    common / nmpagm /ak1,ak2,xn1,xn2,expa1,expa2,expab1,expab2,a1,a2,&
     &                 b1,b2,xw,xq,xh,sige,sigh,sigh0,porom,sgd
    real(kind=8) :: ak1, ak2, xn1, xn2, expa1, expa2, expab1, expab2, a1, a2, b1
    real(kind=8) :: b2, xw, xq, xh, sige, sigh, sigh0, porom, sgd
!
    real(kind=8) :: exp1, exp2, ex1, ex2, tk, tt, xtanh, ctheta, cthetp, psi1
    real(kind=8) :: psi2, psi1p, psi2p, zz1, zz2, g, phip, inv1
!
!---- DEBUT INSERTION C3
!     POTENTIEL BASSE CONTRAINTE (PSI1)
    exp1= xn1 + 1.d0
    ex1 = 0.5d0*exp1
    zz1 = a1*((1.5d0*sigh)**2) + b1*(x**2)
    psi1= (ak1/exp1)*(zz1**ex1)
!     POTENTIEL FORTE CONTRAINTE (PSI12)
    exp2= xn2 + 1.d0
    ex2 = 0.5d0*exp2
    zz2 = a2*((1.5d0*sigh)**2) + b2*(x**2)
!     RESTONS RAISONNABLE
    if (zz2 .ge. 1.d8) then
        zz2 = 1.0d8
    endif
!
    psi2=(ak2/exp2)*(zz2**ex2)
!     FONCTION COUPLAGE (CTHETA)
    tk = tschem + 273.d0
    inv1 = sqrt(2.d0*x*x/3.d0+3.d0*sigh*sigh)
    tt = xw*(inv1**xq)
    xtanh = tanh( (tk-tt)/xh )
    ctheta = 0.5d0*(1 + xtanh )
!     CALCUL DE G
    psi1p = x*ak1*b1*(zz1**(ex1-1.d0))
    psi2p = x*ak2*b2*(zz2**(ex2-1.d0))
    phip = - 2.d0*xw*xq*x*(inv1**(xq-2.d0))/(3.d0*xh)
    cthetp = 0.5d0*(1.d0-xtanh**2)*phip
    if (x .eq. 0.d0) then
        g = 0.d0
    else
        g = (1.d0-ctheta)*psi1p + ctheta*psi2p + (psi2-psi1)*cthetp
    endif
!---- FIN INSERTION C3
    g = g*theta
    vpagm2 = 1.5d0*deuxmu*deltat*g + x - sieleq
end function
