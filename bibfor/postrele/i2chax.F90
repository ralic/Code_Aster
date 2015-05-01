subroutine i2chax(x1, y1, x2, y2, x3,&
                  y3, xm, ym, xn, yn,&
                  c, xnm, ynm, xnn, ynn)
    implicit none
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
!*****************************************************************
!
!         CALCUL DES AXES DANS LESQUELS LA PARABOLE
!         PASSANT PAR LES (XI,YI) EST REPRESENTEE PAR
!         LA COURBE D' EQUATION Y = C X**2
!
!         PUIS, CALCUL DES COORDONNEES DES POINTS M, N, ET P
!         DANS CE NOUVEAU SYSTEME D' AXES.
!
!*****************************************************************
!
    real(kind=8) :: x1, y1, x2, y2, x3, y3, xm, ym, xn, yn
    real(kind=8) :: c, xnm, ynm, xnn, ynn
!
    real(kind=8) :: a2, a1, a0, b2, b1, b0
    real(kind=8) :: c1, c0, d2, d1, d0
    real(kind=8) :: invc1, invc12, invalf
    real(kind=8) :: alfa, beta, gama
    real(kind=8) :: tx, ty
    real(kind=8) :: u, v
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    u = 0.0d0
    v = 0.0d0
!
    xnm = 0.0d0
    ynm = 0.0d0
    xnn = 0.0d0
    ynn = 0.0d0
!
    a2 = x1 - 2*x2 + x3
    a2 = 2*a2
    a1 = -3*x1 + 4*x2 - x3
    a0 = x1
    b2 = y1 - 2*y2 + y3
    b2 = 2*b2
    b1 = -3*y1 + 4*y2 - y3
    b0 = y1
!
    c0 = a0*b2 - a2*b0
    c1 = a1*b2 - a2*b1
    d0 = a2*a0 + b2*b0
    d1 = a2*a1 + b2*b1
    d2 = a2*a2 + b2*b2
!
    invc1 = 1.0d0/c1
    invc12 = invc1*invc1
!
    alfa = d2 *invc12
    beta = d1*invc1
    gama = d0 - c0*beta
    beta = beta - 2.0d0*c0*alfa
    gama = gama + c0*c0*alfa
!
    c = alfa
!
    invalf = 1.0d0/alfa
!
    tx = 0.5d0*beta*invalf
    ty = 0.5d0*beta*tx - gama
!
!_____________TRAITEMENT DE M--------------------------------
!
    u = b2*xm - a2*ym
    v = a2*xm + b2*ym
!
    xnm = u + tx
    ynm = v + ty
!_____________TRAITEMENT DE N--------------------------------
!
    u = b2*xn - a2*yn
    v = a2*xn + b2*yn
!
    xnn = u + tx
    ynn = v + ty
!
end subroutine
