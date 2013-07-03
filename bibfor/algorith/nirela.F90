subroutine nirela(irela, jp, gm, gp, am,&
                  ap, bp, boa, aa, bb,&
                  daa, dbb, dboa, d2boa)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "asterfort/assert.h"
    integer :: irela
    real(kind=8) :: jp, gm, gp
    real(kind=8) :: am, ap, bp, boa, aa, bb, daa, dbb, dboa, d2boa
!-----------------------------------------------------------------------
!          CALCUL DES OPTIONS DE MECANIQUE NON LINEAIRE
!           GRANDES DEFORMATIONS QUASI-INCOMPRESSIBLES
!
!  INITIALISATION DES FONCTIONS POUR LA RELATION : B(J) = B(A(G))
!-----------------------------------------------------------------------
! IN  IRELA  IDENTIFIANT DE LA RELATION : 1) J = 1 + G
!                                         2) ln(J) = G
!                                         3) J = exp(G)
!                                         4) J^2 = 1 + G
! IN  JP     CHANGEMENT DE VOLUME EN T+
! IN  GM     GONFLEMENT EN T-
! IN  GP     GONFLEMENT EN T+
! OUT AM     A(GM)
! OUT AP     A(GP)
! OUT BP     B(JP)
! OUT BOA    B O A EN T+
! OUT AA(G)  DA/DG / A(G)
! OUT BB(J)  J * DB/DJ
! OUT DAA    DAA/DG
! OUT DBB    DBB/DJ
! OUT DBOA   D(B O A)/DG
! OUT D2BOA  D2(B O A)/DG2
!
    if (irela .eq. 1) then
!-----------------------------------------------------------------------
!    APPLICATION: A(G) = 1+G   ET   B(J) = J
!-----------------------------------------------------------------------
        am = 1.d0+gm
        ap = 1.d0+gp
        bp = jp
        boa = ap
        aa = 1.d0/ap
        bb = jp
        daa = -1.d0/(1.d0+gp)**2
        dbb = 1.d0
        dboa = 1.d0
        d2boa = 0.d0
    else if (irela .eq. 2) then
!-----------------------------------------------------------------------
!    APPLICATION: A(G) = exp(G)   ET   B(J) = ln(J)
!-----------------------------------------------------------------------
        am = exp(gm)
        ap = exp(gp)
        bp = log(jp)
        boa = gp
        aa = 1.d0
        bb = 1.d0
        daa = 0.d0
        dbb = 0.d0
        dboa = 1.d0
        d2boa = 0.d0
    else if (irela .eq. 3) then
!-----------------------------------------------------------------------
!    APPLICATION: A(G) = exp(G)   ET   B(J) = J
!-----------------------------------------------------------------------
        am = exp(gm)
        ap = exp(gp)
        bp = jp
        boa = ap
        aa = 1.d0
        bb = jp
        daa = 1.d0
        dbb = jp
        dboa = ap
        d2boa = ap
    else if (irela .eq. 4) then
!-----------------------------------------------------------------------
!    APPLICATION: A(G) = SQRT(1+G)   ET   B(J) = J^2
!-----------------------------------------------------------------------
        am = sqrt(1.d0+gm)
        ap = sqrt(1.d0+gp)
        bp = jp**2
        boa = 1.d0+gp
        aa = 0.5d0/ap
        bb = 2.d0*jp**2
        daa = -0.25d0/ap**(3.d0/2.d0)
        dbb = 4.d0*jp
        dboa = 1.d0
        d2boa = 0.d0
    else
        call assert(.false.)
    endif
end subroutine
