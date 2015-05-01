subroutine d2crit(zimat, nmnbn, nmplas, nmdpla, nmprox,&
                  cnbn, cplas, rpara, cief, cdeps,&
                  cdtg, cier, cdepsp, dc1, dc2)
    implicit none
! ======================================================================
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
!     CALCUL DES MULTIPLICATEURS PLASTIQUES
!     ET DE L INCREMENT DE COURBURE PLASTIQUE
!     DANS LE CAS OU 2 CRITERE PLASTIQUE SONT ACTIVES
!     METHODE EXPLICITE (PREMIER ORDRE)
!
! IN  ZIMAT : ADRESSE DE LA LISTE DE MATERIAU CODE
! IN  NMNBN : FORCE - BACKFORCE
! IN  NMPLAS : MOMENTS LIMITES DE PLASTICITE
! IN  NMDPLA : DERIVEES DES MOMENTS LIMITES DE PLASTICITE
! IN  NMDDPL : DERIVEES SECONDES DES MOMENTS LIMITES DE PLASTICITE
! IN  NMPROX : NMPROX > 0 : NBN DANS ZONE DE CRITIQUE
! IN  CDTG : MATRICE TANGENTE
! IN  DC1 : MATRICE ELASTIQUE + CONSTANTES DE PRAGER
! IN  DC2 : MATRICE ELASTIQUE + CONSTANTES DE PRAGER
!
! IN/OUT RPARA : LISTES DE PARAMETRES DE TYPE ENTIER
!
! OUT CNBN : NOUVELLE FORCE - BACKFORCE
! OUT CPLAS : NOUVEAUX MOMENTS LIMITES DE PLASTICITE
! OUT CIEF : NOUVEAU CIEF > 0 : NBN HORS DE LA ZONE DE DEFINITION DE MP
! OUT CDEPS : NOUVEL INCREMENT DE DEFORMATION DANS LE REPERE ORTHO
! OUT CIER : NOUVEAU CODE ERREUR
! OUT CDEPSP : NOUVEL INCREMENT DE DEF PLASTIQUE DANS LE REPERE ORTHO
!
#include "asterfort/dfplgl.h"
#include "asterfort/dfuuss.h"
#include "asterfort/fplass.h"
#include "asterfort/matmul.h"
#include "asterfort/nmnet2.h"
#include "asterfort/r8inir.h"
    integer :: j, zimat, nmprox(2), cief, cier
!
    real(kind=8) :: nmnbn(6), nmplas(2, 3), nmdpla(2, 2)
    real(kind=8) :: cnbn(6), cplas(2, 3), czef, czeg, cdeps(6)
    real(kind=8) :: cdtg(6, 6), cdepsp(6), dc1(6, 6), dc2(6, 6), normm
    real(kind=8) :: rpara(3), cp(6), depsp2(6, 2)
    real(kind=8) :: lambda(2, 2), f1, f2, df(6, 2), tdf(2, 6), a(2), b1(2)
    real(kind=8) :: b2(2)
    real(kind=8) :: denom, df1(6), df2(6), aux, dfu1(6), dfu2(6), dfu(6, 2)
!
    czef = rpara(1)
    czeg = rpara(2)
    normm = rpara(3)
!
!     CALCUL LE GRADIENT DES CRITERES DE PLASICITE
    call dfplgl(nmnbn, nmplas, nmdpla, 1, df1)
    call dfplgl(nmnbn, nmplas, nmdpla, 2, df2)
!
    do 10, j = 1,6
    df(j,1) = df1(j)
    df(j,2) = df2(j)
    tdf(1,j) = df(j,1)
    tdf(2,j) = df(j,2)
    10 end do
!
!     CALUL DES DIRECTIONS DE L ECOULEMENT DES DEFORMATIONS PLASTIQUES
    call dfuuss(nmnbn, nmplas, nmdpla, nmprox, 1,&
                dfu1)
    call dfuuss(nmnbn, nmplas, nmdpla, nmprox, 2,&
                dfu2)
!
    do 20, j = 1,6
    dfu(j,1) = dfu1(j)
    dfu(j,2) = dfu2(j)
    20 end do
!
    call matmul(cdtg, cdeps, 6, 6, 1,&
                cp)
    call matmul(tdf, cp, 2, 6, 1,&
                a)
    call matmul(dc1, dfu1, 6, 6, 1,&
                cp)
    call matmul(tdf, cp, 2, 6, 1,&
                b1)
    call matmul(dc2, dfu2, 6, 6, 1,&
                cp)
    call matmul(tdf, cp, 2, 6, 1,&
                b2)
!
    denom = b1(1)*b2(2)-b1(2)*b2(1)
    aux = b1(1)**2 + b2(1)**2 + b1(2)**2 + b2(2)**2
!
!     CALUL DES MULTIPLICATEURS PLASTIQUES
    call r8inir(2*2, 0.0d0, lambda, 1)
!
    f1 = fplass(nmnbn,nmplas,1) + a(1)
    f2 = fplass(nmnbn,nmplas,2) + a(2)
!
    if (abs(denom) .lt. 1.0d-3 * aux) then
        denom = b1(1)+b2(2)+b1(2)+b2(1)
!
        if (abs(denom) .lt. 1.0d-3 * sqrt(aux)) then
            cier=3
            call r8inir(6, 0.0d0, cdepsp, 1)
            goto 40
        endif
!
        lambda(1,1) = (f1 + f2) / denom
        lambda(2,2) = lambda(1,1)
    else
        lambda(1,1) = (f1*b2(2)-f2*b2(1))/denom
        lambda(2,2) = (f2*b1(1)-f1*b1(2))/denom
    endif
!
    call matmul(dfu, lambda, 6, 2, 2,&
                depsp2)
!
    do 30, j = 1,6
    cdepsp(j) = depsp2(j,1)+depsp2(j,2)
    30 end do
!
!     CALCUL DE CNBN ET DEPSP2 QUAND DEUX CRITERES PLAST SONT ACTIVES
    call nmnet2(zimat, nmnbn, cnbn, cplas, czef,&
                czeg, cief, cdeps, cdtg, cier,&
                dc1, dc2, depsp2, normm)
!
40  continue
!
    rpara(1) = czef
    rpara(2) = czeg
    rpara(3) = normm
!
end subroutine
