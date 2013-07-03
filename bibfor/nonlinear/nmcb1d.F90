subroutine nmcb1d(e0, labord, sigm, varm, epsm,&
                  deps, esout, sigp, varp, crit,&
                  option)
! ----------------------------------------------------------------------
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
!
! ----------------------------------------------------------------------
!
!              LOI DE LABORDERIE EN 1D
!
! ----------------------------------------------------------------------
    implicit       none
#include "asterc/r8prem.h"
#include "asterfort/nmcb13.h"
#include "asterfort/nmcb2.h"
    character(len=*) :: option
    real(kind=8) :: e0, sigm, epsm, deps, esout, sigp
    real(kind=8) :: labord(*), varm(*), varp(*), crit(*)
!
! ----------------------------------------------------------------------
!  IN :
!     E0       : MODULE D'YOUNG INITIAL
!     LABORD   : LES 9 COEFFICIENTS DE LA LOI, DANS CET ORDRE
!                    Y01,Y02,A1,A2,B1,B2,BETA1,BETA2,SIGF1
!     SIGM     : CONTRAINTE A L'INSTANT MOINS
!     VARM     : VARIABLES INTERNES A L'INSTANT MOINS
!     EPSM     : DEFORMATION TOTALE A L'INSTANT MOINS
!     DEPS     : INCREMENT DE DEFORMATION TOTALE
!     CRIT     : CRITERES DE CONVERGENGE
!     OPTION   : FULL_MECA,      MISE A JOUR DE MAT VI SIG
!                RAPH_MECA       MISE A JOUR DE     VI SIG
!                RIGI_MECA_TANG, MISE A JOUR DE MAT
!
!  OUT :
!     ESOUT    : MODULE SECANT OU TANGENT
!     SIGP     : CONTRAINTE A L'INSTANT PLUS
!     VARP     : VARIABLES INTERNES A L'INSTANT PLUS
!
! ----------------------------------------------------------------------
    logical :: rigi, resi
    real(kind=8) :: y01, y02, a1, a2, b1, b2, beta1, beta2, sigf1, esec
    real(kind=8) :: d2, d1, ylim1, ylim2, sig, d1m, d2m, y1, y2, z1, z2
    real(kind=8) :: eps, x1, x2, epsf, un
    integer :: cas, ccas
    parameter  (un=1.0d+0)
! ----------------------------------------------------------------------
!
!     RIGI_MECA_TANG ->       DSIDEP       -->  RIGI
!     FULL_MECA      ->  SIG  DSIDEP  VIP  -->  RIGI  RESI
!     RAPH_MECA      ->  SIG          VIP  -->        RESI
    rigi = (option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL')
    resi = (option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL')
    rigi=.true.
!
! --- DEFORMATION MECANIQUE TOTALE
    eps = epsm + deps
!
! --- VARIABLES INTERNES
    d1m = varm(1)
    d2m = varm(2)
    ylim1 = varm(3)
    ylim2 = varm(4)
!
! --- CARACTERISTIQUES MATERIAUX
    y01 = abs( labord(1) )
    y02 = abs( labord(2) )
    a1 = abs( labord(3) )
    a2 = abs( labord(4) )
    b1 = abs( labord(5) )
    b2 = abs( labord(6) )
    beta1 = abs( labord(7) )
    beta2 = -abs( labord(8) )
    sigf1 = abs( labord(9) )
!
! --- AU DEBUT DU PAS DE TEMPS, ON SOUSTRAIT 1 POUR QUE L'ENDOMMAGEMENT
!     N'EVOLUE PLUS A CAUSE DU TEST DE DEPASSEMENT DE SEUIL
    y1 = ylim1 - un
    y2 = ylim2 - un
!
!     ON PREND LE MAX POUR EVITER QUE Z1 ET Z2 SOIT "NAN"
    z1=max(ylim1 - un , y01)
    z2=max(ylim2 - un , y02)
!
! --- ON DUPLIQUE L'ENDOMMAGEMENT :
    d1=d1m
    d2=d2m
!
! --- BORNES DES DEFORMATIONS INITIALES
    x1 = (beta1*d1/(un-d1)+beta2*d2/(un-d2))/e0
    x2 = (beta2*d2-sigf1)/(un-d2)/e0
! --- BOUCLE TANT QUE L'ON CHANGE DE CAS
10  continue
!        TRAITEMENT EN FONCTION DE LA DEFORMATION
    if (x1 .le. eps) then
!           TRACTION
        cas = 1
        epsf = eps - beta2*d2/(un-d2)/e0
        call nmcb13(epsf, sig, esec, e0, d1m,&
                    d1, beta1, a1, b1, y1,&
                    y01, z1, crit)
    else if (eps.le.x2) then
!           COMPRESSION AU DELA DE LA FERMETURE DES FISSURES
        cas = 3
        call nmcb13(eps, sig, esec, e0, d2m,&
                    d2, beta2, a2, b2, y2,&
                    y02, z2, crit)
    else
!           FAIBLE COMPRESSION
        cas = 2
        call nmcb2(eps, beta1, e0, d1m, sigf1,&
                   beta2, d2m, sig)
    endif
!        BORNES DES DEFORMATIONS FINALES
    x1 = (beta1*d1/(un-d1)+beta2*d2/(un-d2))/e0
    x2 = (beta2*d2-sigf1)/(un-d2)/e0
    if (x1 .le. eps) then
        ccas = 1
    else if (eps.le.x2) then
        ccas = 3
    else
        ccas = 2
    endif
    if (ccas .ne. cas) goto 10
! --- FIN BOUCLE
!
    if (rigi) then
! ------ MODULE TANGENT NUMERIQUE + 10% D'INITIAL
        if (abs(deps) .gt. r8prem()) then
            esout = min( (sig-sigm)/deps+e0*0.10d+0 , e0 )
        else
            if (abs(varm(5)) .gt. r8prem()) then
                esout = varm(5)
            else
                esout = e0
            endif
        endif
    endif
!
    if (resi) then
! ------ CONTRAINTE
        sigp = sig
! ------ VARIABLES INTERNES REACTUALISEES
        if (y1 .gt. varm(3)) then
            varp(1) = d1
            varp(3) = y1
        else
            varp(1) = varm(1)
            varp(3) = varm(3)
        endif
        if (y2 .gt. varm(4)) then
            varp(2) = d2
            varp(4) = y2
        else
            varp(2) = varm(2)
            varp(4) = varm(4)
        endif
        varp(5) = esout
    endif
end subroutine
