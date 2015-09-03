subroutine posigr(nomte, efge, sigm)
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
! --------------------------------------------------------------------------------------------------
!
! Calcul du vecteur élémentaire contrainte réel pour les éléments de poutre EULER et TIMOSHENKO
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
!
    character(len=*) :: nomte
    real(kind=8) :: sigm(*), efge(12)
!
#include "jeveux.h"
#include "asterfort/tecach.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/utmess.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: itsec, lrcou, iret
    real(kind=8) :: a, a2, hy1, hy2, hz1, hz2,r1, r2
    real(kind=8) :: zero, deux
    real(kind=8) :: smf1, smf2, smfy1, smfy2, smfz1, smfz2, sn1, sn2
    real(kind=8) :: xiy, xiy2, xiz, xiz2
    real(kind=8) :: xfly, xflz, xsiy, xsiz, xxy, xxz
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_cara = 6
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'A1','IY1','IZ1','A2','IY2','IZ2'/
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_cara1 = 7
    real(kind=8) :: vale_cara1(nb_cara1)
    character(len=8) :: noms_cara1(nb_cara1)
    data noms_cara1 /'HY1','HZ1','HY2','HZ2','R1','R2','TSEC'/
!
! --------------------------------------------------------------------------------------------------
!
    zero = 0.d0
    deux = 2.d0
!
!   Récuperation des caractéristiques générales des sections
    call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
!   Section initiale
    a      = vale_cara(1)
    xiy    = vale_cara(2)
    xiz    = vale_cara(3)
!   Section finale
    a2     = vale_cara(4)
    xiy2   = vale_cara(5)
    xiz2   = vale_cara(6)
!
    if (nomte .eq. 'MECA_POU_D_TG') then
        a2=a
    else if (nomte .eq. 'MECA_POU_D_T') then
        call tecach('ONN', 'PCAARPO', 'L', iret, iad=lrcou)
        if ( iret .eq. 0 ) then
            xfly = zr(lrcou)
            xsiy = zr(lrcou+1)
            xflz = zr(lrcou+2)
            xsiz = zr(lrcou+3)
!           prise en compte de l'indice de flexibilité
            xiy  = xiy/xfly
            xiz  = xiz/xflz
            xiy2 = xiy2/xfly
            xiz2 = xiz2/xflz
!           prise en compte de l'indice de contraintes
            xxy  = xsiy/xfly
            xxz  = xsiz/xflz
            xiy  = xiy/xxy
            xiz  = xiz/xxz
            xiy2 = xiy2/xxy
            xiz2 = xiz2/xxz
        endif
    endif
!
!   caractéristiques des sections cercle et rectangle
    call poutre_modloc('CAGEPO', noms_cara1, nb_cara1, lvaleur=vale_cara1)
    itsec = nint(vale_cara1(7))
!
!   sxx calculé à partir des 2 flexions et de l'effort normal
    sn1 = -efge(1)/a
    sn2 =  efge(7)/a2
!
!   section rectangulaire: le max  et le min sont obtenus sur les coins
    if (itsec .eq. 1) then
        hy1 = vale_cara1(1)
        hz1 = vale_cara1(2)
        hy2 = vale_cara1(3)
        hz2 = vale_cara1(4)
        smfy1 = abs(efge(5)/xiy*hz1/deux)
        smfz1 = abs(efge(6)/xiz*hy1/deux)
        smfy2 = abs(efge(11)/xiy2*hz2/deux)
        smfz2 = abs(efge(12)/xiz2*hy2/deux)
        sigm(1) = sn1-smfy1-smfz1
        sigm(2) = sn1+smfy1+smfz1
        sigm(3) = sn2-smfy2-smfz2
        sigm(4) = sn2+smfy2+smfz2
!
!   section circulaire: xiy = xiz.
    else if (itsec.eq.2) then
!       formule utilisee :  a cos(t) + b sin(t) = R cos(t-s)
!                         avec R= sqrt(a^2+b^2) et tan(s)= b/a
!       donc max de a cos(t) + b sin(t) = R
!       et   min de a cos(t) + b sin(t) = -R
        r1 = vale_cara1(5)
        r2 = vale_cara1(6)
        smf1 = (r1/xiy)*sqrt(efge(5)**2+efge(6)**2)
        smf2 = (r2/xiy2)*sqrt(efge(11)**2+efge(12)**2)
        sigm(1) = sn1-smf1
        sigm(2) = sn1+smf1
        sigm(3) = sn2-smf2
        sigm(4) = sn2+smf2
!
!   section generale: interdit
    else if (itsec.eq.0) then
        call utmess('A', 'ELEMENTS4_4')
    endif
!
end subroutine
