subroutine posigr(nomte, efge, sigm)
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/utmess.h"
    character(len=*) :: nomte
    real(kind=8) :: sigm(*)
    real(kind=8) :: efge(12)
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL DU VECTEUR ELEMENTAIRE CONTRAINTE REEL
!     POUR LES ELEMENTS DE POUTRE D'EULER ET DE TIMOSHENKO.
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: itsec, lrcou, lsecr, lsect, lsect2
    real(kind=8) :: a, a2, hy1, hy2, hz1, hz2,r1, r2
    real(kind=8) :: zero, deux
    real(kind=8) :: smf1, smf2, smfy1, smfy2, smfz1, smfz2, sn1, sn2
    real(kind=8) :: xiy, xiy2, xiz, xiz2
    real(kind=8) :: xfl, xfly, xflz, xsi, xsiy, xsiz, xxy, xxz
!
!-----------------------------------------------------------------------
    zero = 0.d0
    deux = 2.d0
!     ------------------------------------------------------------------
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
!
    call jevech('PCAGNPO', 'L', lsect)
    lsect = lsect - 1
!     --- SECTION INITIALE ---
    a = zr(lsect+1)
    xiy = zr(lsect+2)
    xiz = zr(lsect+3)
!     --- SECTION FINALE ---
    lsect2 = lsect + 11
    a2 = zr(lsect2+1)
    xiy2 = zr(lsect2+2)
    xiz2 = zr(lsect2+3)
!
    if (nomte .eq. 'MECA_POU_D_TG') then
        a2=a
!
    else if (nomte.eq.'MECA_POU_C_T') then
        call jevech('PCAARPO', 'L', lrcou)
        xfl = zr(lrcou+2)
        xsi = zr(lrcou+3)
        xfly = xfl
        xflz = xfl
        xsiy = xsi
        xsiz = xsi
        if (xfl .eq. zero) then
            xfly = zr(lrcou+4)
            xsiy = zr(lrcou+5)
            xflz = zr(lrcou+6)
            xsiz = zr(lrcou+7)
        endif
        xiy = xiy/xfly
        xiz = xiz/xflz
        xiy2 = xiy2/xfly
        xiz2 = xiz2/xflz
!
!        PRISE EN COMPTE DE L'INDICE DE CONTRAINTES
!
        xxy = xsiy/xfly
        xxz = xsiz/xflz
        xiy = xiy/xxy
        xiz = xiz/xxz
        xiy2 = xiy2/xxy
        xiz2 = xiz2/xxz
    endif
!
!   --- caracteristiques des sections cercle et rectangle
!
    call jevech('PCAGEPO', 'L', lsecr)
    lsecr = lsecr - 1
    itsec = nint(zr(lsecr+13))
    if (itsec .eq. 1) then
!   --- section rectangulaire section initiale
        hy1 = zr(lsecr+1)
        hz1 = zr(lsecr+2)
!   --- section rectangulaire section finale
        hy2 = zr(lsecr+5)
        hz2 = zr(lsecr+6)
    else if (itsec.eq.2) then
!   --- section circulaire sections initiale et finale
        r1 = zr(lsecr+9)
        r2 = zr(lsecr+11)
    endif
!
!   sxx calcule a partir des 2 flexions et de l'effort normal
!

    sn1 = -efge(1)/a
    sn2 = efge(7)/a2
!
!   --- section rectangulaire: le max  et le min sont obtenus sur les coins
!
    if (itsec .eq. 1) then
        smfy1 = abs(efge(5)/xiy*hz1/deux)
        smfz1 = abs(efge(6)/xiz*hy1/deux)
        smfy2 = abs(efge(11)/xiy2*hz2/deux)
        smfz2 = abs(efge(12)/xiz2*hy2/deux)
        sigm(1) = sn1-smfy1-smfz1
        sigm(2) = sn1+smfy1+smfz1
        sigm(3) = sn2-smfy2-smfz2
        sigm(4) = sn2+smfy2+smfz2
!
!   --- section circulaire: xiy = xiz.
!
    else if (itsec.eq.2) then
!       formule utilisee :  a cos(t) + b sin(t) = R cos(t-s)
!                         avec R= sqrt(a^2+b^2) et tan(s)= b/a
!       donc max de a cos(t) + b sin(t) = R
!       et   min de a cos(t) + b sin(t) = -R
        smf1 = (r1/xiy)*sqrt(efge(5)**2+efge(6)**2)
        smf2 = (r2/xiy2)*sqrt(efge(11)**2+efge(12)**2)
        sigm(1) = sn1-smf1
        sigm(2) = sn1+smf1
        sigm(3) = sn2-smf2
        sigm(4) = sn2+smf2
!
!   --- section generale: interdit
!
    else if (itsec.eq.0) then
        call utmess('A', 'ELEMENTS4_4')
    endif
!
end subroutine
