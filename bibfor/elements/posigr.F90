subroutine posigr(nomte, efge, sigm)
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
    character(len=*) :: nomte
    real(kind=8) :: sigm(*)
    real(kind=8) :: efge(12)
!     ------------------------------------------------------------------
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
!     CALCUL DU VECTEUR ELEMENTAIRE CONTRAINTE REEL
!     POUR LES ELEMENTS DE POUTRE D'EULER ET DE TIMOSHENKO.
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: itsec, lrcou, lsecr, lsect, lsect2
    real(kind=8) :: a, a2, alfay, alfay2, alfaz, alfaz2, aredy
    real(kind=8) :: aredy2, aredz, aredz2, deux, hy1, hy2, hz1
    real(kind=8) :: hz2, r1, r2, rt, rt2, ry, ry2
    real(kind=8) :: rz, rz2, sgnsn1, sgnsn2, smf1, smf2, smfy1
    real(kind=8) :: smfy2, smfz1, smfz2, sn1, sn2, un, xfl
    real(kind=8) :: xfly, xflz, xiy, xiy2, xiz, xiz2, xjx
    real(kind=8) :: xjx2, xsi, xsiy, xsiz, xxy, xxz, zero
!
!-----------------------------------------------------------------------
    zero = 0.d0
    un = 1.d0
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
    alfay = zr(lsect+4)
    alfaz = zr(lsect+5)
    xjx = zr(lsect+8)
    ry = zr(lsect+9)
    rz = zr(lsect+10)
    rt = zr(lsect+11)
!     --- SECTION FINALE ---
    lsect2 = lsect + 11
    a2 = zr(lsect2+1)
    xiy2 = zr(lsect2+2)
    xiz2 = zr(lsect2+3)
    alfay2 = zr(lsect2+4)
    alfaz2 = zr(lsect2+5)
    xjx2 = zr(lsect2+8)
    ry2 = zr(lsect2+9)
    rz2 = zr(lsect2+10)
    rt2 = zr(lsect2+11)
!
!
    if (nomte .eq. 'MECA_POU_D_TG') a2=a
!
    if (nomte .eq. 'MECA_POU_D_E') then
        alfay = zero
        alfaz = zero
        alfay2 = zero
        alfaz2 = zero
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
!     --- CARACTERISTIQUES DES SECTIONS CERCLE ET RECTANGLE
!
    call jevech('PCAGEPO', 'L', lsecr)
    lsecr = lsecr - 1
    itsec = nint(zr(lsecr+13))
    if (itsec .eq. 1) then
!        --- SECTION RECTANGULAIRE SECTION INITIALE
        hy1 = zr(lsecr+1)
        hz1 = zr(lsecr+2)
!        --- SECTION RECTANGULAIRE SECTION FINALE
        hy2 = zr(lsecr+5)
        hz2 = zr(lsecr+6)
    else if (itsec.eq.2) then
!        --- SECTION CIRCULAIRE SECTIONS INITIALE ET FINALE
        r1 = zr(lsecr+9)
        r2 = zr(lsecr+11)
    endif
!
!      --- CARACTERISTIQUES DES SECTIONS D EXTREMITE DE L ELEMENT
!
    aredy = a
    aredz = a
    aredy2 = a2
    aredz2 = a2
    if (alfay .ne. zero) aredy = a/alfay
    if (alfaz .ne. zero) aredz = a/alfaz
    if (alfay2 .ne. zero) aredy2 = a2/alfay2
    if (alfaz2 .ne. zero) aredz2 = a2/alfaz2
!
!     SXX CALCULE A PARTIR DES 2 FLEXIONS ET DE L'EFFORT NORMAL
!     SYY = SZZ = SYZ = 0
!     SXY ET SXZ : DUS A L'EFFORT TRANCHANT ET A LA TORSION
!
    sigm(2) = zero
    sigm(3) = zero
    sigm(4) = - (efge(2)/aredy) - (efge(4)/xjx*rt)
    sigm(5) = - (efge(3)/aredz) - (efge(4)/xjx*rt)
    sigm(6) = zero
!
    sigm(8) = zero
    sigm(9) = zero
    sigm(10) = (efge(8)/aredy2) + (efge(10)/xjx2*rt2)
    sigm(11) = (efge(9)/aredz2) + (efge(10)/xjx2*rt2)
    sigm(12) = zero
!
!     CALCUL DES CONTRAINTES. SXX MAXI SUIVANT LA FORME DE SECTION
!     CONTRAINTES EN REPERE LOCAL
!
!     SXX CALCULE A PARTIR DES 2 FLEXIONS ET DE L'EFFORT NORMAL
!
!     ON CALCULE LE MAXI EN VALEUR ABSOLUE ET ON LUI AFFECTE LE
!     SIGNE DE L'EFFORT NORMAL
!
    sn1 = -efge(1)/a
    if (sn1 .ge. zero) then
        sgnsn1 = un
    else
        sgnsn1 = -1.d0
    endif
    sn2 = efge(7)/a2
    if (sn2 .ge. zero) then
        sgnsn2 = un
    else
        sgnsn2 = -1.d0
    endif
!
!     --- SECTION RECTANGULAIRE: LE MAX EST OBTENU SUR UN DES COINS
!         CE QUI REVIENT A PRENDRE LA VALEUR ABSOLUE (SIGNEE)
!
    if (itsec .eq. 1) then
        smfy1 = abs(efge(5)/xiy*hz1/deux)
        smfz1 = abs(efge(6)/xiz*hy1/deux)
        smfy2 = abs(efge(11)/xiy2*hz2/deux)
        smfz2 = abs(efge(12)/xiz2*hy2/deux)
        sigm(1) = sgnsn1* (abs(sn1)+smfy1+smfz1)
        sigm(7) = sgnsn2* (abs(sn2)+smfy2+smfz2)
!
!     --- SECTION CIRCULAIRE: XIY = XIZ.
!
    else if (itsec.eq.2) then
        smf1 = (r1/xiy)*sqrt(efge(5)**2+efge(6)**2)
        smf2 = (r2/xiy2)*sqrt(efge(11)**2+efge(12)**2)
        sigm(1) = sgnsn1* (abs(sn1)+smf1)
        sigm(7) = sgnsn2* (abs(sn2)+smf2)
!
!     --- SECTION GENERALE: ON DONNE SXX AU POINT (RY,RZ)
!
    else if (itsec.eq.0) then
        sigm(1) = -efge(1)/a - efge(5)/xiy*rz + efge(6)/xiz*ry
        sigm(7) = efge(7)/a2 + efge(11)/xiy2*rz2 - efge(12)/xiz2*ry2
    endif
!
end subroutine
