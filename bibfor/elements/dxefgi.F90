subroutine dxefgi(nomte, xyzl, pgl, epsini, sigt)
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
#include "jeveux.h"
!
#include "asterfort/dxmate.h"
#include "asterfort/u2mesk.h"
    character(len=16) :: nomte
    real(kind=8) :: xyzl(3, 1), pgl(3, 3)
    real(kind=8) :: epsini(6)
    real(kind=8) :: sigt(1)
!     ------------------------------------------------------------------
! --- EFFORTS GENERALISES D'ORIGINE THERMIQUE AUX POINTS D'INTEGRATION
! --- POUR LES ELEMENTS COQUES A FACETTES PLANES :
! --- DST, DKT, DSQ, DKQ, Q4G
! --- CALCULES A PARTIR D'UN CHAMP DE DEFORMATIONS INITIALES QUI EST
! --- POUR L'INSTANT CONSTANT PAR ELEMENT ET QUI NE PREND PAS EN
! --- COMPTE LES DEFORMATIONS INITIALES DE CISAILLEMENT TRANSVERSE.
!     ------------------------------------------------------------------
!     IN  NOMTE        : NOM DU TYPE D'ELEMENT
!     IN  XYZL(3,NNO)  : COORDONNEES DES CONNECTIVITES DE L'ELEMENT
!                        DANS LE REPERE LOCAL DE L'ELEMENT
!     IN  PGL(3,3)     : MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE
!                        LOCAL
!     IN  EPSINI(6)    : DEFORMATIONS INITIALES CONSTANTES SUR L'ELEMENT
!                        DANS L'ORDRE : EPXX, EPYY, EPXY, KXX, KYY, KXY
!     OUT SIGT(1)      : EFFORTS  GENERALISES D'ORIGINE THERMIQUE
!                        AUX POINTS D'INTEGRATION
    integer :: multic
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3), dc(2, 2), dci(2, 2)
    real(kind=8) :: dmc(3, 2), dfc(3, 2)
    real(kind=8) :: kxx, kyy, kxy, t2ev(4), t2ve(4), t1ve(9)
    logical :: coupmf
!     ------------------------------------------------------------------
!
! --- INITIALISATIONS :
!     -----------------
!-----------------------------------------------------------------------
    integer :: i, igau, nno, npg
    real(kind=8) :: epxx, epxy, epyy, zero
!-----------------------------------------------------------------------
    zero = 0.0d0
!
    do 10 i = 1, 32
        sigt(i) = zero
10  end do
!
    if (nomte .eq. 'MEDKTR3 ' .or. nomte .eq. 'MEDSTR3 ' .or. nomte .eq. 'MEDKTG3 ') then
!
        npg = 3
        nno = 3
!
        else if (nomte.eq.'MEDKQU4 ' .or. nomte.eq.'MEDSQU4 ' .or.&
    nomte.eq.'MEQ4QU4 ' .or. nomte.eq.'MEDKQG4 ') then
        npg = 4
        nno = 4
!
    else
        call u2mesk('F', 'ELEMENTS_14', 1, nomte(1:8))
    endif
!
! --- CALCUL DES MATRICES DE HOOKE DE FLEXION, MEMBRANE,
! --- MEMBRANE-FLEXION, CISAILLEMENT, CISAILLEMENT INVERSE
!     ----------------------------------------------------
!
    call dxmate('RIGI', df, dm, dmf, dc,&
                dci, dmc, dfc, nno, pgl,&
                multic, coupmf, t2ev, t2ve, t1ve)
!
! --- CHOIX DE NOTATIONS PLUS EXPLICITES POUR LES DEFORMATIONS
! --- INITIALES
!     ---------
    epxx = epsini(1)
    epyy = epsini(2)
    epxy = epsini(3)
    kxx = epsini(4)
    kyy = epsini(5)
    kxy = epsini(6)
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION
!     -----------------------------------
    do 20 igau = 1, npg
!
        sigt(1+8* (igau-1)) = dm(1,1)*epxx + dm(1,2)*epyy + dm(1,3)* epxy
        sigt(2+8* (igau-1)) = dm(2,1)*epxx + dm(2,2)*epyy + dm(2,3)* epxy
        sigt(3+8* (igau-1)) = dm(3,1)*epxx + dm(3,2)*epyy + dm(3,3)* epxy
!
        sigt(4+8* (igau-1)) = df(1,1)*kxx + df(1,2)*kyy + df(1,3)*kxy
        sigt(5+8* (igau-1)) = df(2,1)*kxx + df(2,2)*kyy + df(2,3)*kxy
        sigt(6+8* (igau-1)) = df(3,1)*kxx + df(3,2)*kyy + df(3,3)*kxy
!
        sigt(7+8* (igau-1)) = zero
        sigt(8+8* (igau-1)) = zero
20  end do
!
end subroutine
