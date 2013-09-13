subroutine poefgr(nomte, klc, mater, e, xnu,&
                  rho, effo)
! aslint: disable=
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/matro2.h"
#include "asterfort/matrot.h"
#include "asterfort/pmavec.h"
#include "asterfort/pmfmas.h"
#include "asterfort/pomass.h"
#include "asterfort/ptforp.h"
#include "asterfort/trigom.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "asterfort/vecma.h"
#include "asterfort/verifm.h"
    character(len=*) :: nomte
    real(kind=8) :: klc(12, 12), e, xnu, rho, effo(*)
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
!
!     CALCUL DU VECTEUR ELEMENTAIRE EFFORT GENERALISE REEL,
!     POUR LES ELEMENTS DE POUTRE D'EULER ET DE TIMOSHENKO.
!     ------------------------------------------------------------------
!
    real(kind=8) :: ul(12), fe(12), mlv(78), mlc(12, 12), fei(12)
    real(kind=8) :: pgl(3, 3), pgl1(3, 3), pgl2(3, 3)
    character(len=24) :: suropt
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iret, itype, j, jdepl, kanl, ldyna
    integer :: lmater, lopt, lorien, lrcou, lsect, lsect2, lx
    integer :: mater, nc, ncc, nno, nnoc
    real(kind=8) :: a, a2, along, angarc, angs2, deux, f
    real(kind=8) :: rad, xl, zero
!-----------------------------------------------------------------------
    zero = 0.d0
    deux = 2.d0
    nno = 2
    nc = 6
    nnoc = 1
    ncc = 6
!     ------------------------------------------------------------------
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
!
    call jevech('PCAGNPO', 'L', lsect)
    lsect = lsect - 1
    lsect2 = lsect + 11
    itype = nint(zr(lsect+23))
    a = zr(lsect+1)
    a2 = zr(lsect2+1)
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call jevech('PGEOMER', 'L', lx)
    lx = lx - 1
    xl = sqrt( (zr(lx+4)-zr(lx+1))**2+ (zr(lx+5)-zr(lx+2))**2+ (zr(lx+6)-zr(lx+3))**2 )
    if (itype .eq. 10) then
        call jevech('PCAARPO', 'L', lrcou)
        rad = zr(lrcou)
        angarc = zr(lrcou+1)
        angs2 = trigom('ASIN',xl/ (deux*rad))
        xl = rad*angs2*deux
    endif
!
!     --- MATRICE DE ROTATION PGL
!
    call jevech('PCAORIE', 'L', lorien)
    if (itype .eq. 10) then
        call matro2(zr(lorien), angarc, angs2, pgl1, pgl2)
    else
        call matrot(zr(lorien), pgl)
    endif
!
!      --- VECTEUR DEPLACEMENT LOCAL ---
!
    call jevech('PDEPLAR', 'L', jdepl)
    if (itype .eq. 10) then
        call utpvgl(nnoc, ncc, pgl1, zr(jdepl), ul)
        call utpvgl(nnoc, ncc, pgl2, zr(jdepl+6), ul(7))
    else
        call utpvgl(nno, nc, pgl, zr(jdepl), ul)
    endif
!
!     --- VECTEUR EFFORT LOCAL  EFFO = KLC * UL
!
    call pmavec('ZERO', 12, klc, ul, effo)
!
!     --- TENIR COMPTE DES EFFORTS DUS A LA DILATATION ---
!
    call verifm('NOEU', nno, 1, '+', mater,&
                'ELAS', 1, f, iret)
!
    if (f .ne. zero) then
        do 10 i = 1, 12
            ul(i) = zero
10      continue
!
        if (itype .ne. 10) then
            ul(1) = -f*xl
            ul(7) = -ul(1)
        else
            along = deux*rad*f*sin(angs2)
            ul(1) = -along*cos(angs2)
            ul(2) = along*sin(angs2)
            ul(7) = -ul(1)
            ul(8) = ul(2)
        endif
!              --- CALCUL DES FORCES INDUITES ---
        do 30 i = 1, 6
            do 20 j = 1, 6
                effo(i) = effo(i) - klc(i,j)*ul(j)
                effo(i+6) = effo(i+6) - klc(i+6,j+6)*ul(j+6)
20          continue
30      continue
    endif
!
!     --- TENIR COMPTE DES EFFORTS REPARTIS/PESANTEUR ---
!
    call ptforp(itype, 'CHAR_MECA_PESA_R', nomte, a, a2,&
                xl, rad, angs2, 0, nno,&
                nc, pgl, pgl1, pgl2, fe,&
                fei)
    do 40 i = 1, 12
        effo(i) = effo(i) - fe(i)
40  end do
!
    call ptforp(itype, 'CHAR_MECA_FR1D1D', nomte, a, a2,&
                xl, rad, angs2, 0, nno,&
                nc, pgl, pgl1, pgl2, fe,&
                fei)
    do 50 i = 1, 12
        effo(i) = effo(i) - fe(i)
50  end do
!
    call ptforp(itype, 'CHAR_MECA_FF1D1D', nomte, a, a2,&
                xl, rad, angs2, 0, nno,&
                nc, pgl, pgl1, pgl2, fe,&
                fei)
    do 60 i = 1, 12
        effo(i) = effo(i) - fe(i)
60  end do
!
!      --- FORCE DYNAMIQUE ---
!
!      IF ( RHO .NE. ZERO ) THEN
    kanl = 2
    call jevech('PSUROPT', 'L', lopt)
    suropt = zk24(lopt)
    if (suropt(1:4) .eq. 'MASS') then
        if (suropt .eq. 'MASS_MECA_DIAG' .or. suropt .eq. 'MASS_MECA_EXPLI') kanl = 0
        if (suropt .eq. 'MASS_MECA     ') kanl = 1
        if (suropt .eq. 'MASS_FLUI_STRU') kanl = 1
        if (kanl .eq. 2) then
            call utmess('A', 'ELEMENTS2_44', sk=suropt)
        endif
        if (nomte(1:13) .eq. 'MECA_POU_D_EM') then
            call jevech('PMATERC', 'L', lmater)
            call pmfmas(nomte, ' ', 0.d0, zi(lmater), kanl,&
                        mlv)
        else
            call pomass(nomte, e, xnu, rho, kanl,&
                        mlv)
        endif
        call vecma(mlv, 78, mlc, 12)
        call jevech('PCHDYNR', 'L', ldyna)
        if (itype .eq. 10) then
            call utpvgl(nnoc, ncc, pgl1, zr(ldyna), ul)
            call utpvgl(nnoc, ncc, pgl2, zr(ldyna+6), ul(7))
        else
            call utpvgl(nno, nc, pgl, zr(ldyna), ul)
        endif
        call pmavec('ZERO', 12, mlc, ul, fe)
        do 70 i = 1, 12
            effo(i) = effo(i) + fe(i)
70      continue
    endif
!
end subroutine
