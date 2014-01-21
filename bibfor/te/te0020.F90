subroutine te0020(nomopt, nomte)
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/matro2.h"
#include "asterfort/matrot.h"
#include "asterfort/pmfitg.h"
#include "asterfort/pmfitx.h"
#include "asterfort/ptka10.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecael.h"
#include "asterfort/trigom.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvlg.h"
!     ------------------------------------------------------------------
    character(len=16) :: nomte, nomopt
! ----------------------------------------------------------------------
! IN OPTION    : K16 :  OPTION DE CALCUL
!     'CHAR_MECA_EPSI_R'
! IN NOMTE     : K16 : NOM DU TYPE ELEMENT
!     POUTRES DROITE D'EULER
!        'MECA_POU_D_E'   : SECTION VARIABLE
!        'MECA_POU_D_EM'  : SECTION MULTIFIBRES
!     POUTRE DROITE DE TIMOSHENKO
!        'MECA_POU_D_T'   : SECTION VARIABLE
!        'MECA_POU_D_TG'  : AVEC GAUCHISSEMENT
!        'MECA_POU_D_TGM' : AVEC GAUCHISSEMENT, SECTION MULTIFIBRES
!     POUTRE COURBE DE TIMOSHENKO
!        'MECA_POU_C_T'   : SECTION CONSTANTE
!     ------------------------------------------------------------------
    integer :: nbres
    parameter     (nbres=4)
    integer :: nbfib, ind, i, j, codres(nbres)
    integer :: lmater, inbfib, lsect, jacf, lsect2, idefi, ivectu
    integer :: lx, iadzi, iazk24, lrcou, lorien, nnoc, nno, nc
    real(kind=8) :: valres(nbres), zero, un, deux, r8bid, matk(78)
    real(kind=8) :: e, xnu, g, carsec(6), fs(14), bsm(12, 12)
    real(kind=8) :: a, xiy, xiz, alfay, alfaz, xjx
    real(kind=8) :: a2, xiy2, xiz2
    real(kind=8) :: epx, xky, xkz
    real(kind=8) :: rad, angarc, xfl, xfly, xflz, angs2, ang, xl
    real(kind=8) :: along, de(12), pgl(3, 3), pgl1(3, 3), pgl2(3, 3)
    character(len=8) :: nomres(nbres), nomail
    parameter (zero=0.0d0, un=1.0d0, deux = 2.0d0)
!     ------------------------------------------------------------------
!
    ASSERT(nomopt.eq.'CHAR_MECA_EPSI_R')
!
    nc = 6
!
    call jevech('PMATERC', 'L', lmater)
    call jevech('PCAGNPO', 'L', lsect)
    call jevech('PEPSINR', 'L', idefi)
    call jevech('PVECTUR', 'E', ivectu)
    do  i = 1, nbres
        valres(i) = zero
    enddo
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'ALPHA'
    nomres(4) = 'RHO'
    call rcvalb('FPG1', 1, 1, '+', zi(lmater),&
                ' ', 'ELAS', 0, ' ', [zero],&
                4, nomres, valres, codres, 0)
!     call rcvalb('FPG1', 1, 1, '+', zi(lmater),&
!                 ' ', 'ELAS', 0, ' ', [zero],&
!                 2, nomres(3), valres(3), codres(3), 0)
    if (codres(3) .ne. 0) valres(3) = zero
    if (codres(4) .ne. 0) valres(4) = zero
    e = valres(1)
    xnu = valres(2)
    g = e/ (deux* (un+xnu))
!
    lsect = lsect - 1
!   --- SECTION INITIALE ---
    if (nomte .eq. 'MECA_POU_D_TGM') then
        call jevech('PNBSP_I', 'L', inbfib)
        nbfib = zi(inbfib)
        call jevech('PFIBRES', 'L', jacf)
        call pmfitg(nbfib, 3, zr(jacf), carsec)
        a = carsec(1)
        xiy = carsec(5)
        xiz = carsec(4)
    else
        a = zr(lsect+1)
        xiy = zr(lsect+2)
        xiz = zr(lsect+3)
    endif
    alfay = zr(lsect+4)
    alfaz = zr(lsect+5)
    xjx = zr(lsect+8)
!
    if ((nomte.ne.'MECA_POU_D_TG') .and. ( nomte.ne.'MECA_POU_D_TGM')) then
!   --- SECTION FINALE ---
        lsect2 = lsect + 11
        a2 = zr(lsect2+1)
        xiy2 = zr(lsect2+2)
        xiz2 = zr(lsect2+3)
    endif
    epx = zr(idefi)
    xky = zr(idefi+1)
    xkz = zr(idefi+2)
    if (nomte .ne. 'MECA_POU_C_T') then
        fs(1) = e*a*epx
        fs(2) = 0.d0
        fs(3) = 0.d0
        fs(4) = 0.d0
        fs(5) = e*xiy*xky
        fs(6) = e*xiz*xkz
        if ((nomte.eq.'MECA_POU_D_TG')) then
            fs( 7) = 0.d0
            fs( 8) = e*a*epx
            fs( 9) = 0.d0
            fs(10) = 0.d0
            fs(11) = 0.d0
            fs(12) = e*xiy*xky
            fs(13) = e*xiz*xkz
            fs(14) = 0.d0
!
            nc = 7
        else if (nomte.eq.'MECA_POU_D_TGM') then
!           RECUPERATION DES CARACTERISTIQUES DES FIBRES
            call pmfitx(zi(lmater), 1, carsec, r8bid)
            fs(1) = carsec(1)*epx
            fs(5) = carsec(5)*xky
            fs(6) = carsec(4)*xkz
            fs( 7) = 0.d0
            fs( 8) = fs(1)
            fs( 9) = 0.d0
            fs(10) = 0.d0
            fs(11) = 0.d0
            fs(12) = fs(5)
            fs(13) = fs(6)
            fs(14) = 0.d0
!
            nc = 7
        else if (nomte.eq.'MECA_POU_D_EM') then
!           RECUPERATION DES CARACTERISTIQUES DES FIBRES
            call pmfitx(zi(lmater), 1, carsec, r8bid)
            fs(1) = carsec(1)*epx
            fs(5) = carsec(5)*xky
            fs(6) = carsec(4)*xkz
            fs(7) = fs(1)
            fs(8) = 0.d0
            fs(9) = 0.d0
            fs(10) = 0.d0
            fs(11) = fs(5)
            fs(12) = fs(6)
        else
            fs( 7) = e*a2*epx
            fs( 8) = 0.d0
            fs( 9) = 0.d0
            fs(10) = 0.d0
            fs(11) = e*xiy2*xky
            fs(12) = e*xiz2*xkz
        endif
        fs(1) = -fs(1)
        fs(2) = -fs(2)
        fs(3) = -fs(3)
        fs(4) = -fs(4)
        fs(5) = -fs(5)
        fs(6) = -fs(6)
    else
!       COORDONNEES DES NOEUDS
        call jevech('PGEOMER', 'L', lx)
        lx = lx - 1
        xl = sqrt(&
                ( zr(lx+4)-zr(lx+1))**2 + (zr(lx+5)-zr(lx+2) )**2 + (zr(lx+6)-zr(lx+3) )**2)
        if (xl .eq. zero) then
            call tecael(iadzi, iazk24)
            nomail = zk24(iazk24-1+3)(1:8)
            call utmess('F', 'ELEMENTS2_43', sk=nomail)
        endif
        call jevech('PCAARPO', 'L', lrcou)
        rad = zr(lrcou)
        angarc = zr(lrcou+1)
        xfl = zr(lrcou+2)
        xfly = xfl
        xflz = xfl
        if (xfl .eq. zero) then
            xfly = zr(lrcou+4)
            xflz = zr(lrcou+6)
        endif
        angs2 = trigom('ASIN',xl/ (deux*rad))
        ang = angs2*deux
        xl = rad*ang
        xiy = xiy/xfly
        xiz = xiz/xflz
        xiy2 = xiy2/xfly
        xiz2 = xiz2/xflz
        call ptka10(matk, e, a, xiy, xiz,&
                    xjx, g, alfay, alfaz, rad,&
                    ang, 1)
!       REMPLISSAGE DE LA MATRICE CARREE
        ind = 0
        do i = 1, 12
            fs(i) = zero
            de(i) = zero
            do j = 1, i - 1
                ind = ind + 1
                bsm(i,j) = matk(ind)
                bsm(j,i) = matk(ind)
            enddo
            ind = ind + 1
            bsm(i,i) = matk(ind)
        enddo
        along = deux*rad*epx*sin(angs2)
        de(1) = -along*cos(angs2)
        de(2) = along*sin(angs2)
        de(7) = -de(1)
        de(8) = de(2)
!       CALCUL DES FORCES INDUITES ---
        do i = 1, 6
            fs(i) = 0.d0
            fs(i+6) = 0.d0
            do j = 1, 6
                fs(i) = fs(i) + bsm(i,j)*de(j)
                fs(i+6) = fs(i+6) + bsm(i+6,j+6)*de(j+6)
            enddo
        enddo
    endif
!   RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA
    call jevech('PCAORIE', 'L', lorien)
!   MATRICE DE ROTATION MGL
    if (nomte .eq. 'MECA_POU_C_T') then
!       POUTRE COURBE DE TIMOSKENKO A 6 DDL: COORDONNEES DES NOEUDS
        call matro2(zr(lorien), angarc, angs2, pgl1, pgl2)
        nnoc = 1
        call utpvlg(nnoc, nc, pgl1, fs, zr(ivectu))
        call utpvlg(nnoc, nc, pgl2, fs(7), zr(ivectu+6))
    else
        nno = 2
        call matrot(zr(lorien), pgl)
        call utpvlg(nno, nc, pgl, fs, zr(ivectu))
    endif
end subroutine
