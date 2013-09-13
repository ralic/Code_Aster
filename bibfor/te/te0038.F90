subroutine te0038(option, nomte)
! aslint: disable=
    implicit none
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterfort/angvxy.h"
#include "asterfort/carcou.h"
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/matrot.h"
#include "asterfort/normev.h"
#include "asterfort/pmat.h"
#include "asterfort/pmfitg.h"
#include "asterfort/provec.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecael.h"
#include "asterfort/trigom.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vdiff.h"
    character(len=*) :: option, nomte
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
!     CALCULE DES TERMES PROPRES A UN STRUCTURE  (ELEMENTS DE POUTRE)
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!       'MASS_INER      : CALCUL DES CARACTERISTIQUES DE STRUCTURES
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!       'MECA_POU_D_E'  : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!       'MECA_POU_D_T'  : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!       'MECA_POU_C_T'  : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
!       'MECA_POU_D_EM' : POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
!       'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!       'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!                         MULTI-FIBRES SECTION CONSTANTE
!     ------------------------------------------------------------------
!
!
    integer :: codres
    character(len=8) :: materi
    character(len=16) :: ch16, phenom
    real(kind=8) :: rho, a1, iy1, iz1, a2, cdg(3), ab2, ab3, ab4, amb, apb, ep
    real(kind=8) :: rad, ang, angarc, angs2, xfl, xl, xl2, matinl(6)
    real(kind=8) :: matine(6), pgl(3, 3), pgl1(3, 3), pgl2(3, 3), angl(3)
    real(kind=8) :: p1(3, 3), p2(3, 3), p3(3, 3), cdgl(3), xfly, xflz, r8b
    real(kind=8) :: pgl3(3, 3), pi, po, poxi2, rayon, rext, rint, rmoy, rr
    real(kind=8) :: ry1, ry2, rz1, rz2, theta, unpr2, unpr4, unprr, xa, xb, xi
    real(kind=8) :: xig, xisl, xixx, xixz, xizz, xzig, yig, zero, zig
    real(kind=8) :: pgl4(3, 3)
    real(kind=8) :: t1(3), t2(3), norme1, norme2, n(3), normen, x3(3), y3(3)
    real(kind=8) :: coo1(3), coo2(3), coo3(3), prec, omega
    real(kind=8) :: casect(6), casec1(6), val
!
    integer :: lmater, lx, lorien, nno, nc, lcastr, lsect, lsect2, itype, icoude
    integer :: i, n1, n2, lrcou, iadzi, iazk24, nn2
    integer :: inbf, jacf, icompo, isdcom, nbgf, ncarfi, ig
    integer :: nugf, icp, nbfig, ipos
!     ------------------------------------------------------------------
    prec = r8prem()
    zero = 0.0d0
!
!     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
!
    call jevech('PMATERC', 'L', lmater)
!
!
    if (( nomte.ne.'MECA_POU_D_EM' ) .and. ( nomte.ne.'MECA_POU_D_TGM')) then
        call rccoma(zi(lmater), 'ELAS', 1, phenom, codres)
!
        if (phenom .eq. 'ELAS' .or. phenom .eq. 'ELAS_ISTR' .or. phenom .eq. 'ELAS_FLUI'&
            .or. phenom .eq. 'ELAS_ORTH') then
            call rcvalb('FPG1', 1, 1, '+', zi(lmater),&
                        ' ', phenom, 0, ' ', r8b,&
                        1, 'RHO', rho, codres, 1)
        else
            call utmess('F', 'ELEMENTS_50')
        endif
    endif
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
!
    call jevech('PGEOMER', 'L', lx)
    lx = lx - 1
    xl = sqrt( (zr(lx+4)-zr(lx+1))**2 + (zr(lx+5)-zr(lx+2))**2 + (zr(lx+6)-zr(lx+3) )**2 )
    if (xl .eq. 0.d0) then
        ch16 = ' ?????????'
        call utmess('F', 'ELEMENTS2_80', sk=ch16(:8))
    endif
!
!
!     --- ORIENTATION DE LA POUTRE ---
!
    call jevech('PCAORIE', 'L', lorien)
    call matrot(zr(lorien), pgl)
    nno = 1
    nc = 3
!
    if (option .eq. 'MASS_INER') then
        call jevech('PMASSINE', 'E', lcastr)
        do 10 i = 1, 6
            matine(i) = 0.d0
            matinl(i) = 0.d0
10      continue
!
        if ((nomte.eq.'MECA_POU_D_EM') .or. (nomte.eq.'MECA_POU_D_TGM')) then
!           RECUPERATION DES CARACTERISTIQUES DES FIBRES :
            call jevech('PNBSP_I', 'L', inbf)
            nbgf=zi(inbf+1)
            call jevech('PFIBRES', 'L', jacf)
            ncarfi = 3
!           RECUPERATION DES MATERIAUX DANS SDCOMP DANS COMPOR
            call jevech('PCOMPOR', 'L', icompo)
            call jeveuo(zk16(icompo-1+7), 'L', isdcom)
        endif
!
        if ((nomte.ne.'MET3SEG3') .and. (nomte.ne.'MET6SEG3') .and. (nomte.ne.'MET3SEG4')) then
!           RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS
            call jevech('PCAGNPO', 'L', lsect)
            lsect = lsect - 1
            lsect2 = lsect + 11
            itype = nint(zr(lsect+23))
!           SECTION INITIALE
            a1 = zr(lsect+1)
            iy1 = zr(lsect+2)
            iz1 = zr(lsect+3)
            ry1 = zr(lsect+9)
            rz1 = zr(lsect+10)
!           SECTION FINALE
            a2 = zr(lsect2+1)
            ry2 = zr(lsect2+9)
            rz2 = zr(lsect2+10)
        else
!           RECUPERATION DES CARACTERISTIQUES  DES TUYAUX
            itype = -999
            call jevech('PCAGEPO', 'L', lsect)
            rext = zr(lsect)
            ep = zr(lsect+1)
            rint = rext - ep
            rmoy = rext - ep/2.d0
            pi = r8pi()
            a1 = pi* (rext*rext-rint*rint)
            iy1 = pi* (rext**4-rint**4)/4.d0
            iz1 = iy1
!
            call tecael(iadzi, iazk24)
            nn2 = zi(iadzi-1+2)
            call carcou(zr(lorien), xl, pgl, rayon, theta,&
                        pgl1, pgl2, pgl3, pgl4, nn2,&
                        omega, icoude)
            if (icoude .ge. 10) then
                icoude = icoude - 10
            endif
!
            if (icoude .eq. 1) xl = theta*rayon
            xl2 = xl*xl
            angs2 = theta/2.d0
!           CALCUL D'UN REPERE MOYEN
            if (nn2 .eq. 4) then
                do 20 i = 1, 3
                    angl(i) = 0.d0
                    coo1(i) = zr(lx+i)
                    coo2(i) = zr(lx+3+i)
                    coo3(i) = (zr(lx+6+i)+zr(lx+9+i))*0.5d0
20              continue
                call vdiff(3, coo3, coo1, t1)
                call vdiff(3, coo2, coo3, t2)
                call normev(t1, norme1)
                call normev(t2, norme2)
                call provec(t2, t1, n)
                call normev(n, normen)
                call vdiff(3, coo2, coo1, x3)
                call provec(x3, n, y3)
                call angvxy(x3, y3, angl)
                call matrot(angl, pgl3)
            endif
!
!           ------- CALCULS  -----------------------------
!           -------- MASSE
            zr(lcastr) = rho*a1*xl
!           -------- CDG
            cdgl(1) = 0.d0
            if (icoude .eq. 1) then
                cdgl(2) = -rayon* (&
                          sin(angs2)/angs2* (1.d0+ (rmoy* rmoy+ep*ep/4.d0)/ (2.d0*rayon**2))- cos&
                          &(angs2)&
                          )
            else
                cdgl(2) = 0.d0
            endif
            cdgl(3) = 0.d0
            n1 = 1
            n2 = 3
            if (icoude .eq. 1) then
                call utpvlg(n1, n2, pgl3, cdgl, cdg)
            else
                call utpvlg(n1, n2, pgl, cdgl, cdg)
            endif
            zr(lcastr+1) = cdg(1) + (zr(lx+4)+zr(lx+1))/2.d0
            zr(lcastr+2) = cdg(2) + (zr(lx+5)+zr(lx+2))/2.d0
            zr(lcastr+3) = cdg(3) + (zr(lx+6)+zr(lx+3))/2.d0
!           -------- INERTIE
!           --- INERTIE DE L'ELEMENT ---
            if (icoude .eq. 1) then
                xb = rayon*sin(angs2)/angs2* (1.d0+ (rmoy*rmoy+ep*ep/ 4.d0)/ (2.d0*rayon**2))
                matinl(1) = rho*xl* (&
                            iy1+ (a1*rayon**2+3*iz1)* (1.d0/2.d0+sin(theta)/ (4.d0*theta))) - zr(&
                            &lcastr&
                            )*xb* xb
                matinl(2) = 0.d0
                matinl(3) = rho*xl* (&
                            iy1+ (a1*rayon**2+3*iz1)* (1.d0/2.d0-sin(theta)/ (4.d0*theta)))
                matinl(4) = 0.d0
                matinl(5) = 0.d0
                matinl(6) = rho*xl*(a1*rayon**2+3*iz1)-zr(lcastr)*xb* xb
                call utpslg(nno, nc, pgl3, matinl, matine)
            else
                matinl(1) = rho* (iy1+iz1)*xl
                matinl(2) = 0.d0
                matinl(3) = rho*xl* (iy1+a1*xl2/12.d0)
                matinl(4) = 0.d0
                matinl(5) = 0.d0
                matinl(6) = rho*xl* (iz1+a1*xl2/12.d0)
                call utpslg(nno, nc, pgl, matinl, matine)
            endif
        endif
!
!     --- ORIENTATION DE LA POUTRE ---
        if (nomte .eq. 'MECA_POU_C_T') then
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
            angs2 = trigom('ASIN', xl/ (2.d0*rad) )
            ang = angs2*2.d0
            xl = rad*ang
            iy1 = iy1/xfly
            iz1 = iz1/xflz
            angl(1) = zr(lorien)
            angl(2) = zr(lorien+1)
            angl(3) = angarc
            call matrot(angl, p1)
            angl(1) = 0.d0
            angl(2) = 0.d0
            angl(3) = zr(lorien+2)
            call matrot(angl, p2)
            call pmat(3, p2, p1, p3)
        endif
!
!     --- CALCUL DES CARACTERISTIQUES ELEMENTAIRES 'MASS_INER' ----
        matinl(3) = iy1
        matinl(6) = iz1
        xl2 = xl*xl
!
!        --- POUTRE A SECTION CONSTANTE ---
        if (itype .eq. 0) then
!           -------- MASSE
            if (nomte .eq. 'MECA_POU_D_EM' .or. nomte .eq. 'MECA_POU_D_TGM') then
                do 15 i = 1, 6
                    casect(i) = zero
15              continue
!              BOUCLE SUR LES GROUPES DE FIBRE
                ipos=jacf
                do 100 ig = 1, nbgf
                    nugf=zi(inbf+1+ig)
                    icp=isdcom-1+(nugf-1)*6
                    read(zk24(icp+6),'(I24)')nbfig
                    materi=zk24(icp+2)(1:8)
!                 CALCUL DES CARACTERISTIQUES DU GROUPE ---
                    call pmfitg(nbfig, ncarfi, zr(ipos), casec1)
!                 ON MULTIPLIE PAR RHO (CONSTANT SUR LE GROUPE)
                    call rcvalb('FPG1', 1, 1, '+', zi(lmater),&
                                materi, 'ELAS', 0, ' ', zero,&
                                1, 'RHO', val, codres, 0)
                    if (codres .eq. 1) then
                        call rcvalb('FPG1', 1, 1, '+', zi(lmater),&
                                    materi, 'ELAS_FLUI', 0, ' ', zero,&
                                    1, 'RHO', val, codres, 1)
                    endif
                    do 25 i = 1, 6
                        casect(i) = casect(i) + val*casec1(i)
25                  continue
                    ipos=ipos+nbfig*ncarfi
100              continue
!
                zr(lcastr) = casect(1)*xl
!           -------- CDG
                zr(lcastr+1) = (zr(lx+4)+zr(lx+1))/2.d0
                zr(lcastr+2) = (zr(lx+5)+zr(lx+2))/2.d0
                zr(lcastr+3) = (zr(lx+6)+zr(lx+3))/2.d0
!           -------- INERTIE
                matinl(1) = (casect(4)+casect(5))*xl
                matinl(2) = 0.d0
                matinl(3) = xl*casect(5)+ casect(1)*xl*xl2/12.d0
                matinl(4) = 0.d0
                matinl(5) = 0.d0
                matinl(6) = xl*casect(4)+ casect(1)*xl*xl2/12.d0
            else
                zr(lcastr) = rho*a1*xl
!           -------- CDG
                zr(lcastr+1) = (zr(lx+4)+zr(lx+1))/2.d0
                zr(lcastr+2) = (zr(lx+5)+zr(lx+2))/2.d0
                zr(lcastr+3) = (zr(lx+6)+zr(lx+3))/2.d0
!           -------- INERTIE
                matinl(1) = rho* (iy1+iz1)*xl
                matinl(2) = 0.d0
                matinl(3) = rho*xl* (iy1+a1*xl2/12.d0)
                matinl(4) = 0.d0
                matinl(5) = 0.d0
                matinl(6) = rho*xl* (iz1+a1*xl2/12.d0)
            endif
            call utpslg(nno, nc, pgl, matinl, matine)
!
!        --- POUTRE A SECTION VARIABLE AFFINE ---
        else if (itype.eq.1) then
            if ((abs(a1- (4.d0*ry1*rz1)).gt. (a1*prec)) .or.&
                (abs(a2- (4.d0*ry2*rz2)).gt. (a2*prec))) then
                call utmess('F', 'ELEMENTS2_81')
            endif
!           -------- MASSE
            zr(lcastr) = rho*xl* (a1+a2)/2.d0
!           -------- CDG
            xisl = (rz1+2.d0*rz2)/ (3.d0* (rz1+rz2))
            zr(lcastr+1) = zr(lx+1) + (zr(lx+4)-zr(lx+1))*xisl
            zr(lcastr+2) = zr(lx+2) + (zr(lx+5)-zr(lx+2))*xisl
            zr(lcastr+3) = zr(lx+3) + (zr(lx+6)-zr(lx+3))*xisl
!           -------- INERTIE
            xa = xl* (rz1+rz2)
            amb = rz1 - rz2
            apb = rz1 + rz2
            ab2 = rz1**2 + rz2**2 + 4.d0*rz1*rz2
            ab3 = rz1**3 + 3.d0*rz1**2*rz2 - 3.d0*rz1*rz2**2 - rz2**3
            ab4 = rz1**4 + rz2**4 + 2.d0*rz1*rz2* (rz1**2+rz2**2)
!     ------------------------------------------------------------------
            xixx = xl* (4.d0*ab4-2.d0*amb*ab3+amb**2*ab2)/ (18.d0*apb)
            xizz = xl**3*ab2/ (18.d0*apb)
            xixz = xl**2* (ab3-amb*ab2)/ (18.d0*apb)
            xig = rho* ((xa*2.d0*ry1**3/3.d0)+2.d0*ry1*xixx)
            yig = rho* (2.d0*ry1* (xixx+xizz))
            zig = rho* ((xa*2.d0*ry1**3/3.d0)+2.d0*ry1*xizz)
            xzig = rho* (2.d0*ry1*xixz)
            matinl(1) = xig
            matinl(2) = 0.d0
            matinl(3) = yig
            matinl(4) = xzig
            matinl(5) = 0.d0
            matinl(6) = zig
            call utpslg(nno, nc, pgl, matinl, matine)
!
!        --- POUTRE A SECTION VARIABLE HOMOTHETIQUE ---
        else if (itype.eq.2) then
            if (a1 .eq. 0.d0) then
                call utmess('F', 'ELEMENTS2_82')
            endif
!           -------- MASSE
            zr(lcastr) = rho* (a1+a2+sqrt(a1*a2))*xl/3.d0
!           -------- CDG
            rr = sqrt(a2/a1)
            unprr = 1.d0 + rr + rr**2
            xi = (1.d0+2.d0*rr+3.d0*rr**2)/ (4.d0*unprr)
            zr(lcastr+1) = zr(lx+1)* (1.d0-xi) + zr(lx+4)*xi
            zr(lcastr+2) = zr(lx+2)* (1.d0-xi) + zr(lx+5)*xi
            zr(lcastr+3) = zr(lx+3)* (1.d0-xi) + zr(lx+6)*xi
!           -------- INERTIE
            unpr4 = unprr + rr**3 + rr**4
            unpr2 = 1.d0 + 3.d0*rr + 6.d0*rr**2
            po = rho*xl*a1*unprr/3.d0
            xig = rho*xl*(iy1+iz1)*unpr4/5.d0
            poxi2 = rho*xl**3*a1*unpr2/30.d0 - po*xi**2*xl**2
            yig = rho*xl*iy1*unpr4/5.d0 + poxi2
            zig = rho*xl*iz1*unpr4/5.d0 + poxi2
            matinl(1) = xig
            matinl(2) = 0.d0
            matinl(3) = yig
            matinl(4) = 0.d0
            matinl(5) = 0.d0
            matinl(6) = zig
            call utpslg(nno, nc, pgl, matinl, matine)
!
!        --- POUTRE COURBE ---
        else if (itype.eq.10) then
!           -------- MASSE
            zr(lcastr) = rho*a1*xl
!           -------- CDG
            cdgl(1) = 0.d0
            cdgl(2) = rad* (sin(angs2)/angs2-cos(angs2))
            cdgl(3) = 0.d0
            n1 = 1
            n2 = 3
            call utpvlg(n1, n2, p3, cdgl, cdg)
            zr(lcastr+1) = cdg(1) + (zr(lx+4)+zr(lx+1))/2.d0
            zr(lcastr+2) = cdg(2) + (zr(lx+5)+zr(lx+2))/2.d0
            zr(lcastr+3) = cdg(3) + (zr(lx+6)+zr(lx+3))/2.d0
!           -------- INERTIE
            matinl(1) = rho* (iy1+iz1)*xl
            matinl(2) = 0.d0
            matinl(3) = rho*xl* (iy1+a1*xl2/12.d0)
            matinl(4) = 0.d0
            matinl(5) = 0.d0
            matinl(6) = rho*xl* (iz1+a1*xl2/12.d0)
            call utpslg(nno, nc, pgl, matinl, matine)
        endif
!
        zr(lcastr+3+1) = matine(1)
        zr(lcastr+3+2) = matine(3)
        zr(lcastr+3+3) = matine(6)
        zr(lcastr+3+4) = matine(2)
        zr(lcastr+3+5) = matine(4)
        zr(lcastr+3+6) = matine(5)
!
    else
        ch16 = option
        call utmess('F', 'ELEMENTS2_84', sk=ch16)
    endif
!
end subroutine
