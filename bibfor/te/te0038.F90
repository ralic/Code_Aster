subroutine te0038(option, nomte)
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
!     CALCULE DES TERMES PROPRES A UN STRUCTURE  (ELEMENTS DE POUTRE)
!
! --------------------------------------------------------------------------------------------------
!
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!       'MASS_INER      : CALCUL DES CARACTERISTIQUES DE STRUCTURES
!
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!       'MECA_POU_D_E'  : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!       'MECA_POU_D_T'  : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!       'MECA_POU_D_EM' : POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
!       'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!       'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!                         MULTI-FIBRES SECTION CONSTANTE
!
! --------------------------------------------------------------------------------------------------
!
! aslint: disable=
    implicit none
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterfort/angvxy.h"
#include "asterfort/carcou.h"
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/matrot.h"
#include "asterfort/normev.h"
#include "asterfort/pmat.h"
#include "asterfort/pmfitx.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/provec.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vdiff.h"
!
    character(len=*) :: option, nomte
!
! --------------------------------------------------------------------------------------------------
!
    integer :: codres(1)
    character(len=16) :: ch16, phenom
    real(kind=8) :: rho(1), a1, iy1, iz1, a2, cdg(3), ab2, ab3, ab4, amb, apb, ep
    real(kind=8) :: angs2, xl, xl2, matinl(6)
    real(kind=8) :: matine(6), pgl(3, 3), pgl1(3, 3), pgl2(3, 3), angl(3)
    real(kind=8) :: cdgl(3), xfly, xflz, r8b
    real(kind=8) :: pgl3(3, 3), pi, po, poxi2, rayon, rext, rint, rmoy, rr
    real(kind=8) :: ry1, ry2, rz1, rz2, theta, unpr2, unpr4, unprr, xa, xb, xi
    real(kind=8) :: xig, xisl, xixx, xixz, xizz, xzig, yig, zig
    real(kind=8) :: pgl4(3, 3)
    real(kind=8) :: t1(3), t2(3), norme1, norme2, n(3), normen, x3(3), y3(3)
    real(kind=8) :: coo1(3), coo2(3), coo3(3), prec, omega
    real(kind=8) :: casect(6), yg, zg, p1gl(3),p1gg (3), rbid
!
    integer :: lmater, igeom, lorien, nno, nc, lcastr, itype, icoude
    integer :: i, n1, n2, lrcou, iadzi, iazk24, nn2, iret
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_cara = 9
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'A1','IY1','IZ1','RY1','RZ1','A2','RY2','RZ2','TVAR'/
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_cara1 = 2
    real(kind=8) :: vale_cara1(nb_cara1)
    character(len=8) :: noms_cara1(nb_cara1)
    data noms_cara1 /'R1','EP1'/
!
! --------------------------------------------------------------------------------------------------
!
    prec = r8prem()
    r8b = 0.0d0
!
!   RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
    call jevech('PMATERC', 'L', lmater)
!
    if (( nomte.ne.'MECA_POU_D_EM' ) .and. ( nomte.ne.'MECA_POU_D_TGM')) then
        call rccoma(zi(lmater), 'ELAS', 1, phenom, codres(1))
!
        if ( (phenom.eq.'ELAS') .or. (phenom.eq.'ELAS_ISTR') .or. &
             (phenom.eq.'ELAS_FLUI') .or. (phenom .eq. 'ELAS_ORTH') ) then
            call rcvalb('FPG1', 1, 1, '+', zi(lmater), ' ', phenom, 0, ' ', [r8b],&
                        1, 'RHO', rho, codres, 1)
        else
            call utmess('F', 'ELEMENTS_50')
        endif
    endif
!
!   recuperation des coordonnees des noeuds
    xl = lonele(igeom=igeom)
!
!   orientation de la poutre
    call jevech('PCAORIE', 'L', lorien)
    call matrot(zr(lorien), pgl)
    nno = 1
    nc = 3
!
    if (option .eq. 'MASS_INER') then
        call jevech('PMASSINE', 'E', lcastr)
        matine(:) = 0.d0
        matinl(:) = 0.d0
!
        if ((nomte.ne.'MET3SEG3') .and. (nomte.ne.'MET6SEG3') .and. (nomte.ne.'MET3SEG4')) then
!           recuperation des caracteristiques generales des sections
            call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
            a1     = vale_cara(1)
            iy1    = vale_cara(2)
            iz1    = vale_cara(3)
            ry1    = vale_cara(4)
            rz1    = vale_cara(5)
            a2     = vale_cara(6)
            ry2    = vale_cara(7)
            rz2    = vale_cara(8)
            itype  = nint(vale_cara(9))
        else
!           recuperation des caracteristiques  des tuyaux
            itype = -999
            call poutre_modloc('CAGEP1', noms_cara1, nb_cara1, lvaleur=vale_cara1)
            rext  = vale_cara1(1)
            ep    = vale_cara1(2)
!
            rint = rext - ep
            rmoy = rext - ep/2.d0
            pi = r8pi()
            a1 = pi* (rext*rext-rint*rint)
            iy1 = pi* (rext**4-rint**4)/4.d0
            iz1 = iy1
!           Pour ne plus avoir les warning de compilation
            a2=a1; ry1=rext; ry2=rext; rz1=rext; rz2=rext
!
            call tecael(iadzi, iazk24, noms=0)
            nn2 = zi(iadzi-1+2)
            call carcou(zr(lorien), xl, pgl, rayon, theta,&
                        pgl1, pgl2, pgl3, pgl4, nn2, omega, icoude)
            if (icoude .ge. 10) then
                icoude = icoude - 10
            endif
!
            if (icoude .eq. 1) xl = theta*rayon
            xl2 = xl*xl
            angs2 = theta/2.d0
!           calcul d'un repere moyen
            if (nn2 .eq. 4) then
                do i = 1, 3
                    angl(i) = 0.d0
                    coo1(i) = zr(igeom+i)
                    coo2(i) = zr(igeom+3+i)
                    coo3(i) = (zr(igeom+6+i)+zr(igeom+9+i))*0.5d0
                enddo
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
!           calcul masse
            zr(lcastr) = rho(1)*a1*xl
!           calcul CDG
            cdgl(1) = 0.d0
            if (icoude .eq. 1) then
                xb = 1.0d0+(rmoy*rmoy+ep*ep/4.d0)/(2.d0*rayon**2)
                cdgl(2) = -rayon*(sin(angs2)/angs2*xb - cos(angs2))
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
            zr(lcastr+1) = cdg(1) + (zr(igeom+4)+zr(igeom+1))/2.d0
            zr(lcastr+2) = cdg(2) + (zr(igeom+5)+zr(igeom+2))/2.d0
            zr(lcastr+3) = cdg(3) + (zr(igeom+6)+zr(igeom+3))/2.d0
!           inertie de l'element
            if (icoude .eq. 1) then
                xa = (a1*rayon**2+3.0d0*iz1)
                xb = rayon*sin(angs2)/angs2*(1.d0+(rmoy*rmoy+ep*ep/4.d0)/(2.d0*rayon**2))
                matinl(1) = rho(1)*xl*(iy1+xa*(0.5d0+sin(theta)/(4.d0*theta))) - zr(lcastr)*xb*xb
                matinl(2) = 0.d0
                matinl(3) = rho(1)*xl*(iy1+xa*(0.5d0-sin(theta)/(4.d0*theta)))
                matinl(4) = 0.d0
                matinl(5) = 0.d0
                matinl(6) = rho(1)*xl*xa-zr(lcastr)*xb*xb
                call utpslg(nno, nc, pgl3, matinl, matine)
            else
                matinl(1) = rho(1)*(iy1+iz1)*xl
                matinl(2) = 0.d0
                matinl(3) = rho(1)*xl*(iy1+a1*xl2/12.d0)
                matinl(4) = 0.d0
                matinl(5) = 0.d0
                matinl(6) = rho(1)*xl*(iz1+a1*xl2/12.d0)
                call utpslg(nno, nc, pgl, matinl, matine)
            endif
        endif
!
!       caracteristique de coude pour les poutres
        if (nomte .eq. 'MECA_POU_D_T') then
            call tecach('ONN', 'PCAARPO', 'L', iret, iad=lrcou)
            if ( iret .eq. 0 ) then
                xfly = zr(lrcou)
                xflz = zr(lrcou+2)
                iy1  = iy1/xfly
                iz1  = iz1/xflz
            endif
        endif
!
!       calcul des caracteristiques elementaires 'MASS_INER'
        matinl(3) = iy1
        matinl(6) = iz1
        xl2 = xl*xl
!
        if (itype .eq. 0) then
!           poutre a section constante
!           masse
            if (nomte .eq. 'MECA_POU_D_EM' .or. nomte .eq. 'MECA_POU_D_TGM') then
                call pmfitx(zi(lmater), 2, casect, rbid)
                zr(lcastr) = casect(1)*xl
!               correction excentrement
                if (casect(1).gt.prec)then
                    yg=casect(2)/casect(1)
                    zg=casect(3)/casect(1)
                    p1gl(1)=xl/2.d0
                    p1gl(2)=yg
                    p1gl(3)=zg
                    call utpvlg(1,3,pgl,p1gl,p1gg)
                    iy1=casect(5)-casect(1)*zg*zg
                    iz1=casect(4)-casect(1)*yg*yg
!                   cdg
                    zr(lcastr+1) =  zr(igeom+1)+p1gg(1)
                    zr(lcastr+2) =  zr(igeom+2)+p1gg(2)
                    zr(lcastr+3) =  zr(igeom+3)+p1gg(3)
                else
!                   cdg
                    zr(lcastr+1) = (zr(igeom+4)+zr(igeom+1))/2.d0
                    zr(lcastr+2) = (zr(igeom+5)+zr(igeom+2))/2.d0
                    zr(lcastr+3) = (zr(igeom+6)+zr(igeom+3))/2.d0
                endif
!               inertie
                matinl(1) = (iy1+iz1)*xl
                matinl(2) = 0.d0
                matinl(3) = xl*iy1+ casect(1)*xl*xl2/12.d0
                matinl(4) = 0.d0
                matinl(5) = 0.d0
                matinl(6) = xl*iz1+ casect(1)*xl*xl2/12.d0
            else
                zr(lcastr) = rho(1)*a1*xl
!               cdg
                zr(lcastr+1) = (zr(igeom+4)+zr(igeom+1))/2.d0
                zr(lcastr+2) = (zr(igeom+5)+zr(igeom+2))/2.d0
                zr(lcastr+3) = (zr(igeom+6)+zr(igeom+3))/2.d0
!               inertie
                matinl(1) = rho(1)* (iy1+iz1)*xl
                matinl(2) = 0.d0
                matinl(3) = rho(1)*xl* (iy1+a1*xl2/12.d0)
                matinl(4) = 0.d0
                matinl(5) = 0.d0
                matinl(6) = rho(1)*xl* (iz1+a1*xl2/12.d0)
            endif
            call utpslg(nno, nc, pgl, matinl, matine)
!
        else if (itype.eq.1) then
!           poutre a section variable affine
            if ((abs(a1- (4.d0*ry1*rz1)).gt. (a1*prec)) .or.&
                (abs(a2- (4.d0*ry2*rz2)).gt. (a2*prec))) then
                call utmess('F', 'ELEMENTS2_81')
            endif
!           masse
            zr(lcastr) = rho(1)*xl* (a1+a2)/2.d0
!           cdg
            xisl = (rz1+2.d0*rz2)/ (3.d0* (rz1+rz2))
            zr(lcastr+1) = zr(igeom+1) + (zr(igeom+4)-zr(igeom+1))*xisl
            zr(lcastr+2) = zr(igeom+2) + (zr(igeom+5)-zr(igeom+2))*xisl
            zr(lcastr+3) = zr(igeom+3) + (zr(igeom+6)-zr(igeom+3))*xisl
!           inertie
            xa  = xl*(rz1+rz2)
            amb = rz1 - rz2
            apb = rz1 + rz2
            ab2 = rz1**2 + rz2**2 + 4.d0*rz1*rz2
            ab3 = rz1**3 + 3.d0*rz1**2*rz2 - 3.d0*rz1*rz2**2 - rz2**3
            ab4 = rz1**4 + rz2**4 + 2.d0*rz1*rz2* (rz1**2+rz2**2)
!
            xixx = xl*(4.d0*ab4-2.d0*amb*ab3+amb**2*ab2)/(18.d0*apb)
            xizz = (xl**3)*ab2/(18.d0*apb)
            xixz = (xl**2)*(ab3-amb*ab2)/ (18.d0*apb)
            xig  = rho(1)*((xa*2.d0*ry1**3/3.d0)+2.d0*ry1*xixx)
            yig  = rho(1)*(2.d0*ry1* (xixx+xizz))
            zig  = rho(1)*((xa*2.d0*ry1**3/3.d0)+2.d0*ry1*xizz)
            xzig = rho(1)*(2.d0*ry1*xixz)
            matinl(1) = xig
            matinl(2) = 0.d0
            matinl(3) = yig
            matinl(4) = xzig
            matinl(5) = 0.d0
            matinl(6) = zig
            call utpslg(nno, nc, pgl, matinl, matine)
!
        else if (itype.eq.2) then
!           poutre a section variable homothetique
            if (a1 .eq. 0.d0) then
                call utmess('F', 'ELEMENTS2_82')
            endif
!           masse
            zr(lcastr) = rho(1)*(a1+a2+sqrt(a1*a2))*xl/3.d0
!           CDG
            rr = sqrt(a2/a1)
            unprr = 1.d0 + rr + rr**2
            xi = (1.d0 + 2.d0*rr + 3.d0*(rr**2))/(4.d0*unprr)
            zr(lcastr+1) = zr(igeom+1)*(1.d0-xi) + zr(igeom+4)*xi
            zr(lcastr+2) = zr(igeom+2)*(1.d0-xi) + zr(igeom+5)*xi
            zr(lcastr+3) = zr(igeom+3)*(1.d0-xi) + zr(igeom+6)*xi
!           inertie
            unpr4 = unprr + rr**3 + rr**4
            unpr2 = 1.d0 + 3.d0*rr + 6.d0*rr**2
            po    = rho(1)*xl*a1*unprr/3.d0
            xig   = rho(1)*xl*(iy1+iz1)*unpr4/5.d0
            poxi2 = rho(1)*(xl**3)*a1*unpr2/30.d0 - po*((xi*xl)**2)
            yig   = rho(1)*xl*iy1*unpr4/5.d0 + poxi2
            zig   = rho(1)*xl*iz1*unpr4/5.d0 + poxi2
            matinl(1) = xig
            matinl(2) = 0.d0
            matinl(3) = yig
            matinl(4) = 0.d0
            matinl(5) = 0.d0
            matinl(6) = zig
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
end subroutine
