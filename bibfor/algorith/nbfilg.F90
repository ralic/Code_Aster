subroutine nbfilg(ndim, nno1, nno2, nno3, npg,&
                  iw, vff1, vff2, vff3, idff1,&
                  vu, vg, vp, geomi, typmod,&
                  option, mate, compor, lgpg, crit,&
                  instm, instp, ddlm, ddld, angmas,&
                  sigm, vim, sigp, vip, resi,&
                  rigi, vect, matr, matsym, codret)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: sebastien.fayolle at edf.fr
! aslint: disable=W1306,W1504
    implicit none
!
#include "asterf_types.h"
#include "asterfort/codere.h"
#include "asterfort/dfdmip.h"
#include "asterfort/dsde2d.h"
#include "asterfort/nirela.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmepsi.h"
#include "asterfort/nmmalu.h"
#include "asterfort/pmat.h"
#include "asterfort/poslog.h"
#include "asterfort/prelog.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
#include "blas/dscal.h"
    aster_logical :: resi, rigi, matsym
    integer :: ndim, nno1, nno2, nno3, npg, iw, idff1, lgpg
    integer :: mate
    integer :: vu(3, 27), vg(27), vp(27)
    integer :: codret
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg), vff3(nno3, npg)
    real(kind=8) :: instm, instp
    real(kind=8) :: geomi(ndim, nno1), ddlm(*), ddld(*), angmas(*)
    real(kind=8) :: sigm(2*ndim, npg), sigp(2*ndim, npg)
    real(kind=8) :: vim(lgpg, npg), vip(lgpg, npg)
    real(kind=8) :: vect(*), matr(*)
    real(kind=8) :: crit(*)
    character(len=8) :: typmod(*)
    character(len=16) :: compor(*), option
!-----------------------------------------------------------------------
!          CALCUL DES FORCES INTERNES POUR LES ELEMENTS
!          INCOMPRESSIBLES POUR LES GRANDES DEFORMATIONS
!          3D/D_PLAN/AXIS
!          ROUTINE APPELEE PAR TE0590
!-----------------------------------------------------------------------
! IN  RESI    : CALCUL DES FORCES INTERNES
! IN  RIGI    : CALCUL DE LA MATRICE DE RIGIDITE
! IN  MATSYM  : MATRICE TANGENTE SYMETRIQUE OU NON
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  NNO1    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AUX DEPLACEMENTS
! IN  NNO2    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AU GONFLEMENT
! IN  NNO3    : NOMBRE DE NOEUDS DE L'ELEMENT LIES A LA PRESSION
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  IW      : POIDS DES POINTS DE GAUSS
! IN  VFF1    : VALEUR  DES FONCTIONS DE FORME LIES AUX DEPLACEMENTS
! IN  VFF2    : VALEUR  DES FONCTIONS DE FORME LIES AU GONFLEMENT
! IN  VFF3    : VALEUR  DES FONCTIONS DE FORME LIES A LA PRESSION
! IN  IDFF1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  VU      : TABLEAU DES INDICES DES DDL DE DEPLACEMENTS
! IN  VG      : TABLEAU DES INDICES DES DDL DE GONFLEMENT
! IN  VP      : TABLEAU DES INDICES DES DDL DE PRESSION
! IN  GEOMI   : COORDONEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  OPTION  : OPTION DE CALCUL
! IN  MATE    : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTM   : INSTANT PRECEDENT
! IN  INSTP   : INSTANT DE CALCUL
! IN  DDLM    : DEGRES DE LIBERTE A L'INSTANT PRECEDENT
! IN  DDLD    : INCREMENT DES DEGRES DE LIBERTE
! IN  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
! OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
! OUT VECT    : FORCES INTERNES
! OUT MATR    : MATRICE DE RIGIDITE (RIGI_MECA_TANG ET FULL_MECA)
! OUT CODRET  : CODE RETOUR
!-----------------------------------------------------------------------
!
    aster_logical :: axi, grand
    integer :: g, nddl, ndu
    integer :: ia, na, ra, sa, ib, nb, rb, sb, ja, jb
    integer :: lij(3, 3), vij(3, 3), os, kk
    integer :: viaja, vibjb, vuiana, vgra, vpsa
    integer :: cod(27)
    real(kind=8) :: geomm(3*27), geomp(3*27), deplm(3*27), deplp(3*27)
    real(kind=8) :: r, w, wp, dff1(nno1, 4)
    real(kind=8) :: presm(27), presd(27)
    real(kind=8) :: gonfm(27), gonfd(27)
    real(kind=8) :: gm, gd, gp, pm, pd, pp
    real(kind=8) :: fm(3, 3), jm, ftm(3, 3), corm, epsm(6), epsml(6)
    real(kind=8) :: fp(3, 3), jp, ftp(3, 3), corp, epsp(6), deps(6)
    real(kind=8) :: gn(3, 3), lamb(3), logl(3)
    real(kind=8) :: tn(6), tp(6), dtde(6, 6)
    real(kind=8) :: pk2(6), pk2m(6)
    real(kind=8) :: taup(6), taudv(6), tauhy, tauldc(6)
    real(kind=8) :: dsidep(6, 6)
    real(kind=8) :: d(6, 6), ddev(6, 6), devd(6, 6), dddev(6, 6)
    real(kind=8) :: iddid, devdi(6), iddev(6)
    real(kind=8) :: ftr(3, 3), t1, t2
    real(kind=8) :: idev(6, 6), kr(6)
    real(kind=8) :: tampon(10), id(3, 3), rbid(1)
    real(kind=8) :: am, ap, bp, boa, aa, bb, daa, dbb, dboa, d2boa
!
    parameter    (grand = .true._1)
    data         vij  / 1, 4, 5,&
     &                  4, 2, 6,&
     &                  5, 6, 3 /
    data         kr   / 1.d0, 1.d0, 1.d0, 0.d0, 0.d0, 0.d0/
    data         id   / 1.d0, 0.d0, 0.d0,&
     &                  0.d0, 1.d0, 0.d0,&
     &                  0.d0, 0.d0, 1.d0/
    data         idev / 2.d0,-1.d0,-1.d0, 0.d0, 0.d0, 0.d0,&
     &                 -1.d0, 2.d0,-1.d0, 0.d0, 0.d0, 0.d0,&
     &                 -1.d0,-1.d0, 2.d0, 0.d0, 0.d0, 0.d0,&
     &                  0.d0, 0.d0, 0.d0, 3.d0, 0.d0, 0.d0,&
     &                  0.d0, 0.d0, 0.d0, 0.d0, 3.d0, 0.d0,&
     &                  0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 3.d0/
!-----------------------------------------------------------------------
!
! - INITIALISATION
    axi = typmod(1).eq.'AXIS'
    nddl = nno1*ndim + nno2 + nno3
    ndu = ndim
    if (axi) ndu = 3
!
! - REACTUALISATION DE LA GEOMETRIE ET EXTRACTION DES CHAMPS
    do na = 1, nno1
        do ia = 1, ndim
            geomm(ia+ndim*(na-1)) = geomi(ia,na) + ddlm(vu(ia,na))
            geomp(ia+ndim*(na-1)) = geomm(ia+ndim*(na-1))+ddld(vu(ia, na))
            deplm(ia+ndim*(na-1)) = ddlm(vu(ia,na))
            deplp(ia+ndim*(na-1)) = ddlm(vu(ia,na))+ddld(vu(ia,na))
        end do
    end do
!
    do ra = 1, nno2
        gonfm(ra) = ddlm(vg(ra))
        gonfd(ra) = ddld(vg(ra))
    end do
!
    do sa = 1, nno3
        presm(sa) = ddlm(vp(sa))
        presd(sa) = ddld(vp(sa))
    end do
!
    if (resi) call r8inir(nddl, 0.d0, vect, 1)
    if (rigi) then
        if (matsym) then
            call r8inir(nddl*(nddl+1)/2, 0.d0, matr, 1)
        else
            call r8inir(nddl*nddl, 0.d0, matr, 1)
        endif
    endif
!
    call r8inir(36, 0.d0, dsidep, 1)
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
    do g = 1, npg
!
! - CALCUL DES DEFORMATIONS
        call dfdmip(ndim, nno1, axi, geomi, g,&
                    iw, vff1(1, g), idff1, r, w,&
                    dff1)
        call nmepsi(ndim, nno1, axi, grand, vff1(1, g),&
                    r, dff1, deplm, fm, epsm)
        call nmepsi(ndim, nno1, axi, grand, vff1(1, g),&
                    r, dff1, deplp, fp, epsp)
        call dfdmip(ndim, nno1, axi, geomp, g,&
                    iw, vff1(1, g), idff1, r, wp,&
                    dff1)
!
        call nmmalu(nno1, axi, r, vff1(1, g), dff1,&
                    lij)
!
        jm = fm(1,1)*(fm(2,2)*fm(3,3)-fm(2,3)*fm(3,2)) - fm(2,1)*(fm(1,2)*fm(3,3)-fm(1,3)*fm(3,2)&
             &) + fm(3,1)*(fm(1,2)*fm(2,3)-fm(1,3)*fm(2,2))
        jp = fp(1,1)*(fp(2,2)*fp(3,3)-fp(2,3)*fp(3,2)) - fp(2,1)*(fp(1,2)*fp(3,3)-fp(1,3)*fp(3,2)&
             &) + fp(3,1)*(fp(1,2)*fp(2,3)-fp(1,3)*fp(2,2))
!
        if (jp .le. 0.d0) then
            codret = 1
            goto 999
        endif
!
! - CALCUL DE LA PRESSION ET DU GONFLEMENT AU POINT DE GAUSS
        gm = ddot(nno2,vff2(1,g),1,gonfm,1)
        gd = ddot(nno2,vff2(1,g),1,gonfd,1)
        gp = gm+gd
!
        pm = ddot(nno3,vff3(1,g),1,presm,1)
        pd = ddot(nno3,vff3(1,g),1,presd,1)
        pp = pm+pd
!
! - CALCUL DES FONCTIONS A, B,... DETERMINANT LA RELATION LIANT G ET J
        call nirela(2, jp, gm, gp, am,&
                    ap, bp, boa, aa, bb,&
                    daa, dbb, dboa, d2boa)
!
! - CALCUL DES DEFORMATIONS ENRICHIES
        corm = (am/jm)**(1.d0/3.d0)
        call dcopy(9, fm, 1, ftm, 1)
        call dscal(9, corm, ftm, 1)
!
        corp = (ap/jp)**(1.d0/3.d0)
        call dcopy(9, fp, 1, ftp, 1)
        call dscal(9, corp, ftp, 1)
!
! - APPEL A LA LOI DE COMPORTEMENT
        cod(g) = 0
        call r8inir(36, 0.d0, dtde, 1)
        call r8inir(6, 0.d0, tp, 1)
        call r8inir(6, 0.d0, taup, 1)
!
        call prelog(ndim, lgpg, vim(1, g), gn, lamb,&
                    logl, ftm, ftp, epsml, deps,&
                    tn, resi, cod(g))
!
        call nmcomp('RIGI', g, 1, ndim, typmod,&
                    mate, compor, crit, instm, instp,&
                    6, epsml, deps, 6, tn,&
                    vim(1, g), option, angmas, 10, tampon,&
                    tp, vip(1, g), 36, dtde, 1,&
                    rbid, cod(g))
!
! - DSIDEP = 2dS/dC = dS/dE_GL
!
        call poslog(resi, rigi, tn, tp, ftm,&
                    lgpg, vip(1, g), ndim, ftp, g,&
                    dtde, sigm(1, g), .false._1, 'RIGI', mate,&
                    instp, angmas, gn, lamb, logl,&
                    sigp(1, g), dsidep, pk2m, pk2, cod(g))
!
        if (cod(g) .eq. 1) then
            codret = 1
            if (.not. resi) then
                call utmess('F', 'ALGORITH14_75')
            endif
            goto 999
        endif
!
! - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
        if (resi) then
            call dscal(2*ndim, exp(gp), sigp(1, g), 1)
            call dcopy(2*ndim, sigp(1, g), 1, taup, 1)
            call dscal(2*ndim, 1.d0/jp, sigp(1, g), 1)
!
! - CONTRAINTE HYDROSTATIQUE ET DEVIATEUR
            tauhy = (taup(1)+taup(2)+taup(3))/3.d0
            do ia = 1, 6
                taudv(ia) = taup(ia) - tauhy*kr(ia)
            end do
!
! - VECTEUR FINT:U
            do na = 1, nno1
                do ia = 1, ndu
                    kk = vu(ia,na)
                    t1 = 0.d0
                    do ja = 1, ndu
                        t2 = taudv(vij(ia,ja)) + pp*bb*id(ia,ja)
                        t1 = t1 + t2*dff1(na,lij(ia,ja))
                    end do
                    vect(kk) = vect(kk) + w*t1
                end do
            end do
!
! - VECTEUR FINT:G
            t2 = tauhy*aa - pp*dboa
            do ra = 1, nno2
                kk = vg(ra)
                t1 = vff2(ra,g)*t2
                vect(kk) = vect(kk) + w*t1
            end do
!
! - VECTEUR FINT:P
            t2 = bp - boa
            do sa = 1, nno3
                kk = vp(sa)
                t1 = vff3(sa,g)*t2
                vect(kk) = vect(kk) + w*t1
            end do
        endif
!
! - MATRICE TANGENTE
        if (rigi) then
            if (resi) then
                call dcopy(9, ftp, 1, ftr, 1)
            else
                call dcopy(2*ndim, sigm(1,g), 1, taup, 1)
                call dscal(2*ndim, jm, taup, 1)
                call dcopy(9, ftm, 1, ftr, 1)
            endif
!
! - CALCUL DE L'OPERATEUR TANGENT SYMÉTRISÉ D
            call dsde2d(3, ftr, dsidep, d)
!
            call pmat(6, idev/3.d0, d, devd)
            call pmat(6, d, idev/3.d0, ddev)
            call pmat(6, devd, idev/3.d0, dddev)
!
! - CALCUL DU TENSEUR DE CONTRAINTE : TRACE ET PARTIE DEVIATORIQUE
            tauhy = (taup(1)+taup(2)+taup(3))/3.d0
!
! - CALCUL DE D^DEV:ID ET ID:D^DEV ET ID:D:ID
            iddid = 0.d0
            do ia = 1, 6
                devdi(ia) = devd(ia,1)+devd(ia,2)+devd(ia,3)
                iddev(ia) = ddev(1,ia)+ddev(2,ia)+ddev(3,ia)
                taudv(ia) = taup(ia) - tauhy*kr(ia)
                tauldc(ia) = taup(ia) + (pp*bb-tauhy)*kr(ia)
                do ja = 1, 3
                    iddid = iddid+kr(ia)*d(ia,ja)
                end do
            end do
!
            if (matsym) then
! - MATRICE SYMETRIQUE
! - TERME K:UX
                do na = 1, nno1
                    do ia = 1, ndu
                        vuiana = vu(ia,na)
                        os = (vuiana-1)*vuiana/2
!
! - TERME K:UU      KUU(NDIM,NNO1,NDIM,NNO1)
                        do nb = 1, nno1
                            do ib = 1, ndu
                                if (vu(ib,nb) .le. vuiana) then
                                    kk = os+vu(ib,nb)
                                    t1 = 0.d0
! - RIGIDITE DE COMPORTEMENT
                                    do ja = 1, ndu
                                        viaja=vij(ia,ja)
                                        do jb = 1, ndu
                                            vibjb=vij(ib,jb)
                                            t2 = dddev(viaja,vibjb)
                                            t2 = t2 + taup(vij(ia,jb))*kr(vij(ib,ja))
                                            t2 = t2 + taup(vij(jb,ja))*kr(vij(ia,ib))
                                            t2 = t2 - 2.d0/3.d0*(&
                                                 taup(viaja)*kr(vibjb)+taup(vibjb)*kr(viaja))
                                            t2 = t2 + 2.d0/3.d0*tauhy*kr(viaja)*kr(vibjb)
                                            t1 = t1+dff1(na,lij(ia,ja))*t2*dff1(nb,lij(ib,jb))
                                        end do
                                    end do
!
                                    t2 = pp*jp*dbb
                                    t1 = t1+dff1(na,lij(ia,ia))*t2*dff1(nb,lij(ib,ib))
!
! - RIGIDITE GEOMETRIQUE
                                    do jb = 1, ndu
                                        t1 = t1 - dff1(&
                                             na, lij(ia, ib))*dff1(nb,&
                                             lij(ib, jb)) *tauldc(vij(ia, jb)&
                                             )
                                    end do
                                    matr(kk) = matr(kk) + w*t1
                                endif
                            end do
                        end do
!
! - TERME K:UG      KUG(NDIM,NNO1,NNO2)
                        t1 = 0.d0
                        do ja = 1, ndu
                            viaja=vij(ia,ja)
                            t2 = (devdi(viaja)+2.d0*taudv(viaja))
                            t1 = t1 + dff1(na,lij(ia,ja))*t2
                        end do
                        t1 = t1*aa/3.d0
!
                        do rb = 1, nno2
                            if (vg(rb) .lt. vuiana) then
                                kk = os + vg(rb)
                                matr(kk) = matr(kk) + w*t1*vff2(rb,g)
                            endif
                        end do
!
! - TERME K:UP      KUP(NDIM,NNO1,NNO3)
                        do sb = 1, nno3
                            if (vp(sb) .lt. vuiana) then
                                kk = os + vp(sb)
                                t1 = dff1(na,lij(ia,ia))*bb*vff3(sb,g)
                                matr(kk) = matr(kk) + w*t1
                            endif
                        end do
                    end do
                end do
!
! - TERME K:GX
                do ra = 1, nno2
                    vgra = vg(ra)
                    os = (vgra-1)*vgra/2
!
! - TERME K:GU      KGU(NDIM,NNO2,NNO1)
                    do nb = 1, nno1
                        do ib = 1, ndu
                            if (vu(ib,nb) .lt. vgra) then
                                kk = os + vu(ib,nb)
                                t1 = 0.d0
                                do jb = 1, ndu
                                    vibjb=vij(ib,jb)
                                    t2 = (iddev(vibjb)+2.d0*taudv( vibjb))
                                    t1 = t1 + t2*dff1(nb,lij(ib,jb))
                                end do
                                matr(kk) = matr(kk) + w*t1*aa*vff2(ra,g)/ 3.d0
                            endif
                        end do
                    end do
!
! - TERME K:GG      KGG(NNO2,NNO2)
                    t2 = (iddid/9.d0+2.d0*tauhy/3.d0)*aa**2 - pp*d2boa + tauhy*daa
                    do rb = 1, nno2
                        if (vg(rb) .le. vgra) then
                            kk = os + vg(rb)
                            t1 = vff2(ra,g)*t2*vff2(rb,g)
                            matr(kk) = matr(kk) + w*t1
                        endif
                    end do
!
! - TERME K:GP      KGP(NNO2,NNO3)
                    do sb = 1, nno3
                        if (vp(sb) .lt. vgra) then
                            kk = os + vp(sb)
                            t1 = - vff2(ra,g)*dboa*vff3(sb,g)
                            matr(kk) = matr(kk) + w*t1
                        endif
                    end do
                end do
!
! - TERME K:PX
                do sa = 1, nno3
                    vpsa = vp(sa)
                    os = (vpsa-1)*vpsa/2
!
! - TERME K:PU      KPU(NDIM,NNO3,NNO1)
                    do nb = 1, nno1
                        do ib = 1, ndu
                            if (vu(ib,nb) .lt. vpsa) then
                                kk = os + vu(ib,nb)
                                t1 = vff3(sa,g)*bb*dff1(nb,lij(ib,ib))
                                matr(kk) = matr(kk) + w*t1
                            endif
                        end do
                    end do
!
! - TERME K:PG      KPG(NNO3,NNO2)
                    do rb = 1, nno2
                        if (vg(rb) .lt. vpsa) then
                            kk = os + vg(rb)
                            t1 = - vff3(sa,g)*dboa*vff2(rb,g)
                            matr(kk) = matr(kk) + w*t1
                        endif
                    end do
!
! - TERME K:PP = 0.D0      KPP(NNO3,NNO3)
                end do
!
            else
! - MATRICE NON SYMETRIQUE
! - TERME K:UX
                do na = 1, nno1
                    do ia = 1, ndu
                        os = (vu(ia,na)-1)*nddl
!
! - TERME K:UU      KUU(NDIM,NNO1,NDIM,NNO1)
                        do nb = 1, nno1
                            do ib = 1, ndu
                                kk = os+vu(ib,nb)
                                t1 = 0.d0
! - RIGIDITE DE COMPORTEMENT
                                do ja = 1, ndu
                                    viaja=vij(ia,ja)
                                    do jb = 1, ndu
                                        vibjb=vij(ib,jb)
                                        t2 = dddev(viaja,vibjb)
                                        t2 = t2 + taup(vij(ia,jb))*kr(vij(ib,ja))
                                        t2 = t2 + taup(vij(jb,ja))*kr(vij(ia,ib))
                                        t2 = t2 - 2.d0/3.d0*(&
                                             taup(viaja)*kr(vibjb)+kr(viaja)*taup(vibjb))
                                        t2 = t2 + 2.d0*kr(viaja)*kr(vibjb)*tauhy/3.d0
                                        t1 = t1+dff1(na,lij(ia,ja))*t2*dff1(nb,lij(ib,jb))
                                    end do
                                end do
!
                                t2 = pp*jp*dbb
                                t1 = t1+dff1(na,lij(ia,ia))*t2*dff1(nb,lij(ib,ib))
!
! - RIGIDITE GEOMETRIQUE
                                do jb = 1, ndu
                                    t1 = t1 - dff1(&
                                         na,lij(ia,ib))*dff1(nb,lij(ib,jb)) * tauldc(vij(ia,jb))
                                end do
                                matr(kk) = matr(kk) + w*t1
                            end do
                        end do
!
! - TERME K:UG      KUG(NDIM,NNO1,NNO2)
                        t1 = 0.d0
                        do ja = 1, ndu
                            viaja=vij(ia,ja)
                            t2 = (devdi(viaja)+2.d0*taudv(viaja))
                            t1 = t1 + dff1(na,lij(ia,ja))*t2
                        end do
                        t1 = t1*aa/3.d0
!
                        do rb = 1, nno2
                            kk = os + vg(rb)
                            matr(kk) = matr(kk) + w*t1*vff2(rb,g)
                        end do
!
! - TERME K:UP      KUP(NDIM,NNO1,NNO3)
                        do sb = 1, nno3
                            kk = os + vp(sb)
                            t1 = dff1(na,lij(ia,ia))*bb*vff3(sb,g)
                            matr(kk) = matr(kk) + w*t1
                        end do
                    end do
                end do
!
! - TERME K:GX
                do ra = 1, nno2
                    os = (vg(ra)-1)*nddl
!
! - TERME K:GU      KGU(NDIM,NNO2,NNO1)
                    do nb = 1, nno1
                        do ib = 1, ndu
                            kk = os + vu(ib,nb)
                            t1 = 0.d0
                            do jb = 1, ndu
                                vibjb=vij(ib,jb)
                                t2 = (iddev(vibjb)+2.d0*taudv(vibjb))
                                t1 = t1 + t2*dff1(nb,lij(ib,jb))
                            end do
                            matr(kk) = matr(kk) + w*t1*aa*vff2(ra,g)/3.d0
                        end do
                    end do
!
! - TERME K:GG      KGG(NNO2,NNO2)
                    t2 = (iddid/9.d0+2.d0*tauhy/3.d0)*aa**2 - pp*d2boa + tauhy*daa
                    do rb = 1, nno2
                        kk = os + vg(rb)
                        t1 = vff2(ra,g)*t2*vff2(rb,g)
                        matr(kk) = matr(kk) + w*t1
                    end do
!
! - TERME K:GP      KGP(NNO2,NNO3)
                    do sb = 1, nno3
                        kk = os + vp(sb)
                        t1 = - vff2(ra,g)*dboa*vff3(sb,g)
                        matr(kk) = matr(kk) + w*t1
                    end do
                end do
!
! - TERME K:PX
                do sa = 1, nno3
                    os = (vp(sa)-1)*nddl
!
! - TERME K:PU      KPU(NDIM,NNO3,NNO1)
                    do nb = 1, nno1
                        do ib = 1, ndu
                            kk = os + vu(ib,nb)
                            t1 = vff3(sa,g)*bb*dff1(nb,lij(ib,ib))
                            matr(kk) = matr(kk) + w*t1
                        end do
                    end do
!
! - TERME K:PG      KPG(NNO3,NNO2)
                    do rb = 1, nno2
                        kk = os + vg(rb)
                        t1 = - vff3(sa,g)*dboa*vff2(rb,g)
                        matr(kk) = matr(kk) + w*t1
                    end do
!
! - TERME K:PP = 0.D0      KPP(NNO3,NNO3)
!
                end do
            endif
        endif
    end do
!
! - SYNTHESE DES CODES RETOURS
    call codere(cod, npg, codret)
!
999 continue
end subroutine
