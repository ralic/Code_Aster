subroutine nifism(ndim, nno1, nno2, nno3, npg,&
                  iw, vff1, vff2, vff3, idff1,&
                  idff2, vu, vg, vp, geomi,&
                  typmod, option, mate, compor, lgpg,&
                  crit, instm, instp, ddlm, ddld,&
                  angmas, sigm, vim, sigp, vip,&
                  resi, rigi, vect, matr, codret)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: sebastien.fayolle at edf.fr
! aslint: disable=W1306,W1504
    implicit none
!
#include "asterfort/codere.h"
#include "asterfort/dfdmip.h"
#include "asterfort/nirela.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmepsi.h"
#include "asterfort/nmmalu.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvala.h"
#include "asterfort/u2mess.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
#include "blas/dscal.h"
    logical :: resi, rigi
    integer :: ndim, nno1, nno2, nno3, npg, iw, idff1, idff2, lgpg
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
! IN  IDFF2   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
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
    logical :: axi, grand, nonloc
    integer :: g, nddl, ndu
    integer :: ia, na, ra, sa, ib, nb, rb, sb, ja, jb
    integer :: k2ret, lij(3, 3), vij(3, 3), os, kk
    integer :: viaja
    integer :: cod(27)
    real(kind=8) :: rac2
    real(kind=8) :: geomm(3*27), geomp(3*27), deplm(3*27), depld(3*27)
    real(kind=8) :: r, w, wm, wp, dff1(nno1, 4), dff2(nno2, 3)
    real(kind=8) :: presm(27), presd(27)
    real(kind=8) :: gonfm(27), gonfd(27)
    real(kind=8) :: gm, gd, gp, pm, pd, pp
    real(kind=8) :: fm(3, 3), jm, ftm(3, 3), corm, epsm(6)
    real(kind=8) :: fd(3, 3), jd, jp, ftd(3, 3), cord, epsd(6)
    real(kind=8) :: sigmam(6)
    real(kind=8) :: taup(6), taudv(6), tauhy, tauldc(6)
    real(kind=8) :: dsidep(6, 3, 3)
    real(kind=8) :: d(6, 3, 3), hdv(3, 3), dhy(6), h(3, 3), hhy
    real(kind=8) :: gradgp(3), c
    real(kind=8) :: t1, t2
    real(kind=8) :: kr(6)
    real(kind=8) :: tampon(10), id(3, 3), rbid
    real(kind=8) :: am, ap, bp, boa, aa, bb, daa, dbb, dboa, d2boa
!
    parameter    (grand = .true.)
    data         vij  / 1, 4, 5,&
     &                    4, 2, 6,&
     &                    5, 6, 3 /
    data         kr   / 1.d0, 1.d0, 1.d0, 0.d0, 0.d0, 0.d0/
    data         id   / 1.d0, 0.d0, 0.d0,&
     &                    0.d0, 1.d0, 0.d0,&
     &                    0.d0, 0.d0, 1.d0/
!-----------------------------------------------------------------------
!
! - INITIALISATION
!
    rac2 = sqrt(2.d0)
    axi = typmod(1).eq.'AXIS'
    nddl = nno1*ndim + nno2 + nno3
    ndu = ndim
    if (axi) ndu = 3
!
! - REACTUALISATION DE LA GEOMETRIE ET EXTRACTION DES CHAMPS
    do 10 na = 1, nno1
        do 11 ia = 1, ndim
            geomm(ia+ndim*(na-1)) = geomi(ia,na) + ddlm(vu(ia,na))
            geomp(ia+ndim*(na-1)) = geomm(ia+ndim*(na-1))+ddld(vu(ia, na))
            deplm(ia+ndim*(na-1)) = ddlm(vu(ia,na))
            depld(ia+ndim*(na-1)) = ddld(vu(ia,na))
11      continue
10  end do
!
    do 20 ra = 1, nno2
        gonfm(ra) = ddlm(vg(ra))
        gonfd(ra) = ddld(vg(ra))
20  end do
    do 30 sa = 1, nno3
        presm(sa) = ddlm(vp(sa))
        presd(sa) = ddld(vp(sa))
30  end do
!
    if (resi) call r8inir(nddl, 0.d0, vect, 1)
    if (rigi) call r8inir(nddl*nddl, 0.d0, matr, 1)
    call r8inir(6, 0.d0, sigmam, 1)
    call r8inir(6, 0.d0, taup, 1)
    call r8inir(54, 0.d0, dsidep, 1)
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
    do 1000 g = 1, npg
!
! - LONGUEUR CARACTERISTIQUE -> PARAMETRE C
        c=0.d0
        call rcvala(mate, ' ', 'NON_LOCAL', 0, ' ',&
                    0.d0, 1, 'C_GONF', c, k2ret,&
                    0)
        nonloc = k2ret.eq.0 .and. c.ne.0.d0
!
! - CALCUL DES DEFORMATIONS
        call dfdmip(ndim, nno1, axi, geomi, g,&
                    iw, vff1(1, g), idff1, r, w,&
                    dff1)
        call nmepsi(ndim, nno1, axi, grand, vff1(1, g),&
                    r, dff1, deplm, fm, epsm)
        call dfdmip(ndim, nno1, axi, geomm, g,&
                    iw, vff1(1, g), idff1, r, wm,&
                    dff1)
        call nmepsi(ndim, nno1, axi, grand, vff1(1, g),&
                    r, dff1, depld, fd, epsd)
        call dfdmip(ndim, nno1, axi, geomp, g,&
                    iw, vff1(1, g), idff1, r, wp,&
                    dff1)
!
        call nmmalu(nno1, axi, r, vff1(1, g), dff1,&
                    lij)
!
        jm = fm(1,1)*(fm(2,2)*fm(3,3)-fm(2,3)*fm(3,2)) - fm(2,1)*(fm( 1,2)*fm(3,3)-fm(1,3)*fm(3,2&
             &)) + fm(3,1)*(fm(1,2)*fm(2,3)-fm(1, 3)*fm(2,2))
        jd = fd(1,1)*(fd(2,2)*fd(3,3)-fd(2,3)*fd(3,2)) - fd(2,1)*(fd( 1,2)*fd(3,3)-fd(1,3)*fd(3,2&
             &)) + fd(3,1)*(fd(1,2)*fd(2,3)-fd(1, 3)*fd(2,2))
        jp = jm*jd
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
        call nirela(1, jp, gm, gp, am,&
                    ap, bp, boa, aa, bb,&
                    daa, dbb, dboa, d2boa)
!
! - PERTINENCE DES GRANDEURS
        if (jd .le. 1.d-2 .or. jd .gt. 1.d2) then
            codret = 1
            goto 9999
        endif
        if (abs(ap/am) .gt. 1.d2) then
            codret = 1
            goto 9999
        endif
!
! - CALCUL DU GRADIENT DU GONFLEMENT POUR LA REGULARISATION
        if (nonloc) then
            call dfdmip(ndim, nno2, axi, geomi, g,&
                        iw, vff2(1, g), idff2, r, w,&
                        dff2)
            do 50 ia = 1, ndim
                gradgp(ia) = ddot(&
                             nno2, dff2(1, ia), 1, gonfm, 1) + ddot( nno2, dff2(1, ia), 1, gonfd,&
                             1&
                             )
50          continue
        endif
!
! - CALCUL DES DEFORMATIONS ENRICHIES
        corm = (am/jm)**(1.d0/3.d0)
        call dcopy(9, fm, 1, ftm, 1)
        call dscal(9, corm, ftm, 1)
!
        cord = (ap/am/jd)**(1.d0/3.d0)
        call dcopy(9, fd, 1, ftd, 1)
        call dscal(9, cord, ftd, 1)
!
! - APPEL A LA LOI DE COMPORTEMENT
        cod(g) = 0
!
! - POUR LES LOIS QUI NE RESPECTENT PAS ENCORE LA NOUVELLE INTERFACE
! - ET QUI UTILISENT ENCORE LA CONTRAINTE EN T-
        call dcopy(ndim*2, sigm(1, g), 1, sigmam, 1)
!
        call nmcomp('RIGI', g, 1, 3, typmod,&
                    mate, compor, crit, instm, instp,&
                    9, ftm, ftd, 6, sigmam,&
                    vim(1, g), option, angmas, 10, tampon,&
                    taup, vip( 1, g), 54, dsidep, 1,&
                    rbid, cod(g))
!
        if (cod(g) .eq. 1) then
            codret = 1
            if (.not. resi) call u2mess('F', 'ALGORITH14_75')
            goto 9999
        endif
!
! - SUPPRESSION DES RACINES DE 2
        if (resi) call dscal(3, 1/rac2, taup(4), 1)
!
! - MATRICE TANGENTE SANS LES RACINES DE 2
        if (rigi) then
            call dscal(9, 1/rac2, dsidep(4, 1, 1), 6)
            call dscal(9, 1/rac2, dsidep(5, 1, 1), 6)
            call dscal(9, 1/rac2, dsidep(6, 1, 1), 6)
        endif
!
!
! - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
        if (resi) then
! - CONTRAINTE DE CAUCHY A PARTIR DE KIRCHHOFF
            call dcopy(2*ndim, taup, 1, sigp(1, g), 1)
            call dscal(2*ndim, 1.d0/jp, sigp(1, g), 1)
!
! - CONTRAINTE HYDROSTATIQUE ET DEVIATEUR
            tauhy = (taup(1)+taup(2)+taup(3))/3.d0
            do 100 ia = 1, 6
                taudv(ia) = taup(ia) - tauhy*kr(ia)
100          continue
!
! - VECTEUR FINT:U
            do 200 na = 1, nno1
                do 210 ia = 1, ndu
                    kk = vu(ia,na)
                    t1 = 0.d0
                    do 220 ja = 1, ndu
                        t2 = taudv(vij(ia,ja)) + pp*bb*id(ia,ja)
                        t1 = t1 + t2*dff1(na,lij(ia,ja))
220                  continue
                    vect(kk) = vect(kk) + w*t1
210              continue
200          continue
!
! - VECTEUR FINT:G
            t2 = tauhy*aa - pp*dboa
            do 230 ra = 1, nno2
                kk = vg(ra)
                t1 = vff2(ra,g)*t2
                vect(kk) = vect(kk) + w*t1
230          continue
!
            if (nonloc) then
                do 235 ra = 1, nno2
                    kk = vg(ra)
                    t1 = c*ddot(ndim,gradgp,1,dff2(ra,1),nno2)
                    vect(kk) = vect(kk) + w*t1
235              continue
            endif
!
! - VECTEUR FINT:P
            t2 = bp - boa
            do 240 sa = 1, nno3
                kk = vp(sa)
                t1 = vff3(sa,g)*t2
                vect(kk) = vect(kk) + w*t1
240          continue
        endif
!
! - MATRICE TANGENTE
        if (rigi) then
            if (.not. resi) then
                call dcopy(2*ndim, sigm(1, g), 1, taup, 1)
                call dscal(2*ndim, jm, taup, 1)
            endif
!
! - CALCUL DU TENSEUR DE CONTRAINTE : TRACE ET PARTIE DEVIATORIQUE
            tauhy = (taup(1)+taup(2)+taup(3))/3.d0
            do 370 ia = 1, 6
                tauldc(ia) = taup(ia) + (pp*bb-tauhy)*kr(ia)
370          continue
!
! - PROJECTIONS DEVIATORIQUES ET HYDROSTATIQUES DE LA MAT. TANG.
            do 400 ia = 1, 3
                do 410 ja = 1, 3
                    h(ia,ja)=(dsidep(1,ia,ja)+dsidep(2,ia,ja) +dsidep(&
                    3,ia,ja))/3.d0
                    do 420 na = 1, 6
                        d(na,ia,ja) = dsidep(na,ia,ja) - kr(na)*h(ia, ja)
420                  continue
410              continue
400          continue
!
            hhy = (h(1,1)+h(2,2)+h(3,3))/3.d0
            do 450 ia = 1, 3
                do 455 ja = 1, 3
                    hdv(ia,ja) = h(ia,ja) - hhy*id(ia,ja)
455              continue
450          continue
            do 460 ia = 1, 6
                dhy(ia) = (d(ia,1,1)+d(ia,2,2)+d(ia,3,3))/3.d0
460          continue
!
            do 500 na = 1, nno1
                do 510 ia = 1, ndu
                    os = (vu(ia,na)-1)*nddl
!
! - TERME K:UU      KUU(NDIM,NNO1,NDIM,NNO1)
                    do 520 nb = 1, nno1
                        do 530 ib = 1, ndu
                            kk = os+vu(ib,nb)
                            t1 = 0.d0
                            do 550 ja = 1, ndu
                                do 560 jb = 1, ndu
                                    viaja = vij(ia,ja)
                                    t2 = d(viaja,ib,jb)-dhy(viaja)*id( ib,jb)
                                    t1 = t1 + dff1( na,lij(ia,ja))*t2* dff1(nb,lij(ib,jb))
560                              continue
550                          continue
!
                            t2 = pp*jp*dbb
                            t1 = t1 + dff1( na,lij(ia,ia))*t2*dff1(nb, lij(ib,ib))
!
! - RIGIDITE GEOMETRIQUE
                            do 570 jb = 1, ndu
                                t1 = t1 - dff1(&
                                     na, lij(ia, ib))*dff1(nb, lij(ib, jb)) *tauldc(vij(ia, jb))
570                          continue
                            matr(kk) = matr(kk) + w*t1
530                      continue
520                  continue
!
!
! - TERME K:UG      KUG(NDIM,NNO1,NNO2)
                    do 600 rb = 1, nno2
                        kk = os + vg(rb)
                        t1 = 0.d0
                        do 610 ja = 1, ndu
                            t2 = dhy(vij(ia,ja))*aa
                            t1 = t1 + dff1(na,lij(ia,ja))*t2*vff2(rb, g)
610                      continue
                        matr(kk) = matr(kk) + w*t1
600                  continue
!
! - TERME K:UP      KUP(NDIM,NNO1,NNO3)
                    do 650 sb = 1, nno3
                        kk = os + vp(sb)
                        t1 = dff1(na,lij(ia,ia))*bb*vff3(sb,g)
                        matr(kk) = matr(kk) + w*t1
650                  continue
510              continue
500          continue
!
            do 700 ra = 1, nno2
                os = (vg(ra)-1)*nddl
!
! - TERME K:GU      KGU(NDIM,NNO2,NNO1)
                do 710 nb = 1, nno1
                    do 715 ib = 1, ndu
                        kk = os + vu(ib,nb)
                        t1 = 0.d0
                        do 720 jb = 1, ndu
                            t1 = t1 + vff2(ra,g)*aa*hdv(ib,jb)*dff1( nb,lij(ib,jb))
720                      continue
                        matr(kk) = matr(kk) + w*t1
715                  continue
710              continue
!
! - TERME K:GG      KGG(NNO2,NNO2)
                do 730 rb = 1, nno2
                    kk = os + vg(rb)
                    t2 = hhy*aa**2 - pp*d2boa + tauhy*daa
                    t1 = vff2(ra,g)*t2*vff2(rb,g)
                    matr(kk) = matr(kk) + w*t1
730              continue
!
                if (nonloc) then
                    do 735 rb = 1, nno2
                        kk = os + vg(rb)
                        t1 = c*ddot(ndim,dff2(ra,1),nno2,dff2(rb,1), nno2)
                        matr(kk) = matr(kk) + w*t1
735                  continue
                endif
!
! - TERME K:GP      KGP(NNO2,NNO3)
                do 740 sb = 1, nno3
                    kk = os + vp(sb)
                    t1 = - vff2(ra,g)*dboa*vff3(sb,g)
                    matr(kk) = matr(kk) + w*t1
740              continue
700          continue
!
            do 750 sa = 1, nno3
                os = (vp(sa)-1)*nddl
!
! - TERME K:PU      KPU(NDIM,NNO3,NNO1)
                do 760 nb = 1, nno1
                    do 765 ib = 1, ndu
                        kk = os + vu(ib,nb)
                        t1 = vff3(sa,g)*bb*dff1(nb,lij(ib,ib))
                        matr(kk) = matr(kk) + w*t1
765                  continue
760              continue
!
! - TERME K:PG      KPG(NNO3,NNO2)
                do 780 rb = 1, nno2
                    kk = os + vg(rb)
                    t1 = - vff3(sa,g)*dboa*vff2(rb,g)
                    matr(kk) = matr(kk) + w*t1
780              continue
!
! - TERME K:PP = 0.D0      KPP(NNO3,NNO3)
!
750          continue
!
        endif
1000  end do
!
! - SYNTHESE DES CODES RETOURS
    call codere(cod, npg, codret)
!
9999  continue
end subroutine
