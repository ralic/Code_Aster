subroutine nifipd(ndim, nno1, nno2, nno3, npg,&
                  iw, vff1, vff2, vff3, idff1,&
                  vu, vg, vp, geomi, typmod,&
                  option, mate, compor, lgpg, crit,&
                  instm, instp, ddlm, ddld, angmas,&
                  sigm, vim, sigp, vip, resi,&
                  rigi, vect, matr, codret)
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
#include "asterfort/nmcomp.h"
#include "asterfort/nmepsi.h"
#include "asterfort/pmat.h"
#include "asterfort/r8inir.h"
#include "asterfort/u2mess.h"
#include "blas/ddot.h"
    logical :: resi, rigi
    integer :: ndim, nno1, nno2, nno3, npg, iw, idff1, lgpg
    integer :: mate
    integer :: vu(3, 27), vg(27), vp(27)
    integer :: codret
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg), vff3(nno3, npg)
    real(kind=8) :: instm, instp
    real(kind=8) :: geomi(ndim, nno1), ddlm(*), ddld(*), angmas(*)
    real(kind=8) :: sigm(2*ndim+1, npg), sigp(2*ndim+1, npg)
    real(kind=8) :: vim(lgpg, npg), vip(lgpg, npg)
    real(kind=8) :: vect(*), matr(*)
    real(kind=8) :: crit(*)
    character(len=8) :: typmod(*)
    character(len=16) :: compor(*), option
!-----------------------------------------------------------------------
!          CALCUL DES FORCES INTERNES POUR LES ELEMENTS
!          INCOMPRESSIBLES POUR LES PETITES DEFORMATIONS
!          3D/D_PLAN/AXIS
!          ROUTINE APPELEE PAR TE0590
!-----------------------------------------------------------------------
! IN  RESI    : CALCUL DES FORCES INTERNES
! IN  RIGI    : CALCUL DE LA MATRICE DE RIGIDITE
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
    logical :: axi, grand
    integer :: g, nddl
    integer :: ia, na, ra, sa, ib, nb, rb, sb, ja, jb
    integer :: os, kk
    integer :: vuiana, vgra, vpsa
    integer :: cod(27)
    real(kind=8) :: rac2
    real(kind=8) :: deplm(3*27), depld(3*27)
    real(kind=8) :: r, w, dff1(nno1, ndim)
    real(kind=8) :: presm(27), presd(27)
    real(kind=8) :: gonfm(27), gonfd(27)
    real(kind=8) :: gm, gd, pm, pd
    real(kind=8) :: fm(3, 3), epsm(6), deps(6)
    real(kind=8) :: sigma(6), sigmam(6), sigtr
    real(kind=8) :: dsidep(6, 6)
    real(kind=8) :: def(6, nno1, ndim), deftr(nno1, ndim), ddivu, divum
    real(kind=8) :: ddev(6, 6), devd(6, 6), dddev(6, 6)
    real(kind=8) :: iddid, devdi(6), iddev(6)
    real(kind=8) :: t1, t2
    real(kind=8) :: idev(6, 6), kr(6)
    real(kind=8) :: tampon(10), rbid
!
    parameter    (grand = .false.)
    data         kr   / 1.d0, 1.d0, 1.d0, 0.d0, 0.d0, 0.d0/
    data         idev / 2.d0,-1.d0,-1.d0, 0.d0, 0.d0, 0.d0,&
     &                   -1.d0, 2.d0,-1.d0, 0.d0, 0.d0, 0.d0,&
     &                   -1.d0,-1.d0, 2.d0, 0.d0, 0.d0, 0.d0,&
     &                    0.d0, 0.d0, 0.d0, 3.d0, 0.d0, 0.d0,&
     &                    0.d0, 0.d0, 0.d0, 0.d0, 3.d0, 0.d0,&
     &                    0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 3.d0/
!-----------------------------------------------------------------------
!
! - INITIALISATION
    axi = typmod(1).eq.'AXIS'
    nddl = nno1*ndim + nno2 + nno3
    rac2 = sqrt(2.d0)
!
! - EXTRACTION DES CHAMPS
    do 10 na = 1, nno1
        do 11 ia = 1, ndim
            deplm(ia+ndim*(na-1)) = ddlm(vu(ia,na))
            depld(ia+ndim*(na-1)) = ddld(vu(ia,na))
11      continue
10  end do
!
    do 20 ra = 1, nno2
        gonfm(ra) = ddlm(vg(ra))
        gonfd(ra) = ddld(vg(ra))
20  end do
!
    do 30 sa = 1, nno3
        presm(sa) = ddlm(vp(sa))
        presd(sa) = ddld(vp(sa))
30  end do
!
    if (resi) call r8inir(nddl, 0.d0, vect, 1)
    if (rigi) call r8inir(nddl*(nddl+1)/2, 0.d0, matr, 1)
!
    call r8inir(36, 0.d0, dsidep, 1)
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
    do 1000 g = 1, npg
!
! - CALCUL DES DEFORMATIONS
        call r8inir(6, 0.d0, epsm, 1)
        call r8inir(6, 0.d0, deps, 1)
        call dfdmip(ndim, nno1, axi, geomi, g,&
                    iw, vff1(1, g), idff1, r, w,&
                    dff1)
        call nmepsi(ndim, nno1, axi, grand, vff1(1, g),&
                    r, dff1, deplm, fm, epsm)
        call nmepsi(ndim, nno1, axi, grand, vff1(1, g),&
                    r, dff1, depld, fm, deps)
!
! - CALCUL DE LA PRESSION ET DU GONFLEMENT AU POINT DE GAUSS
        gm = ddot(nno2,vff2(1,g),1,gonfm,1)
        gd = ddot(nno2,vff2(1,g),1,gonfd,1)
!
        pm = ddot(nno3,vff3(1,g),1,presm,1)
        pd = ddot(nno3,vff3(1,g),1,presd,1)
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
        divum = epsm(1) + epsm(2) + epsm(3)
        ddivu = deps(1) + deps(2) + deps(3)
!
! - CALCUL DE LA MATRICE B EPS_ij=B_ijkl U_kl
! - DEF (XX,YY,ZZ,2/RAC(2)XY,2/RAC(2)XZ,2/RAC(2)YZ)
        if (ndim .eq. 2) then
            do 35 na = 1, nno1
                do 45 ia = 1, ndim
                    def(1,na,ia)= fm(ia,1)*dff1(na,1)
                    def(2,na,ia)= fm(ia,2)*dff1(na,2)
                    def(3,na,ia)= 0.d0
                    def(4,na,ia)=(fm(ia,1)*dff1(na,2)+fm(ia,2)*dff1(&
                    na,1))/rac2
45              continue
35          continue
!
! - TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
            if (axi) then
                do 47 na = 1, nno1
                    def(3,na,1) = fm(3,3)*vff1(na,g)/r
47              continue
            endif
        else
            do 36 na = 1, nno1
                do 46 ia = 1, ndim
                    def(1,na,ia)= fm(ia,1)*dff1(na,1)
                    def(2,na,ia)= fm(ia,2)*dff1(na,2)
                    def(3,na,ia)= fm(ia,3)*dff1(na,3)
                    def(4,na,ia)=(fm(ia,1)*dff1(na,2)+fm(ia,2)*dff1(&
                    na,1))/rac2
                    def(5,na,ia)=(fm(ia,1)*dff1(na,3)+fm(ia,3)*dff1(&
                    na,1))/rac2
                    def(6,na,ia)=(fm(ia,2)*dff1(na,3)+fm(ia,3)*dff1(&
                    na,2))/rac2
46              continue
36          continue
        endif
!
! - CALCUL DE TRACE(B)
        do 50 na = 1, nno1
            do 49 ia = 1, ndim
                deftr(na,ia) = def(1,na,ia) + def(2,na,ia) + def(3,na, ia)
49          continue
50      continue
!
! - DEFORMATION POUR LA LOI DE COMPORTEMENT
        do 60 ia = 1, 3
            epsm(ia) = epsm(ia) + (gm - divum)/3.d0
            deps(ia) = deps(ia) + (gd - ddivu)/3.d0
60      continue
!
! - CONTRAINTE EN T- POUR LA LOI DE COMPORTEMENT
        do 62 ia = 1, 3
            sigmam(ia) = sigm(ia,g) + sigm(2*ndim+1,g)
62      continue
        do 65 ia = 4, 2*ndim
            sigmam(ia) = sigm(ia,g)*rac2
65      continue
!
! - APPEL A LA LOI DE COMPORTEMENT
        call nmcomp('RIGI', g, 1, ndim, typmod,&
                    mate, compor, crit, instm, instp,&
                    6, epsm, deps, 6, sigmam,&
                    vim(1, g), option, angmas, 10, tampon,&
                    sigma, vip(1, g), 36, dsidep, 1,&
                    rbid, cod(g))
!
        if (cod(g) .eq. 1) then
            codret = 1
            if (.not. resi) call u2mess('F', 'ALGORITH14_75')
            goto 9999
        endif
!
! - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
        if (resi) then
! - CONTRAINTES A L'EQUILIBRE
            sigtr = sigma(1) + sigma(2) + sigma(3)
            do 130 ia = 1, 3
                sigma(ia) = sigma(ia) - sigtr/3.d0 + (pm+pd)
130          continue
!
! - VECTEUR FINT:U
            do 300 na = 1, nno1
                do 310 ia = 1, ndim
                    kk = vu(ia,na)
                    t1 = ddot(2*ndim, sigma,1, def(1,na,ia),1)
                    vect(kk) = vect(kk) + w*t1
310              continue
300          continue
!
! - VECTEUR FINT:G
            t2 = (sigtr/3.d0 - pm - pd)
            do 350 ra = 1, nno2
                kk = vg(ra)
                t1 = vff2(ra,g)*t2
                vect(kk) = vect(kk) + w*t1
350          continue
!
! - VECTEUR FINT:P
            t2 = (divum+ddivu-gm-gd)
            do 370 sa = 1, nno3
                kk = vp(sa)
                t1 = vff3(sa,g)*t2
                vect(kk) = vect(kk) + w*t1
370          continue
!
! - STOCKAGE DES CONTRAINTES
            do 190 ia = 1, 3
                sigp(ia,g) = sigma(ia)
190          continue
            do 195 ia = 4, 2*ndim
                sigp(ia,g) = sigma(ia)/rac2
195          continue
            sigp(2*ndim+1,g) = sigtr/3.d0 - pm - pd
        endif
!
! - MATRICE TANGENTE
        if (rigi) then
!
            call pmat(6, idev/3.d0, dsidep, devd)
            call pmat(6, dsidep, idev/3.d0, ddev)
            call pmat(6, devd, idev/3.d0, dddev)
!
! - CALCUL DE D^DEV:ID ET ID:D^DEV ET ID:D:ID/9.D0
            iddid = 0.d0
            do 380 ia = 1, 6
                devdi(ia) = devd(ia,1)+devd(ia,2)+devd(ia,3)
                iddev(ia) = ddev(1,ia)+ddev(2,ia)+ddev(3,ia)
                do 390 ja = 1, 3
                    iddid = iddid+kr(ia)*dsidep(ia,ja)
390              continue
380          continue
            iddid = iddid/9.d0
!
! - MATRICE SYMETRIQUE
! - TERME K:UX
            do 400 na = 1, nno1
                do 410 ia = 1, ndim
                    vuiana = vu(ia,na)
                    os = (vuiana-1)*vuiana/2
!
! - TERME K:UU      KUU(NDIM,NNO1,NDIM,NNO1)
                    do 420 nb = 1, nno1
                        do 430 ib = 1, ndim
                            if (vu(ib,nb) .le. vuiana) then
                                kk = os+vu(ib,nb)
                                t1 = 0.d0
                                do 440 ja = 1, 2*ndim
                                    do 450 jb = 1, 2*ndim
                                        t1 = t1 + def(ja,na,ia)*dddev( ja,jb)*def(jb,nb,ib)
450                                  continue
440                              continue
                                matr(kk) = matr(kk) + w*t1
                            endif
430                      continue
420                  continue
!
! - TERME K:UG      KUG(NDIM,NNO1,NNO2)
                    t1 = 0.d0
                    do 470 ja = 1, 2*ndim
                        t1 = t1 + def(ja,na,ia)*devdi(ja)
470                  continue
                    t1 = t1/3.d0
!
                    do 480 rb = 1, nno2
                        if (vg(rb) .lt. vuiana) then
                            kk = os + vg(rb)
                            matr(kk) = matr(kk) + w*t1*vff2(rb,g)
                        endif
480                  continue
!
! - TERME K:UP      KUP(NDIM,NNO1,NNO3)
                    do 490 sb = 1, nno3
                        if (vp(sb) .lt. vuiana) then
                            kk = os + vp(sb)
                            t1 = deftr(na,ia)*vff3(sb,g)
                            matr(kk) = matr(kk) + w*t1
                        endif
490                  continue
410              continue
400          continue
!
! - TERME K:GX
            do 500 ra = 1, nno2
                vgra = vg(ra)
                os = (vgra-1)*vgra/2
!
! - TERME K:GU      KGU(NDIM,NNO2,NNO1)
                do 510 nb = 1, nno1
                    do 520 ib = 1, ndim
                        if (vu(ib,nb) .lt. vgra) then
                            kk = os + vu(ib,nb)
                            t1 = 0.d0
                            do 530 jb = 1, 2*ndim
                                t1 = t1 + iddev(jb)*def(jb,nb,ib)
530                          continue
                            matr(kk) = matr(kk) + w*t1*vff2(ra,g)/ 3.d0
                        endif
520                  continue
510              continue
!
! - TERME K:GG      KGG(NNO2,NNO2)
                do 540 rb = 1, nno2
                    if (vg(rb) .le. vgra) then
                        kk = os + vg(rb)
                        t1 = vff2(ra,g)*iddid*vff2(rb,g)
                        matr(kk) = matr(kk) + w*t1
                    endif
540              continue
!
! - TERME K:GP      KGP(NNO2,NNO3)
                do 550 sb = 1, nno3
                    if (vp(sb) .lt. vgra) then
                        kk = os + vp(sb)
                        t1 = - vff2(ra,g)*vff3(sb,g)
                        matr(kk) = matr(kk) + w*t1
                    endif
550              continue
500          continue
!
! - TERME K:PX
            do 600 sa = 1, nno3
                vpsa = vp(sa)
                os = (vpsa-1)*vpsa/2
!
! - TERME K:PU      KPU(NDIM,NNO3,NNO1)
                do 610 nb = 1, nno1
                    do 620 ib = 1, ndim
                        if (vu(ib,nb) .lt. vpsa) then
                            kk = os + vu(ib,nb)
                            t1 = vff3(sa,g)*deftr(nb,ib)
                            matr(kk) = matr(kk) + w*t1
                        endif
620                  continue
610              continue
!
! - TERME K:PG      KPG(NNO3,NNO2)
                do 630 rb = 1, nno2
                    if (vg(rb) .lt. vpsa) then
                        kk = os + vg(rb)
                        t1 = - vff3(sa,g)*vff2(rb,g)
                        matr(kk) = matr(kk) + w*t1
                    endif
630              continue
!
! - TERME K:PP = 0.D0      KPP(NNO3,NNO3)
600          continue
        endif
1000  end do
!
! - SYNTHESE DES CODES RETOURS
    call codere(cod, npg, codret)
!
9999  continue
end subroutine
