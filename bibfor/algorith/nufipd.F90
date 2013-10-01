subroutine nufipd(ndim, nno1, nno2, npg, iw,&
                  vff1, vff2, idff1, vu, vp,&
                  geomi, typmod, option, mate, compor,&
                  lgpg, crit, instm, instp, ddlm,&
                  ddld, angmas, sigm, vim, sigp,&
                  vip, resi, rigi, mini, vect,&
                  matr, codret)
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
#include "asterfort/calkbb.h"
#include "asterfort/calkbp.h"
#include "asterfort/calkce.h"
#include "asterfort/codere.h"
#include "asterfort/dfdmip.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmepsi.h"
#include "asterfort/pmat.h"
#include "asterfort/r8inir.h"
#include "asterfort/tanbul.h"
#include "asterfort/utmess.h"
#include "blas/ddot.h"
    logical :: resi, rigi, mini
    integer :: ndim, nno1, nno2, npg, iw, idff1, lgpg
    integer :: mate
    integer :: vu(3, 27), vp(27)
    integer :: codret
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg)
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
!          ROUTINE APPELEE PAR TE0596
!-----------------------------------------------------------------------
! IN  RESI    : CALCUL DES FORCES INTERNES
! IN  RIGI    : CALCUL DE LA MATRICE DE RIGIDITE
! IN  MINI    : STABILISATION BULLE - MINI ELEMENT
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  NNO1    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AUX DEPLACEMENTS
! IN  NNO2    : NOMBRE DE NOEUDS DE L'ELEMENT LIES A LA PRESSION
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  IW      : POIDS DES POINTS DE GAUSS
! IN  VFF1    : VALEUR  DES FONCTIONS DE FORME LIES AUX DEPLACEMENTS
! IN  VFF2    : VALEUR  DES FONCTIONS DE FORME LIES A LA PRESSION
! IN  IDFF1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  VU      : TABLEAU DES INDICES DES DDL DE DEPLACEMENTS
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
    integer :: ia, na, sa, ib, nb, sb, ja, jb
    integer :: os, kk
    integer :: vuiana, vpsa
    integer :: cod(27)
    real(kind=8) :: rac2
    real(kind=8) :: deplm(3*27), depld(3*27)
    real(kind=8) :: r, w, dff1(nno1, ndim)
    real(kind=8) :: presm(27), presd(27)
    real(kind=8) :: pm, pd
    real(kind=8) :: fm(3, 3), epsm(6), deps(6)
    real(kind=8) :: sigma(6), sigmam(6), sigtr
    real(kind=8) :: dsidep(6, 6)
    real(kind=8) :: def(2*ndim, nno1, ndim), deftr(nno1, ndim), ddivu, divum
    real(kind=8) :: ddev(6, 6), devd(6, 6), dddev(6, 6)
    real(kind=8) :: t1, t2
    real(kind=8) :: idev(6, 6)
    real(kind=8) :: tampon(10), rbid(1)
    real(kind=8) :: alpha, trepst
    real(kind=8) :: dsbdep(2*ndim, 2*ndim), kbb(ndim, ndim), kbp(ndim, nno2)
    real(kind=8) :: kce(nno2, nno2), rce(nno2)
!
    parameter    (grand = .false.)
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
    nddl = nno1*ndim + nno2
    rac2 = sqrt(2.d0)
!
! - EXTRACTION DES CHAMPS
    do na = 1, nno1
        do ia = 1, ndim
            deplm(ia+ndim*(na-1)) = ddlm(vu(ia,na))
            depld(ia+ndim*(na-1)) = ddld(vu(ia,na))
        end do
    end do
!
    do sa = 1, nno2
        presm(sa) = ddlm(vp(sa))
        presd(sa) = ddld(vp(sa))
    end do
!
    if (resi) call r8inir(nddl, 0.d0, vect, 1)
    if (rigi) call r8inir(nddl*(nddl+1)/2, 0.d0, matr, 1)
!
    call r8inir(36, 0.d0, dsidep, 1)
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
    do g = 1, npg
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
! - CALCUL DE LA PRESSION
        pm = ddot(nno2,vff2(1,g),1,presm,1)
        pd = ddot(nno2,vff2(1,g),1,presd,1)
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
        divum = epsm(1) + epsm(2) + epsm(3)
        ddivu = deps(1) + deps(2) + deps(3)
!
! - CALCUL DE LA MATRICE B EPS_ij=B_ijkl U_kl
! - DEF (XX,YY,ZZ,2/RAC(2)XY,2/RAC(2)XZ,2/RAC(2)YZ)
        if (ndim .eq. 2) then
            do na = 1, nno1
                do ia = 1, ndim
                    def(1,na,ia)= fm(ia,1)*dff1(na,1)
                    def(2,na,ia)= fm(ia,2)*dff1(na,2)
                    def(3,na,ia)= 0.d0
                    def(4,na,ia)=(fm(ia,1)*dff1(na,2)+fm(ia,2)*dff1(na,1))/rac2
                end do
            end do
!
! - TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
            if (axi) then
                do na = 1, nno1
                    def(3,na,1) = fm(3,3)*vff1(na,g)/r
                end do
            endif
        else
            do na = 1, nno1
                do ia = 1, ndim
                    def(1,na,ia)= fm(ia,1)*dff1(na,1)
                    def(2,na,ia)= fm(ia,2)*dff1(na,2)
                    def(3,na,ia)= fm(ia,3)*dff1(na,3)
                    def(4,na,ia)=(fm(ia,1)*dff1(na,2)+fm(ia,2)*dff1(na,1))/rac2
                    def(5,na,ia)=(fm(ia,1)*dff1(na,3)+fm(ia,3)*dff1(na,1))/rac2
                    def(6,na,ia)=(fm(ia,2)*dff1(na,3)+fm(ia,3)*dff1(na,2))/rac2
                end do
            end do
        endif
!
! - CALCUL DE TRACE(B)
        do na = 1, nno1
            do ia = 1, ndim
                deftr(na,ia) = def(1,na,ia) + def(2,na,ia) + def(3,na,ia)
            end do
        end do
!
! - CONTRAINTE EN T- POUR LA LOI DE COMPORTEMENT
        do ia = 1, 3
            sigmam(ia) = sigm(ia,g) + sigm(2*ndim+1,g)
        end do
        do ia = 4, 2*ndim
            sigmam(ia) = sigm(ia,g)*rac2
        end do
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
            if (.not. resi) then
                call utmess('F', 'ALGORITH14_75')
            endif
            goto 999
        endif
!
! - CALCUL DE LA MATRICE D'ELASTICITE BULLE
        call tanbul(option, ndim, g, mate, compor(1),&
                    resi, mini, alpha, dsbdep, trepst)
!
! - CALCUL DE LA MATRICE DE CONDENSATION STATIQUE
        if (mini) then
            call calkbb(nno1, ndim, w, def, dsbdep,&
                        kbb)
            call calkbp(nno2, ndim, w, dff1, kbp)
            call calkce(nno1, ndim, kbp, kbb, presm,&
                        presd, kce, rce)
        else
            call r8inir(nno2, 0.d0, rce, 1)
            call r8inir(nno2*nno2, 0.d0, kce, 1)
        endif
!
! - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
        if (resi) then
! - CONTRAINTES A L'EQUILIBRE
            sigtr = sigma(1) + sigma(2) + sigma(3)
            do ia = 1, 3
                sigma(ia) = sigma(ia) - sigtr/3.d0 + (pm+pd)
            end do
!
! - VECTEUR FINT:U
            do na = 1, nno1
                do ia = 1, ndim
                    kk = vu(ia,na)
                    t1 = ddot(2*ndim, sigma,1, def(1,na,ia),1)
                    vect(kk) = vect(kk) + w*t1
                end do
            end do
!
! - VECTEUR FINT:P
            t2 = (divum+ddivu-(pm+pd)*alpha-trepst)
            do sa = 1, nno2
                kk = vp(sa)
                t1 = vff2(sa,g)*t2
                vect(kk) = vect(kk) + w*t1 - rce(sa)
            end do
!
! - STOCKAGE DES CONTRAINTES
            do ia = 1, 3
                sigp(ia,g) = sigma(ia)
            end do
            do ia = 4, 2*ndim
                sigp(ia,g) = sigma(ia)/rac2
            end do
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
! - MATRICE SYMETRIQUE
! - TERME K:UX
            do na = 1, nno1
                do ia = 1, ndim
                    vuiana = vu(ia,na)
                    os = (vuiana-1)*vuiana/2
!
! - TERME K:UU      KUU(NDIM,NNO1,NDIM,NNO1)
                    do nb = 1, nno1
                        do ib = 1, ndim
                            if (vu(ib,nb) .le. vuiana) then
                                kk = os+vu(ib,nb)
                                t1 = 0.d0
                                do ja = 1, 2*ndim
                                    do jb = 1, 2*ndim
                                        t1 = t1 + def(ja,na,ia)*dddev(ja,jb)*def(jb,nb,ib)
                                    end do
                                end do
                                matr(kk) = matr(kk) + w*t1
                            endif
                        end do
                    end do
!
! - TERME K:UP      KUP(NDIM,NNO1,NNO2)
                    do sb = 1, nno2
                        if (vp(sb) .lt. vuiana) then
                            kk = os + vp(sb)
                            t1 = deftr(na,ia)*vff2(sb,g)
                            matr(kk) = matr(kk) + w*t1
                        endif
                    end do
                end do
            end do
!
! - TERME K:PX
            do sa = 1, nno2
                vpsa = vp(sa)
                os = (vpsa-1)*vpsa/2
!
! - TERME K:PU      KPU(NDIM,NNO2,NNO1)
                do nb = 1, nno1
                    do ib = 1, ndim
                        if (vu(ib,nb) .lt. vpsa) then
                            kk = os + vu(ib,nb)
                            t1 = vff2(sa,g)*deftr(nb,ib)
                            matr(kk) = matr(kk) + w*t1
                        endif
                    end do
                end do
!
! - TERME K:PP      KPP(NNO2,NNO2)
                do sb = 1, nno2
                    if (vp(sb) .le. vpsa) then
                        kk = os + vp(sb)
                        t1 = - vff2(sa,g)*vff2(sb,g)*alpha
                        matr(kk) = matr(kk) + w*t1 - kce(sa,sb)
                    endif
                end do
            end do
        endif
    end do
!
! - SYNTHESE DES CODES RETOURS
    call codere(cod, npg, codret)
!
999 continue
end subroutine
