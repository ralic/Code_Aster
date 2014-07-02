subroutine nmplgs(ndim, nno1, vff1, idfde1, nno2,&
                  vff2, idfde2, npg, iw, geom,&
                  typmod, option, mate, compor, crit,&
                  instam, instap, angmas, ddlm, ddld,&
                  sigm, lgpg, vim, sigp, vip,&
                  matr, vect, codret, dfdi2, livois,&
                  nbvois, numa, lisoco, nbsoco)
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
! ======================================================================
! ======================================================================
!
! CALCUL  RAPH_MECA, RIGI_MECA_* ET FULL_MECA_* POUR GRAD_SIGM(2D ET 3D)
!
! IN  NDIM    : DIMENSION DES ELEMENTS
! IN  NNO1    : NOMBRE DE NOEUDS (FAMILLE U)
! IN  VFF1    : VALEUR DES FONCTIONS DE FORME (FAMILLE U)
! IN  IDFDE1  : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE U)
! IN  NNO2    : NOMBRE DE NOEUDS (FAMILLE E)
! IN  VFF2    : VALEUR DES FONCTIONS DE FORME (FAMILLE E)
! IN  IDFDE2  : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE E)
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  IW      : POIDS DES POINTS DE GAUSS DE REFERENCE (INDICE)
! IN  GEOM    : COORDONNEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODEELISATION
! IN  OPTION  : OPTION DE CALCUL
! IN  MATE   : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT PRECEDENT
! IN  INSTAP  : INSTANT DE CALCUL
! IN  DDLM    : DDL A L'INSTANT PRECEDENT
! IN  DDLD    : INCREMENT DES DDL
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! IN  LGPG    : LONGUEUR DU TABLEAU DES VARIABLES INTERNES
! IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
! IN  ANGMAS  : REPERE LOCAL 3D
! OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA   ET FULL_MECA_*)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA   ET FULL_MECA_*)
! OUT MATR    : MATRICE DE RIGIDITE   (RIGI_MECA_* ET FULL_MECA_*)
! OUT VECT    : FORCES INTERIEURES    (RAPH_MECA   ET FULL_MECA_*)
! OUT CODRET  : CODE RETOUR
! ----------------------------------------------------------------------
!
! aslint: disable=W1306,W1504
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cavini.h"
#include "asterfort/codere.h"
#include "asterfort/dfdmip.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmepsb.h"
#include "asterfort/nmepsi.h"
#include "asterfort/nmmabu.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
#include "blas/dspev.h"
    common  /trucit/iteamm
    character(len=8) :: typmod(*), fami, poum
    character(len=16) :: option, compor(*)
    integer :: nbvois, nvoima, numav, iret, nscoma, iteamm
    integer(kind=4) :: reuss
    parameter(nvoima=12,nscoma=4)
    integer :: ndim, nno1, nno2, npg, idfde1, idfde2, iw, mate, lgpg, codret
    integer :: livois(1:nvoima), numa
    integer :: nbsoco(1:nvoima), lisoco(1:nvoima, 1:nscoma, 1:2)
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg), geom(ndim, nno1)
    real(kind=8) :: crit(*), instam, instap
    real(kind=8) :: ddlm(*), ddld(*), sigm(2*ndim, npg), sigp(2*ndim, npg)
    real(kind=8) :: vim(lgpg, npg), vip(lgpg, npg), matr(*), vect(*)
    real(kind=8) :: dfdi2(nno2, ndim), angmas(3), compar
    integer :: k2(1), kpg, spt
    aster_logical :: resi, rigi, grand, axi
    integer :: ndimsi, nddl, g, gg, cod(27), n, i, m, j, kl, pq, os, kk, vivois
    integer :: iu(3, 27), ie(6, 8), kvois, ll
    integer :: nfin, vrarr(nno2), nn, nnn, vivonu, kvoinu, nini, nunu
    real(kind=8) :: rac2, lc(1), c, deplm(3*27), depld(3*27), dfdi1(27, 3), nono
    real(kind=8) :: r, wg, epsgm(6, 2), epsgd(6, 2), gepsm(6, 3), geps(6, 3)
    real(kind=8) :: f(3, 3)
    real(kind=8) :: b(6, 3, 27), de(6), sigma(6), dsidep(6, 6, 2), t1, t2
    real(kind=8) :: pin(6, 6)
    real(kind=8) :: p(6, 6), sigmam(6), epsrss(6), sigell(6), dist(nno2, 2)
    real(kind=8) :: z(3, 3), w(3), work(9), bary(ndim), baryo(ndim), scal(3)
    real(kind=8) :: dirr(ndim)
!
! ----------------------------------------------------------------------
!
! - INITIALISATION
!
    resi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RAPH_MECA'
    rigi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RIGI_MECA'
    rac2 = sqrt(2.d0)
    grand = .false.
    axi = .false.
    ndimsi = 2*ndim
    nddl = nno1*ndim + nno2*ndimsi
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, mate,&
                ' ', 'NON_LOCAL', 0, ' ', [0.d0],&
                1, 'LONG_CAR', lc, k2, 1)
    c = lc(1)**2
    do 5 g = 1, npg
        cod(g)=0
  5 end do
!
! INITIALISATION CAVINI + INCREMENTATION
! DU COMPTEUR D'ITERATION + L ELEMENT EST-IL POINTE?
    call cavini(ndim, nno2, geom, vim, npg,&
                lgpg, mate)
!
    nono=0.d0
    nini=0
!
8491 continue
!
    nunu=0
!
    if (resi) then
!
        if (nini .eq. 0) then
            do 782 gg = 1, npg
                vip(8,gg) = vip(8,gg)+1.0d0
782         continue
        endif
!
        do 1783 kvois = 1, nbvois
!
            numav=livois(kvois)
            call tecach('OOO', 'PVARIMP', 'L', iret, iad=vivois,&
                        numa=numav)
            ASSERT(iret.eq.0)
!
            if (nint(zr(vivois-1+5)) .eq. numa) then
                if (zr(vivois-1+7) .lt. vip(8,1)) then
                    vivonu=vivois
                    kvoinu=kvois
                    if (nint(vip(2,1)) .eq. 0) then
                        do 1773 gg = 1, npg
                            vip(2,gg)=1.d0
1773                     continue
                    endif
                    if (nint(vip(2,1)) .eq. 1) then
                        bary(1)=zr(vivonu-1+9)
                        bary(2)=zr(vivonu-1+10)
                        baryo(1)=zr(vivonu-1+11)
                        baryo(2)=zr(vivonu-1+12)
                    endif
                endif
            endif
!
            if (nint(zr(vivois-1+6)) .eq. numa) then
                if (zr(vivois-1+7) .lt. vip(8,1)) then
                    vivonu=vivois
                    kvoinu=kvois
                    if (nint(vip(2,1)) .eq. 0) then
                        do 1774 gg = 1, npg
                            vip(2,gg)=1.0d0
1774                     continue
                    endif
                    if (nint(vip(2,1)) .eq. 1) then
                        bary(1)=zr(vivonu-1+11)
                        bary(2)=zr(vivonu-1+12)
                        baryo(1)=zr(vivonu-1+9)
                        baryo(2)=zr(vivonu-1+10)
                    endif
                endif
            endif
!
1783     continue
!
        if (nint(vip(2,1)) .eq. 0) then
            call r8inir(6, 0.d0, epsrss, 1)
            do 2118 kl = 1, ndimsi
                do 2119 i = 1, nno2
                    ll=kl+ndim+((i-1)*(ndim+ndimsi))
                    epsrss(kl)=epsrss(kl)+ddlm(ll)/dble(nno2)
                    epsrss(kl)=epsrss(kl)+ddld(ll)/dble(nno2)
2119             continue
2118         continue
            call dscal(ndimsi-3, 1.d0/rac2, epsrss(4), 1)
        endif
!
        if (nint(vip(2,1)) .eq. 1) then
            call r8inir(6, 0.d0, epsrss, 1)
            scal(1)=0
            scal(2)=0
            scal(3)=0
            do 7614 n = 1, nbsoco(kvoinu)
                pq=lisoco(kvoinu,n,1)
                do 7663 i = 1, ndim
                    scal(n)=scal(n)+(geom(i,pq)-bary(i))**2.d0
7663             continue
7614         continue
            scal(1)=sqrt(scal(1))
            scal(2)=sqrt(scal(2))
            scal(3)=scal(1)+scal(2)
            do 2618 kl = 1, ndimsi
                do 2617 i = 1, nbsoco(kvoinu)
                    pq=lisoco(kvoinu,i,1)
                    ll=kl+ndim+((pq-1)*(ndim+ndimsi))
                    epsrss(kl)=epsrss(kl)+(1.0d0-scal(i)/scal(3))*&
                    ddlm(ll)
                    epsrss(kl)=epsrss(kl)+(1.0d0-scal(i)/scal(3))*&
                    ddld(ll)
2617             continue
2618         continue
            call dscal(ndimsi-3, 1.d0/rac2, epsrss(4), 1)
        endif
!
        sigell(1) = epsrss(1)
        sigell(2) = epsrss(4)
        sigell(3) = epsrss(2)
        sigell(4) = epsrss(5)
        sigell(5) = epsrss(6)
        sigell(6) = epsrss(3)
!
        call dspev('V', 'U', 3, sigell, w,&
                   z, 3, work, reuss)
!
        if (nini .eq. 0) then
            do 5987 i = 1, ndim
                dirr(i)=z(i,3)
5987         continue
        endif
!
        if (nint(vip(2,1)) .eq. 1) then
            if (w(3) .gt. vim(4,1)) then
                do 6354 i = 1, npg
                    vip(2,i)=3.d0
                    vip(7,i)=vip(8,i)
6354             continue
            endif
        endif
!
        if (nint(vip(2,1)) .eq. 0) then
            if (w(3) .gt. vim(3,1)) then
                if (nint(vip(8,1)) .gt. iteamm) then
                    iteamm=nint(vip(8,1))
                else
                    iteamm=0
                    nono=1.d0
                endif
                do 6355 i = 1, npg
                    vip(2,i)=2.d0
                    vip(7,i)=vip(8,i)
6355             continue
!
            endif
        endif
!
        if (nint(vip(7,1)) .eq. nint(vip(8,1))) then
!
            nnn=5
!
            if (nint(vip(2,1)) .eq. 2) then
                bary(1)=0.d0
                bary(2)=0.d0
                do 5612 i = 1, ndim
                    bary(i)=0
                    do 5611 n = 1, nno2
                        bary(i)=bary(i)+geom(i,n)/nno2
5611                 continue
5612             continue
            endif
!
            do 5614 n = 1, nno2
                nfin=n+1
                if (nfin .gt. nno2) then
                    nfin=nfin-nno2
                endif
                scal(1)=0
                scal(2)=0
                scal(3)=0
                do 5663 i = 1, ndim
                    scal(1)=scal(1)+dirr(i)*(geom(i,n)-bary(i))
                    scal(2)=scal(2)+dirr(i)*(geom(i,nfin)-bary(i))
5663             continue
                scal(3)=scal(1)*scal(2)
                if (scal(3) .lt. 0.d0) then
                    vrarr(n)=1
                else
                    vrarr(n)=0
                endif
5614         continue
!
            if (nint(vip(2,1)) .eq. 2) then
!
                do 7636 n = 1, nno2
                    nfin=n+1
                    if (nfin .gt. nno2) then
                        nfin=nfin-nno2
                    endif
                    dist(n,1)=0.d0
                    dist(n,2)=0.d0
                    if (vrarr(n) .eq. 1) then
                        scal(1)=0.d0
                        scal(2)=0.d0
                        scal(3)=0.d0
                        do 7613 i = 1, ndim
                            scal(1)=scal(1)+dirr(i)*(geom(i,n)-bary(i)&
                            )
                            scal(2)=scal(2)+dirr(i)*(geom(i,nfin)-&
                            bary(i))
7613                     continue
                        scal(1)=sqrt(scal(1)**2.d0)
                        scal(2)=sqrt(scal(2)**2.d0)
                        scal(3)=scal(1)+scal(2)
                        dist(n,1)= dist(n,1)+scal(2)*dirr(2)*(geom(1,&
                        n)-bary(1))
                        dist(n,1)= dist(n,1)+scal(1)*dirr(2)*(geom(1,&
                        nfin)-bary(1))
                        dist(n,1)= dist(n,1)-scal(2)*dirr(1)*(geom(2,&
                        n)-bary(2))
                        dist(n,1)= dist(n,1)-scal(1)*dirr(1)*(geom(2,&
                        nfin)-bary(2))
                        dist(n,1)=dist(n,1)/scal(3)
                    endif
7636             continue
!
                do 5616 n = 1, nno2
                    nfin=n+1
                    if (nfin .gt. nno2) then
                        nfin=nfin-nno2
                    endif
                    if (vrarr(n) .eq. 1) then
                        do 5615 kvois = 1, nbvois
                            nn=0
                            do 5617 i = 1, nbsoco(kvois)
                                if (lisoco(kvois,i,1) .eq. n) nn=nn+1
                                if (lisoco(kvois,i,1) .eq. nfin) nn=nn+ 1
5617                         continue
                            if (nn .eq. 2) then
                                do 5618 gg = 1, npg
                                    vip(nnn,gg)=livois(kvois)
                                    vip(2*nnn-1,gg)=bary(1)+dist(n,1)*&
                                    dirr(2)
                                    vip(2*nnn,gg)=bary(2)-dist(n,1)*&
                                    dirr(1)
5618                             continue
                                nnn=nnn+1
                            endif
5615                     continue
                    endif
5616             continue
            endif
!
            if (nint(vip(2,1)) .eq. 3) then
                compar=0.d0
                do 5636 n = 1, nno2
                    nfin=n+1
                    if (nfin .gt. nno2) then
                        nfin=nfin-nno2
                    endif
                    dist(n,1)=0.d0
                    dist(n,2)=0.d0
                    if (vrarr(n) .eq. 1) then
                        scal(1)=0.d0
                        scal(2)=0.d0
                        scal(3)=0.d0
                        do 5613 i = 1, ndim
                            scal(1)=scal(1)+dirr(i)*(geom(i,n)-bary(i)&
                            )
                            scal(2)=scal(2)+dirr(i)*(geom(i,nfin)-&
                            bary(i))
5613                     continue
                        scal(1)=sqrt(scal(1)**2.d0)
                        scal(2)=sqrt(scal(2)**2.d0)
                        scal(3)=scal(1)+scal(2)
                        dist(n,1)= dist(n,1)+scal(2)*dirr(2)*(geom(1,&
                        n)-bary(1))
                        dist(n,1)= dist(n,1)+scal(1)*dirr(2)*(geom(1,&
                        nfin)-bary(1))
                        dist(n,1)= dist(n,1)-scal(2)*dirr(1)*(geom(2,&
                        n)-bary(2))
                        dist(n,1)= dist(n,1)-scal(1)*dirr(1)*(geom(2,&
                        nfin)-bary(2))
                        dist(n,1)=dist(n,1)/scal(3)
                        dist(n,2)=dist(n,1)*dist(n,1)
                        compar=compar+dist(n,2)/2.d0
                    endif
5636             continue
!
                do 5637 n = 1, nno2
                    if (dist(n,2) .gt. compar) then
                        vrarr(n)=1
                    else
                        vrarr(n)=0
                    endif
5637             continue
                scal(1)=0.d0
                scal(2)=0.d0
                scal(3)=0.d0
                do 5626 n = 1, nno2
                    nfin=n+1
                    if (nfin .gt. nno2) then
                        nfin=nfin-nno2
                    endif
                    if (vrarr(n) .eq. 1) then
                        do 5625 kvois = 1, nbvois
                            nn=0
                            do 5627 i = 1, nbsoco(kvois)
                                if (lisoco(kvois,i,1) .eq. n) nn=nn+1
                                if (lisoco(kvois,i,1) .eq. nfin) nn=nn+ 1
5627                         continue
                            if (nn .eq. 2) then
                                do 5628 gg = 1, npg
                                    vip(5,gg)=livois(kvois)
                                    vip(9,gg)=bary(1)+dist(n,1)*dirr(&
                                    2)
                                    vip(10,gg)=bary(2)-dist(n,1)*dirr(&
                                    1)
                                    vip(11,gg)=bary(1)
                                    vip(12,gg)=bary(2)
5628                             continue
                                scal(1)=bary(1)+dist(n,1)*dirr(2)
                                scal(2)=bary(2)-dist(n,1)*dirr(1)
                                nunu=1
                                nnn=nnn+1
                            endif
5625                     continue
                    endif
5626             continue
                nini=0
                if (nunu .eq. 1) then
                    scal(3)=(scal(1)-bary(1))*(bary(1)-baryo(1))
                    scal(3)=scal(3)+(scal(2)-bary(2))*(bary(2)-baryo(&
                    2))
                    if (scal(3) .lt. 0.d0) then
                        scal(3)=0.d0
                        scal(3)=scal(3)+(baryo(2)-bary(2))**(2.d0)
                        scal(3)=scal(3)+(bary(1)-baryo(1))**(2.d0)
                        scal(3)=scal(3)**(0.5d0)
                        dirr(1)=(baryo(2)-bary(2))/scal(3)
                        dirr(2)=(bary(1)-baryo(1))/scal(3)
                        nini=1
                    endif
                endif
!
            endif
!
        endif
    endif
!
    if (nini .eq. 1) goto 8491
! IL FAUDRA PREVOIR DE POUVOIR SORTIR DU DOMAINE GEOMETRIQUE ADMIS
!  AVEC UN CODRET = 2
!
!
!
    if (rigi) call r8inir(nddl*nddl, 0.d0, matr, 1)
    if (resi) call r8inir(nddl, 0.d0, vect, 1)
    call r8inir(6, 0.d0, sigmam, 1)
!
!    POSITION DES INDICES POUR LES DEPLACEMENTS ET LES DEFORMATIONS
!
    do 10 n = 1, nno2
        do 20 i = 1, ndim
            iu(i,n) = i + (n-1)*(ndim+ndimsi)
 20     continue
        do 30 kl = 1, ndimsi
            ie(kl,n) = kl + ndim + (n-1)*(ndim+ndimsi)
 30     continue
 10 end do
    os = (ndimsi+ndim)*nno2
    do 40 n = 1, nno1-nno2
        do 50 i = 1, ndim
            iu(i,n+nno2) = i + (n-1)*ndim + os
 50     continue
 40 end do
!
!
!    EXTRACTION DES DEPLACEMENTS
!
    do 100 n = 1, nno1
        do 110 i = 1, ndim
            deplm(i+(n-1)*ndim) = ddlm(iu(i,n))
            depld(i+(n-1)*ndim) = ddld(iu(i,n))
110     continue
100 end do
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
!
    do 1000 g = 1, npg
!
!      CALCUL DES ELEMENTS GEOMETRIQUES DE L'EF POUR E-BARRE
!
        call dfdmip(ndim, nno2, axi, geom, g,&
                    iw, vff2(1, g), idfde2, r, wg,&
                    dfdi2)
        call nmepsb(ndim, nno2, axi, vff2(1, g), dfdi2,&
                    ddlm, epsgm(1, 2), gepsm)
        call nmepsb(ndim, nno2, axi, vff2(1, g), dfdi2,&
                    ddld, epsgd(1, 2), geps)
!
!      CALCUL DES ELEMENTS GEOMETRIQUES DE L'EF POUR U
!
        call dfdmip(ndim, nno1, axi, geom, g,&
                    iw, vff1(1, g), idfde1, r, wg,&
                    dfdi1)
        call nmepsi(ndim, nno1, axi, grand, vff1(1, g),&
                    r, dfdi1, deplm, f, epsgm)
        call nmepsi(ndim, nno1, axi, grand, vff1(1, g),&
                    r, dfdi1, depld, f, epsgd)
        call nmmabu(ndim, nno1, axi, grand, dfdi1,&
                    b)
!
!      DEFORMATIONS ET ECARTS EN FIN DE PAS DE TEMPS
!
        call daxpy(18, 1.d0, gepsm, 1, geps,&
                   1)
        do 200 kl = 1, ndimsi
            de(kl) = epsgm(kl,2)+epsgd(kl,2)
200     continue
!
!      LOI DE COMPORTEMENT
!
        call dcopy(ndimsi, sigm(1, g), 1, sigmam, 1)
        call dscal(3, rac2, sigmam(4), 1)
!
        call r8inir(36, 0.d0, p, 1)
        call r8inir(36, 0.d0, pin, 1)
        pin(1,1)=nono
!
        call nmcomp('RIGI', g, 1, ndim, typmod,&
                    mate, compor, crit, instam, instap,&
                    12, epsgm, epsgd, 6, sigmam,&
                    vim(1, g), option, angmas, 36, pin,&
                    sigma, vip(1, g), 72, dsidep, 36,&
                    p, cod(g))
        if (cod(g) .eq. 1) goto 9000
!
        call r8inir(6, 1.d0, p, 7)
!
!      FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
!
        if (resi) then
!
!        VECTEUR FINT:U
            do 300 n = 1, nno1
                do 310 i = 1, ndim
                    kk = iu(i,n)
                    t1 = 0
                    do 320 kl = 1, ndimsi
                        t1 = t1 + sigma(kl)*b(kl,i,n)
320                 continue
                    vect(kk) = vect(kk) + wg*t1
310             continue
300         continue
!
!        VECTEUR FINT:E
            do 350 n = 1, nno2
                do 360 kl = 1, ndimsi
                    kk = ie(kl,n)
                    t1 = 0
                    do 365 pq = 1, ndimsi
                        t1 = t1 + p(kl,pq)*de(pq)*vff2(n,g)
                        t1 = t1 - p(kl,pq)*sigma(pq)*vff2(n,g)
365                 continue
                    t2 = 0
                    do 370 i = 1, ndim
                        do 375 pq = 1, ndimsi
                            t2 = t2 + c*dfdi2(n,i)*p(kl,pq)*geps(pq,i)
375                     continue
370                 continue
                    vect(kk) = vect(kk) + wg*(t1+t2)
360             continue
350         continue
!
!        CONTRAINTES
            call dcopy(ndimsi, sigma, 1, sigp(1, g), 1)
            call dscal(ndimsi-3, 1.d0/rac2, sigp(4, g), 1)
!
        endif
!
! - CALCUL DE LA MATRICE DE RIGIDITE (STOCKAGE LIGNE DE DFI/DUJ)
!
        if (rigi) then
!
!        MATRICE K:U(I,N),U(J,M)
            do 500 n = 1, nno1
                do 510 i = 1, ndim
                    os = nddl*(iu(i,n)-1)
                    do 520 m = 1, nno1
                        do 530 j = 1, ndim
                            kk = os+iu(j,m)
                            t1 = 0
                            do 540 kl = 1, ndimsi
                                do 550 pq = 1, ndimsi
                                    t1 = t1 + dsidep(kl,pq,1)*b(pq,j, m)*b(kl,i,n)
550                             continue
540                         continue
                            matr(kk) = matr(kk) + wg*t1
530                     continue
520                 continue
!
!        MATRICE K:U(I,N),E(PQ,M)
                    do 600 m = 1, nno2
                        do 610 pq = 1, ndimsi
                            kk = os+ie(pq,m)
                            t1 = 0
                            matr(kk) = matr(kk) + wg*t1
610                     continue
600                 continue
510             continue
500         continue
!
!        MATRICE K:E(KL,N),U(J,M)
            do 700 n = 1, nno2
                do 710 kl = 1, ndimsi
                    os = nddl*(ie(kl,n)-1)
                    do 720 m = 1, nno1
                        do 730 j = 1, ndim
                            kk = os+iu(j,m)
                            t1 = 0
                            do 735 pq = 1, ndimsi
                                t1=t1 - dsidep(kl,pq,1)*b(pq,j,m)*&
                                vff2(n,g)
735                         continue
                            matr(kk) = matr(kk) + wg*t1
730                     continue
720                 continue
710             continue
700         continue
!
!        MATRICE K:E(KL,N),E(PQ,M)
!
            do 800 n = 1, nno2
                do 810 m = 1, nno2
                    t1 = vff2(n,g)*vff2(m,g)
                    do 820 i = 1, ndim
                        t1 = t1 + c*dfdi2(n,i)*dfdi2(m,i)
820                 continue
                    do 830 kl = 1, ndimsi
                        do 835 pq = 1, ndimsi
                            kk = (ie(kl,n)-1)*nddl + ie(pq,m)
                            matr(kk) = matr(kk) + wg*t1*p(kl,pq)
835                     continue
830                 continue
810             continue
800         continue
!
        endif
!
1000 end do
!
! - SYNTHESE DES CODES RETOUR
!
9000 continue
!
    call codere(cod, npg, codret)
!
end subroutine
