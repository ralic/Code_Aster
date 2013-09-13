subroutine nmfihm(ndim, nddl, nno1, nno2, npg,&
                  lgpg, ipg, wref, vff1, vff2,&
                  idf2, dffr2, mate, option, geom,&
                  ddlm, ddld, iu, ip, sigm,&
                  sigp, vect, matr, vim, vip,&
                  tm, tp, crit, compor, typmod)
!
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
! person_in_charge: jerome.laverne at edf.fr
!
! aslint: disable=W1306,W1504
    implicit none
#include "asterfort/ejcine.h"
#include "asterfort/gedisc.h"
#include "asterfort/nmcomp.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
    integer :: ndim, mate, npg, ipg, idf2, lgpg, nno1, nno2, nddl, iu(3, 16)
    integer :: ip(4)
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg), dffr2(ndim-1, nno2, npg)
    real(kind=8) :: wref(npg), geom(ndim, nno2), ddlm(nddl), ddld(nddl), tm, tp
    real(kind=8) :: sigm(2*ndim-1, npg), sigp(2*ndim-1, npg)
    real(kind=8) :: vect(nddl), matr(nddl*nddl)
    real(kind=8) :: vim(lgpg, npg), vip(lgpg, npg)
    character(len=8) :: typmod(*)
    character(len=16) :: option, compor(*)
!-----------------------------------------------------------------------
!  OPTIONS DE MECANIQUE NON LINEAIRE POUR JOINT ET JOINT_HYME 2D ET 3D
!
!    OPTIONS DE CALCUL
!       * RAPH_MECA      : DDL = DDL- + DDDL  ->   SIGP , FINT
!       * FULL_MECA      : DDL = DDL- + DDDL  ->   SIGP , FINT , KTAN
!       * RIGI_MECA_TANG : DDL = DDL-       ->                  KTAN
!-----------------------------------------------------------------------
! IN  NDIM   DIMENSION DE L'ESPACE
! IN  NDDL   NOMBRE DE DEGRES DE LIBERTE TOTAL
! IN  NNO1   NOMBRE DE NOEUDS DE LA FACE POUR LES DEPLACEMENTS
! IN  NNO2   NOMBRE DE NOEUDS DE LA FACE POUR LES PRESSIONS ET LA GEOM
! IN  NPG    NOMBRE DE POINTS DE GAUSS
! IN  LGPG   NOMBRE DE VARIABLES INTERNES
! IN  WREF   POIDS DE REFERENCE DES POINTS DE GAUSS
! IN  VFF1   VALEUR DES FONCTIONS DE FORME (DE LA FACE) POUR U
! IN  VFF2   VALEUR DES FONCTIONS DE FORME (DE LA FACE) POUR P ET X
! IN  DFFR2  DERIVEE DES FONCTIONS DE FORME DE REFERENCE DE P ET X EN G
! IN  MATE   MATERIAU CODE
! IN  OPTION OPTION DE CALCUL
! IN  GEOM   COORDONNEES NOEUDS DE PRESSION (2D:SEG2,3D:TRIA3 OU QUAD4)
! IN  DDLM   VALEURS DES DEGRES DE LIBERTE A L'INSTANT MOINS
! IN  DDLD   VALEURS DES INCREMEMNT DES DEGRES DE LIBERTE
! IN  IU     DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT
! IN  IP     DECALAGE D'INDICE POUR ACCEDER AUX DDL DE PRESSION
! IN  SIGM   CONTRAINTES -         (RAPH_MECA   ET FULL_MECA_*)
! IN  VIM    VARIABLES INTERNES AU DEBUT DU PAS DE TEMPS
! IN  TM     INSTANT -
! IN  TP     INSTANT +
! IN  CRIT   VALEURS DE L'UTILISATEUR POUR LES CRITERES DE CONVERGENCE
! IN  COMPOR NOM DE LA LOI DE COMPORTEMENT
! IN  TYPMOD TYPE DE LA MODELISATION
!
! OUT SIGP    : CONTRAINTES +         (RAPH_MECA   ET FULL_MECA_*)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA   ET FULL_MECA_*)
! OUT MATR    : MATRICE DE RIGIDITE   (RIGI_MECA_* ET FULL_MECA_*)
! OUT VECT    : FORCES INTERIEURES    (RAPH_MECA   ET FULL_MECA_*)
!-----------------------------------------------------------------------
!
    logical :: resi, rigi, axi, ifhyme
    integer :: i, j, kk, m, n, os, p, q, ibid, kpg, ncooro
    real(kind=8) :: dsidep(6, 6), b(2*ndim-1, ndim+1, 2*nno1+nno2)
    real(kind=8) :: sigmo(6), sigma(6), epsm(6), deps(6), wg
    real(kind=8) :: coopg(ndim+1, npg), rot(ndim*ndim)
    real(kind=8) :: coorot(ndim+ndim*ndim, npg)
    real(kind=8) :: crit, rbid, presgm, presgd, temp
!
    axi = .false.
    resi = option.eq.'RAPH_MECA' .or. option(1:9).eq.'FULL_MECA'
    rigi = option(1:9).eq.'FULL_MECA'.or.option(1:10).eq.'RIGI_MECA_'
!
! IFHYME = TRUE  : CALCUL COUPLE HYDRO-MECA
! IFHYME = FALSE : CALCUL MECA SANS HYDRO ET ELIMINATION DES DDL DE PRES
! (FINT_P=0, KTAN_PP=IDENTITE, KTAN_UP=0)
    if (typmod(2) .eq. 'EJ_HYME') ifhyme=.true.
    if (typmod(2) .eq. 'ELEMJOIN') ifhyme=.false.
!
    if (.not. resi .and. .not. rigi) then
        call utmess('F', 'ALGORITH7_61', sk=option)
    endif
!
!     INITIALISATIONS :
    if (resi) call r8inir(nddl, 0.d0, vect, 1)
    if (rigi) call r8inir(nddl*nddl, 0.d0, matr, 1)
!
!     CALCUL DES COORDONNEES DES POINTS DE GAUSS
    call gedisc(ndim, nno2, npg, vff2, geom,&
                coopg)
!
!     BOUCLE SUR LES PG
    do 11 kpg = 1, npg
!
!       CALCUL DE LA MATRICE CINEMATIQUE
        call ejcine(ndim, axi, nno1, nno2, vff1(1, kpg),&
                    vff2(1, kpg), wref(kpg), dffr2(1, 1, kpg), geom, wg,&
                    kpg, ipg, idf2, rot, b)
!
!       CALCUL DES DEFORMATIONS (SAUTS ET GRADIENTS DE PRESSION)
        call r8inir(6, 0.d0, epsm, 1)
        call r8inir(6, 0.d0, deps, 1)
!
        do 150 i = 1, ndim
            do 160 j = 1, ndim
                do 161 n = 1, 2*nno1
                    epsm(i) = epsm(i) + b(i,j,n)*ddlm(iu(j,n))
                    deps(i) = deps(i) + b(i,j,n)*ddld(iu(j,n))
161              continue
160          continue
150      continue
!
        do 151 i = ndim+1, 2*ndim-1
            do 163 n = 1, nno2
                epsm(i) = epsm(i) + b(i,ndim+1,2*nno1+n)*ddlm(ip(n))
                deps(i) = deps(i) + b(i,ndim+1,2*nno1+n)*ddld(ip(n))
163          continue
151      continue
!
!       CALCUL DE LA PRESSION AU POINT DE GAUSS
        presgm = 0.d0
        presgd = 0.d0
        do 164 n = 1, nno2
            presgm = presgm + ddlm(ip(n))*vff2(n,kpg)
            presgd = presgd + ddld(ip(n))*vff2(n,kpg)
164      continue
!
!       STOCKAGE DE LA PRESSION DE FLUIDE AU PG
!       POUR LA VI DE POST-TRAITEMENT DANS LA LDC
        epsm(2*ndim) = presgm
        deps(2*ndim) = presgd
!
!       COOROT : COORDONNEES DU PG + MATRICE DE ROTATION
!       (MATRICE UTILE POUR LES VI DE POST-TRAITEMENT DANS LA LDC)
        do 165 j = 1, ndim
            coorot(j,kpg)=coopg(j,kpg)
165      continue
        do 166 j = 1, ndim*ndim
            coorot(ndim+j,kpg)=rot(j)
166      continue
        ncooro=ndim+ndim*ndim
!
!       CONTRAINTES -
        call r8inir(6, 0.d0, sigmo, 1)
        do 13 n = 1, 2*ndim-1
            sigmo(n) = sigm(n,kpg)
13      continue
!
! - APPEL A LA LOI DE COMPORTEMENT
        call nmcomp('RIGI', kpg, 1, ndim, typmod,&
                    mate, compor, crit, tm, tp,&
                    6, epsm, deps, 6, sigmo,&
                    vim(1, kpg), option, rbid, ncooro, coorot(1, kpg),&
                    sigma, vip(1, kpg), 36, dsidep, 1,&
                    rbid, ibid)
!
! - CONTRAINTE ET EFFORTS INTERIEURS
!
        if (resi) then
!
!         CONTRAINTES +
            do 12 n = 1, 2*ndim-1
                sigp(n,kpg) = sigma(n)
12          continue
!
!         VECTEUR FINT : U
            do 300 n = 1, 2*nno1
                do 301 i = 1, ndim
!
                    kk = iu(i,n)
                    temp = 0.d0
                    do 320 j = 1, ndim
                        temp = temp + b(j,i,n)*sigp(j,kpg)
320                  continue
!
                    vect(kk) = vect(kk) + wg*temp
!
301              continue
300          continue
!
!         VECTEUR FINT : P
            do 302 n = 1, nno2
!
                kk = ip(n)
                temp = 0.d0
                do 321 i = ndim+1, 2*ndim-1
                    temp = temp + b(i,ndim+1,2*nno1+n)*sigp(i,kpg)
321              continue
                if (ifhyme) then
                    vect(kk) = vect(kk) + wg*temp
                else
!             SI IFHYME=FALSE => FINT_P=0
                    vect(kk) = 0.d0
                endif
!
302          continue
!
        endif
!
! - MATRICE TANGENTE
!
        if (rigi) then
!
!         MATRICE K:U(I,N),U(J,M)
            do 500 n = 1, 2*nno1
                do 501 i = 1, ndim
!
                    os = (iu(i,n)-1)*nddl
!
                    do 520 m = 1, 2*nno1
                        do 521 j = 1, ndim
!
                            kk = os + iu(j,m)
                            temp = 0.d0
!
                            do 540 p = 1, ndim
                                do 550 q = 1, ndim
                                    temp = temp + b(p,i,n)*dsidep(p,q) *b(q,j,m)
550                              continue
540                          continue
!
                            matr(kk) = matr(kk) + wg*temp
!
521                      continue
520                  continue
!
501              continue
500          continue
!
!         MATRICE K:P(N),P(M)
            do 502 n = 1, nno2
!
                os = (ip(n)-1)*nddl
!
                do 522 m = 1, nno2
!
                    kk = os + ip(m)
                    temp = 0.d0
!
                    do 542 p = ndim+1, 2*ndim-1
                        do 552 q = ndim+1, 2*ndim-1
                            temp = temp + b(p,ndim+1,2*nno1+n)*dsidep( p,q) *b(q,ndim+1,2*nno1+m)
552                      continue
542                  continue
                    if (ifhyme) then
                        matr(kk) = matr(kk) + wg*temp
                    else
!               SI IFHYME=FALSE => K_PP=IDENTITE
                        if (n .eq. m) then
                            matr(kk) = 1.d0
                        else
                            matr(kk) = 0.d0
                        endif
                    endif
!
522              continue
502          continue
!
!         MATRICE K:P(N),U(J,M)
!         ATTENTION, TERME MIS A ZERO, VERIFICATION NECESSAIRE
            do 503 n = 1, nno2
!
                os = (ip(n)-1)*nddl
!
                do 523 m = 1, 2*nno1
                    do 533 j = 1, ndim
!
                        kk = os + iu(j,m)
                        temp = 0.d0
!
                        do 543 p = ndim+1, 2*ndim-1
                            do 553 q = 1, ndim
!
                                temp = temp + b(p,ndim+1,2*nno1+n) *dsidep(p,q)*b(q,j,m)*0.d0
553                          continue
543                      continue
!
                        if (ifhyme) then
                            matr(kk) = matr(kk) + wg*temp
                        else
!               SI IFHYME=FALSE => K_PU=0.
                            matr(kk)=0.d0
                        endif
!
533                  continue
523              continue
503          continue
!
!         MATRICE K:U(I,N),P(M)
            do 504 n = 1, 2*nno1
                do 514 i = 1, ndim
!
                    os = (iu(i,n)-1)*nddl
!
                    do 524 m = 1, nno2
!
                        kk = os + ip(m)
                        temp = -b(1,i,n)*vff2(m,kpg)
!
                        if (ifhyme) then
                            matr(kk) = matr(kk) + wg*temp
                        else
!               SI IFHYME=FALSE => K_UP=0.
                            matr(kk)=0.d0
                        endif
!
524                  continue
!
514              continue
504          continue
!
        endif
!
11  end do
!
end subroutine
