subroutine cgfint(ndim, nno1, nno2, npg, wref,&
                  vff1, vff2, dffr1, geom, tang,&
                  typmod, option, mat, compor, lgpg,&
                  crit, instam, instap, ddlm, ddld,&
                  iu, iuc, im, a, sigm,&
                  vim, sigp, vip, matr, vect,&
                  codret)
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
! aslint: disable=W1504
    implicit none
!
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/cgcine.h"
#include "asterfort/codere.h"
#include "asterfort/comp1d.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmiclg.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
    character(len=8) :: typmod(*)
    character(len=16) :: option, compor(*), compoz(7)
!
    integer :: ndim, nno1, nno2, npg, mat, lgpg, iu(3, 3), iuc(3), im(3)
    integer :: codret
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg), geom(ndim, nno1), wref(npg)
    real(kind=8) :: crit(*), instam, instap, sigm(3, npg)
    real(kind=8) :: ddlm(nno1*(ndim+1) + nno2), ddld(nno1*(ndim+1) + nno2)
    real(kind=8) :: vim(lgpg, npg), vip(lgpg, npg), vect(nno1*(ndim+1) + nno2)
    real(kind=8) :: dffr1(nno1, npg), tang(*), sigp(3, npg), matr(*)
    real(kind=8) :: a
! ----------------------------------------------------------------------
!
!   RAPH_MECA, RIGI_MECA_* ET FULL_MECA_* POUR L'ELEMENT CABLE/GAINE
!     INSPIRE DE EIFINT
!
! ----------------------------------------------------------------------
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  NNO1    : NOMBRE DE NOEUDS (FAMILLE U)
! IN  NNO2    : NOMBRE DE NOEUDS (FAMILLE L)
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  WREF    : POIDS DES POINTS DE GAUSS DE REFERENCE
! IN  VFF1    : VALEUR DES FONCTIONS DE FORME (FAMILLE U)
! IN  VFF2    : VALEUR DES FONCTIONS DE FORME (FAMILLE L)
! IN  DFFR1   : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE U)
! IN  GEOM    : COORDONNEES DES NOEUDS
! IN  TANG    : TANGENTE AUX NOEUDS
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  OPTION  : OPTION DE CALCUL
! IN  MAT     : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  LGPG    : LONGUEUR DU TABLEAU DES VARIABLES INTERNES
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT PRECEDENT
! IN  INSTAP  : INSTANT DE CALCUL
! IN  DDLM    : DDL A L'INSTANT PRECEDENT
! IN  DDLD    : INCREMENT DES DDL
! IN  IU      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT GA
! IN  IUC     : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPL RELATIF
! IN  IM      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE LAGRANGE
! IN  A       : SECTION DE LA BARRE
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
! OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA   ET FULL_MECA_*)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA   ET FULL_MECA_*)
! OUT MATR    : MATRICE DE RIGIDITE   (RIGI_MECA_* ET FULL_MECA_*)
! OUT VECT    : FORCES INTERIEURES    (RAPH_MECA   ET FULL_MECA_*)
! OUT CODRET  : CODE RETOUR
! ----------------------------------------------------------------------
    character(len=16) :: cmp1, cmp2
    logical :: resi, rigi
    integer :: nddl, g, cod(27), n, i, m, j, kk, codm(3), nbvifr, nbvica
    integer :: nbvi
    real(kind=8) :: rbid, r, mu, epsm, deps, wg, l(3), de, ddedt, t1
    real(kind=8) :: b(4, 3), gliss
    real(kind=8) :: sigcab, dsidep, dde(2), ddedn, courb
    real(kind=8) :: angmas(3), val(3), rphi, frotc, wkin(2)
    character(len=12) :: nom(3)
    character(len=1) :: poum
!
    data nom /'FROT_LIN','PENA_LAG','FROT_COU'/
! ----------------------------------------------------------------------
!
!
! - INITIALISATION
!
    resi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RAPH'
    rigi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RIGI'
    nddl = nno1*(ndim+1) + nno2
!
!
    ASSERT(compor(1).eq.'KIT_CG')
    cmp2(1:16)=compor(8)
    cmp1(1:16)=compor(9)
    do i = 1, 7
        compoz(i)=compor(i)
    end do
    compoz(1)=cmp1
    write (compoz(6),'(I16)') 52
    do g = 1, npg
        cod(g)=0
    end do
    read (compor(2),'(I16)') nbvi
    nbvifr = 2
    nbvica = nbvi - nbvifr
!
    if (rigi) call r8inir(nddl*nddl, 0.d0, matr, 1)
    if (resi) call r8inir(nddl, 0.d0, vect, 1)
!
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
!
    do g = 1, npg
!
!      CALCUL DES ELEMENTS GEOM DE L'EF AU POINT DE GAUSS CONSIDERE
!
        call cgcine(ndim, nno1, vff1(1, g), wref(g), dffr1(1, g),&
                    geom, tang, wg, l, b,&
                    courb)
!
!      CALCUL DU DEPLACEMENT TANGENTIEL DE LA GAINE (UPROJ)
!                DEPLACEMENT DU CABLE               (UCAB)
!             DE LA DEFORMATION DANS LE CABLE       (EPSCAB)
!             DU LAGRANGE                           (MU)
!         AU POINT DE GAUSS CONSIDERE
!
        gliss = 0.d0
        epsm = 0.d0
        deps = 0.d0
        mu=0.d0
        do 161 n = 1, nno1
            do 160 j = 1, ndim
                epsm = epsm + b(j,n)*ddlm(iu(j,n))
                deps = deps + b(j,n)*ddld(iu(j,n))
160          continue
            gliss = gliss + l(n)*(ddlm(iuc(n))+ddld(iuc(n)))
            epsm = epsm + b(ndim+1,n)*ddlm(iuc(n))
            deps = deps + b(ndim+1,n)*ddld(iuc(n))
161      continue
!
        mu = 0.d0
        do 180 n = 1, nno2
            mu = mu + vff2(n,g)*(ddlm(im(n))+ddld(im(n)))
180      continue
!
!
!      LOI DE COMPORTEMENT DU CABLE
!         SORTIE DE CONTRAINTE DANS LE CABLE     (SIGCAB)
!                DES VARIABLES INTERNES DU CABLE (VIP)
!                DE LA TANGENTE D(SIGCAB)/D(EPSCAB)
!
        if (cmp2 .eq. 'ELAS' .or. cmp2 .eq. 'VMIS_ISOT_LINE' .or. cmp2 .eq.&
            'VMIS_ISOT_TRAC' .or. cmp2 .eq. 'CORR_ACIER' .or. cmp2 .eq. 'VMIS_CINE_LINE'&
            .or. cmp2 .eq. 'PINTO_MENEGOTTO' .or. cmp2 .eq. 'VMIS_ASYM_LINE' .or. cmp2 .eq.&
            'SANS') then
!     ---------------------------------------------------
!
!
            call nmiclg('RIGI', g, 1, option, cmp2,&
                        mat, epsm, deps, sigm(1, g)/a, vim(1, g),&
                        sigcab, vip(1, g), dsidep, crit, codret)
!
        else
            call r8inir(3, r8nnem(), angmas, 1)
            call comp1d('RIGI', g, 1, option, sigm(1, g)/a,&
                        epsm, deps, angmas, vim(1, g), vip(1, g),&
                        sigcab, dsidep, codret)
!
        endif
!
!      LOI DE COMPORTEMENT: GLISSEMENT GAINE CABLE
!         ADHERENCE PARFAITE OU GLISSEMENT PARFAIT
!         SORTIE DE GLISSEMENT RELATIF AU POINT DE GAUSS (DE)
!                DE LA TANGENTE D(DE)/D(TAU) AVEC
!                   TAU=L+MU(GLISS)
!      CONVENTIONS :
!       1. MU EST RANGE DANS EPSM(1)
!       2. GLISS EST RANGE DANS EPSD(1)
!       3. DELTA EST RENVOYE DANS SIGP(1)             : DE
!       4. D(DE)/DTAU EST RENVOYE DANS DSIDEP(1,1) : DDEDT
!       5. TENSION ET COURBURE : DANS TABLEAU DE TRAVAIL
!
!    RECUPERATION DES PARAMETRES PHYSIQUES
        if (option .eq. 'RIGI_MECA_TANG') then
            poum = '-'
        else
            poum = '+'
        endif
!
        call rcvalb('RIGI', g, 1, poum, mat,&
                    ' ', 'CABLE_GAINE_FROT', 0, ' ', [0.d0],&
                    3, nom, val, codm, 2)
        rphi=val(1)
        r=val(2)
        frotc = val(3)
        wkin(1)=a*sigcab
        wkin(2)=courb
!
        call nmcomp('RIGI', g, 1, ndim, typmod,&
                    mat, compoz, crit, instam, instap,&
                    3, mu, gliss, 1, rbid,&
                    vim(nbvica+1, g), option, rbid, 2, wkin,&
                    de, vip(nbvica+1, g), 36, dde, 1,&
                    rbid, cod(g))
        if (cod(g) .eq. 1) goto 9000
!
!      FORCE INTERIEURE ET CONTRAINTES DE CAUCHY
!
        if (resi) then
!
!        STOCKAGE DES CONTRAINTES
!        CONVENTION DE RANGEMENT SIGP(1,2,3) EXPLICITE CI-DESSOUS
            sigp( 1,g) = sigcab*a
            sigp( 2,g) = mu + r*(gliss-de)
            sigp( 3,g) = gliss-de
!
!        VECTEUR FINT:U ET UC
            do 300 n = 1, nno1
                do 301 i = 1, ndim
                    kk = iu(i,n)
                    t1 = b(i,n)*sigp(1,g)
                    vect(kk) = vect(kk) + wg*t1
301              continue
                kk=iuc(n)
                t1=b(4,n)*sigp(1,g)+l(n)*sigp(2,g)
                vect(kk)=vect(kk)+wg*t1
300          continue
!
!        VECTEUR FINT:M
            do 350 n = 1, nno2
                kk = im(n)
                t1 = vff2(n,g)*sigp(3,g)
                vect(kk) = vect(kk) + wg*t1
350          continue
!
!
        endif
!
!
! - CALCUL DE LA MATRICE DE RIGIDITE
!   STOCKAGE TRIANGLE INFERIEUR LIGNE DE DFI/DUJ
!
        if (rigi) then
!        MATRICE K:U(I,N),U(J,M)
            ddedt=dde(1)
            ddedn=a*dde(2)
            do 500 n = 1, nno1
                do 501 i = 1, ndim
                    do 510 m = 1, nno1
                        do 511 j = 1, ndim
                            kk=(iu(i,n)-1)*nddl+iu(j,m)
                            t1 = b(i,n)*b(j,m)*dsidep*a
                            matr(kk) = matr(kk) + wg*t1
511                      continue
510                  continue
501              continue
500          continue
!
!        MATRICE K:U(I,N),UC(M)
            do 520 n = 1, nno1
                do 521 i = 1, ndim
                    do 522 m = 1, nno1
                        t1= b(i,n)*b(4,m)*dsidep*a
                        kk=(iu(i,n)-1)*nddl+iuc(m)
                        matr(kk)=matr(kk)+wg*t1
                        t1=t1-r*l(m)*ddedn*dsidep*b(i,n)
                        kk=(iuc(m)-1)*nddl+iu(i,n)
                        matr(kk)=matr(kk)+wg*t1
522                  continue
521              continue
520          continue
!
!
!        MATRICES K:UC(N),UC(M)
            do 620 n = 1, nno1
                do 621 m = 1, nno1
                    t1=b(4,n)*b(4,m)*dsidep*a+r*l(n)*l(m)*(1.d0-r*&
                    ddedt) -r*l(n)*ddedn*dsidep*b(4,m)
                    kk=(iuc(n)-1)*nddl+iuc(m)
                    matr(kk) = matr(kk) + wg*t1
621              continue
620          continue
!
!        MATRICE K:UC(N),MU(M)
            do 640 n = 1, nno1
                do 642 m = 1, nno2
                    t1=l(n)*vff2(m,g)*(1.d0-r*ddedt)
                    kk=(iuc(n)-1)*nddl+im(m)
                    matr(kk)=matr(kk)+wg*t1
                    kk=(im(m)-1)*nddl+iuc(n)
                    matr(kk)=matr(kk)+wg*t1
642              continue
640          continue
!
!        MATRICES K:MU(N),MU(M)
            do 700 n = 1, nno2
                do 710 m = 1, nno2
                    t1 = - vff2(n,g)*ddedt*vff2(m,g)
                    kk=(im(n)-1)*nddl+im(m)
                    matr(kk) = matr(kk) + wg*t1
710              continue
700          continue
!
        endif
!
    end do
!
! - SYNTHESE DES CODES RETOUR
!
9000  continue
    call codere(cod, npg, codret)
!
end subroutine
