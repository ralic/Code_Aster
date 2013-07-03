subroutine poslog(resi, rigi, tn, tp, fm,&
                  lgpg, vip, ndim, fp, g,&
                  dtde, sigm, cplan, fami, mate,&
                  instp, angmas, gn, lamb, logl,&
                  sigp, dsidep, pk2m, pk2, codret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!     BUT:  POST TRAITEMENT GRANDES DEFORMATIONS 2D ET 3D LOG
!     SUIVANT ARTICLE MIEHE APEL LAMBRECHT CMAME 2002
!     CONFIGURATION LAGRANGIENNE
! ----------------------------------------------------------------------
! IN  RESI    : .TRUE. SI FULL_MECA/RAPH_MECA .FALSE. SI RIGI_MECA_TANG
! IN  RIGI    : .TRUE. SI FULL_MECA/RIGI_MECA_TANG
! IN  TN      : CONTRAINTES ASSOCIEES AUX DEF. LOGARITHMIQUES EN T-
! IN  TP      : CONTRAINTES ASSOCIEES AUX DEF. LOGARITHMIQUES EN T+
! IN  FM      : GRADIENT TRANSFORMATION EN T-
! IN  LGPG    : DIMENSION DU VECTEUR DES VAR. INTERNES POUR 1 PT GAUSS
! VAR VIP     : VARIABLES INTERNES EN T+
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  FP      : GRADIENT TRANSFORMATION EN T+
! IN  PES     : OPERATEUR DE TRANSFORMATION TN (OU TP) EN PK2
! IN  G       : NUMERO DU POINTS DE GAUSS
! IN  DTDE    : OPERATEUR TANGENT ISSU DE NMCOMP (6,6)
! IN  SIGM    : CONTRAINTES DE CAUCHY EN T-
! IN  GN      : TERMES UTILES AU CALCUL DE TL DANS POSLOG
! IN  FETA    : TERMES UTILES AU CALCUL DE TL DANS POSLOG
! IN  XI      : TERMES UTILES AU CALCUL DE TL DANS POSLOG
! IN  ME      : TERMES UTILES AU CALCUL DE TL DANS POSLOG
! OUT SIGP    : CONTRAINTES DE CAUCHY EN T+
!
! aslint: disable=W1306,W1504
    implicit none
#include "asterfort/d1macp.h"
#include "asterfort/deflg2.h"
#include "asterfort/deflg3.h"
#include "asterfort/lcdetf.h"
#include "asterfort/lctr2m.h"
#include "asterfort/pk2sig.h"
#include "asterfort/pmat.h"
#include "asterfort/r8inir.h"
#include "asterfort/symt46.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    integer :: ndim, i, j, kl, g, lgpg, ivtn, mate, codret
    character(len=*) :: fami
    real(kind=8) :: dtde(6, 6), trav(6, 6), trav2(6, 6)
    real(kind=8) :: pes(6, 6), sigm(2*ndim), sigp(2*ndim), tp2(6), tn(6)
    real(kind=8) :: fr(3, 3), vip(lgpg)
    real(kind=8) :: detf, dsidep(6, 6), rac2, instp, angmas(*)
    real(kind=8) :: pk2(6), tp(6), fp(3, 3), fm(3, 3), pk2m(6)
    real(kind=8) :: tl(3, 3, 3, 3), tls(6, 6), epse(4), d1(4, 4)
    real(kind=8) :: gn(3, 3), feta(4), xi(3, 3), me(3, 3, 3, 3), lamb(3)
    real(kind=8) :: logl(3)
    logical :: resi, rigi, cplan
! ---------------------------------------------------------------------
!********************CONTRAINTE ET FORCES INTERIEURES******************
!
    rac2=sqrt(2.d0)
    codret=0
!
!
!     CALCUL DES PRODUITS SYMETR. DE F PAR N
    if (resi) then
        call dcopy(9, fp, 1, fr, 1)
    else
        call dcopy(9, fm, 1, fr, 1)
    endif
!
!     DETERMINANT DE LA MATRICE F A L INSTANT T+
    call lcdetf(ndim, fr, detf)
!
!     PERTINENCE DES GRANDEURS
    if (detf .le. 1.d-2 .or. detf .gt. 1.d2) then
        codret = 1
        goto 9999
    endif
!
! CORRECTION POUR LES CONTRAINTES PLANES
! NE FONCTIONNE QUE SI DET(F_PLAS)=1  SOIT DEF. PLAS. INCOMPRESSIBLES
! CF. COMP. METHODES FOR PLASTICITY - DE SOUZA-NIETO, PERIC, OWEN p.603
!
    if (cplan) then
        call r8inir(4, 0.d0, epse, 1)
        if (resi) then
            call d1macp(fami, mate, instp, '+', g,&
                        1, angmas, d1)
            do 10 i = 1, 4
                do 10 j = 1, 4
                    epse(i)=epse(i)+d1(i,j)*tp(j)
10              continue
!           EN ELASTICITE ISTROPE
            epse(3)=d1(1,2)*(tp(1)+tp(2))
        else
            call d1macp(fami, mate, instp, '-', g,&
                        1, angmas, d1)
            do 11 i = 1, 4
                do 11 j = 1, 4
                    epse(i)=epse(i)+d1(i,j)*tn(j)
11              continue
!           EN ELASTICITE ISTROPE
            epse(3)=d1(1,2)*(tn(1)+tn(2))
        endif
        detf=exp(epse(1)+epse(2)+epse(3))
    endif
!
! ********************* TENSEUR DE PASSAGE DE T A PK2*******************
!
    call deflg2(gn, lamb, logl, pes, feta,&
                xi, me)
!
!
! *********************MATRICE TANGENTE(SYMETRIQUE)*********************
    if (rigi) then
!
!        POUR LA RIGIDITE GEOMETRIQUE : CALCUL AVEC LES PK2
        call r8inir(6, 0.d0, tp2, 1)
        if (.not.resi) then
            call pk2sig(ndim, fm, detf, pk2m, sigm,&
                        -1)
            do 54 kl = 4, 2*ndim
                pk2m(kl)=pk2m(kl)*rac2
54          continue
            call dcopy(6, tn, 1, tp2, 1)
        else
            call dcopy(6, tp, 1, tp2, 1)
        endif
!
        call deflg3(gn, feta, xi, me, tp2,&
                    tl)
!
        call symt46(tl, tls)
!
        call r8inir(36, 0.d0, dsidep, 1)
        call lctr2m(6, pes, trav)
        call pmat(6, trav, dtde, trav2)
        call pmat(6, trav2, pes, dsidep)
!
        call daxpy(36, 1.d0, tls, 1, dsidep,&
                   1)
!
    endif
!
    if (resi) then
!
!        TRANSFORMATION DU TENSEUR T EN PK2
!
        call r8inir(6, 0.d0, pk2, 1)
        do 51 i = 1, 6
            do 52 j = 1, 6
                pk2(i)=pk2(i)+tp(j)*pes(j,i)
52          continue
51      continue
!        CALCUL DES CONTRAINTES DE CAUCHY, CONVERSION LAGRANGE -> CAUCHY
        call pk2sig(ndim, fp, detf, pk2, sigp,&
                    1)
!
!C       --------------------------------
!C       pour gagner du temps : on stocke TP comme variable interne
!C       --------------------------------
        ivtn=lgpg-6+1
        call dcopy(2*ndim, tp, 1, vip(ivtn), 1)
!
    endif
!
9999  continue
!
end subroutine
