subroutine xmvec1(ndim, jnne, ndeple, nnc, jnnm,&
                  hpg, ffc, ffe, ffm,&
                  jacobi, dlagrc, coefcr,&
                  coefcp, lpenac, jeu, norm,&
                  nsinge, nsingm, rre, rrm,&
                  jddle, jddlm, nfhe, nfhm, lmulti,&
                  heavno, heavn, heavfa, vtmp)
!
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
!
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "asterfort/indent.h"
#include "asterfort/xplma2.h"
#include "asterfort/xcalc_code.h"
#include "asterfort/xcalc_heav.h"
    integer :: ndim, jnne(3), jnnm(3), nnc
    integer :: nsinge, nsingm
    real(kind=8) :: hpg, ffc(9), jacobi, ffe(20), ffm(20)
    real(kind=8) :: dlagrc, jeu, norm(3), coefcr, coefcp, rre, rrm
    real(kind=8) :: vtmp(336)
    integer :: ndeple, jddle(2), jddlm(2)
    integer :: nfhe, nfhm, heavno(8), heavfa(*), heavn(*)
    aster_logical :: lpenac, lmulti
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEMGG - CALCUL ELEM.)
!
! VECTEUR SECOND MEMBRE SI CONTACT AVEC COMPLIANCE (XFEM)
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNES   : NOMBRE DE NOEUDS SOMMETS DE LA MAILLE ESCLAVE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFC    : FONCTIONS DE FORME DU POINT DE CONTACT DANS ELC
! IN  FFE    : FONCTIONS DE FORME DU POINT DE CONTACT DANS ESC
! IN  FFM    : FONCTIONS DE FORME DE LA PROJECTION DU PTC DANS MAIT
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  DLAGRC : LAGRANGE DE CONTACT AU POINT D'INTÉGRATION
! IN  COEFCA : COEF_REGU_CONT
! IN  JEU    : VALEUR DU JEU
! IN  NORM   : VALEUR DE LA NORMALE AU POINT DE CONTACT
! IN  NSINGE  : NOMBRE DE FONCTION SINGULIERE ESCLAVE
! IN  NSINGM  : NOMBRE DE FONCTION SINGULIERE MAITRE
! IN  RRE    : SQRT LST ESCLAVE
! IN  RRM    : SQRT LST MAITRE
! I/O VTMP   : VECTEUR SECOND MEMBRE ELEMENTAIRE DE CONTACT/FROTTEMENT
! ----------------------------------------------------------------------
    integer :: i, j, ii, pl, iin, nddle
    integer :: nne, nnes, nnem, nnm, nnms, ddles, ddlem, ddlms, ddlmm
    integer :: ifh, iddl, hea_fa(2)
    real(kind=8) :: vv, iescl(6), imait(6)
! ----------------------------------------------------------------------
!
!
! --- INITIALISATION
!
    iescl(1) = 1
    iescl(2) =-1
    iescl(2+nfhe)=-rre
    imait(1) = 1
    imait(2) = 1
    imait(2+nfhm)= rrm
!    DEFINITION A LA MAIN DE LA TOPOLOGIE DE SOUS-DOMAINE PAR FACETTE (SI NFISS=1)
    if (.not.lmulti) then
      hea_fa(1)=xcalc_code(1,he_inte=[-1])
      hea_fa(2)=xcalc_code(1,he_inte=[+1])
    endif
!
! --------------------- CALCUL DE [L1_CONT]-----------------------------
!
    nne=jnne(1)
    nnes=jnne(2)
    nnem=jnne(3)
    nnm=jnnm(1)
    nnms=jnnm(2)
    ddles=jddle(1)
    ddlem=jddle(2)
    ddlms=jddlm(1)
    ddlmm=jddlm(2)
    nddle = ddles*nnes+ddlem*nnem
!
    if (nnm .ne. 0) then
!
        do 10 j = 1, ndim
            do 20 i = 1, ndeple
                call indent(i, ddles, ddlem, nnes, iin)
                if (lpenac) then
                    vv = hpg*jacobi*dlagrc* ffe(i)*norm(j)
                else
                    vv = hpg*jacobi*(dlagrc-coefcr*jeu) * ffe(i)*norm( j)
                endif
                if (lmulti) then
                    do 15 ifh = 1, nfhe
                        iescl(1+ifh)=xcalc_heav(heavn(nfhe*(i-1)+ifh),&
                                                heavfa(1),&
                                                heavn(nfhe*nne+nfhm*nnm+i))
 15                 continue
                else
                        iescl(2)=xcalc_heav(heavn(i),&
                                            hea_fa(1),&
                                            heavn(nfhe*nne+nfhm*nnm+i))
                endif
                do 25 iddl = 1, 1+nfhe+nsinge
                    ii = iin + (iddl-1)*ndim + j
                    vtmp(ii) = -iescl(iddl)*vv
 25             continue
 20         continue
            do 30 i = 1, nnm
                call indent(i, ddlms, ddlmm, nnms, iin)
                iin = iin + nddle
                if (lpenac) then
                    vv = hpg*jacobi*dlagrc* ffm(i)*norm(j)
                else
                    vv = hpg*jacobi*(dlagrc-coefcr*jeu) * ffm(i)*norm( j)
                endif
                if (lmulti) then
                    do 35 ifh = 1, nfhm
                        imait(1+ifh)=xcalc_heav(heavn(nfhe*nne+nfhm*(i-1)+ifh),&
                                                heavfa(2),&
                                                heavn((1+nfhe)*nne+nfhm*nnm+i))
 35                 continue
                else
                        imait(2)=xcalc_heav(heavn(nne+i),&
                                            hea_fa(2),&
                                            heavn((1+nfhe)*nne+nfhm*nnm+i))  
                endif
                do 45 iddl = 1, 1+nfhm+nsingm
                    ii = iin + (iddl-1)*ndim + j
                    vtmp(ii) = imait(iddl)*vv
 45             continue
 30         continue
 10     continue
    else
        do 50 j = 1, ndim
            do 60 i = 1, ndeple
! --- BLOCS ES,SI
                if (lpenac) then
                    vv = hpg*jacobi*dlagrc* ffe(i)*norm(j)
                else
                    vv = hpg*jacobi*(dlagrc-coefcr*jeu) * ffe(i)*norm( j)
                endif
                call indent(i, ddles, ddlem, nnes, iin)
                ii = iin + j
                vtmp(ii) = rre * vv
 60         continue
 50     continue
    endif
!
! --------------------- CALCUL DE [L2]----------------------------------
!
    do 40 i = 1, nnc
        call xplma2(ndim, nne, nnes, ddles, i,&
                    nfhe, pl)
        if (lmulti) pl = pl + (heavno(i)-1)*ndim
        if (lpenac) then
            vtmp(pl) = -hpg*jacobi*(dlagrc/coefcp+jeu) *ffc(i)
        else
            vtmp(pl) = -hpg*jacobi*jeu*ffc(i)
        endif
 40 continue
!
end subroutine
