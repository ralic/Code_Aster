subroutine xmvef1(ndim, jnne, jnnm, ndeple, nnc,&
                  hpg, ffc, ffe,&
                  ffm, jacobi, dlagrc, dlagrf,&
                  coeffr, lpenaf, coefff, tau1,&
                  tau2, rese, mproj, coefcr,&
                  jeu, nsinge, nsingm, rre,&
                  rrm, nvit, nconta, jddle, jddlm,&
                  nfhe, nfhm, lmulti, heavn, heavfa,&
                  vtmp)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "asterfort/indent.h"
#include "asterfort/xplma2.h"
#include "asterfort/xcalc_heav.h"
#include "asterfort/xcalc_code.h"
    integer :: ndim, nnc, jnne(3), jnnm(3)
    integer :: nsinge, nsingm, nvit, jddle(2), jddlm(2), nfhe
    integer :: nfhm, heavn(*), heavfa(*)
    real(kind=8) :: hpg, ffc(9), ffe(20), ffm(20), jacobi
    real(kind=8) :: dlagrc, dlagrf(2), jeu
    real(kind=8) :: coefff, coeffr, rre, rrm, coefcr
    real(kind=8) :: tau1(3), tau2(3), rese(3), mproj(3, 3), vtmp(336)
    integer :: nconta, ndeple
    aster_logical :: lpenaf, lmulti
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEMGG - CALCUL ELEM.)
!
! CALCUL DU SECOND MEMBRE POUR LE FROTTEMENT
! CAS AVEC CONTACT
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNES   : NOMBRE DE NOEUDS SOMMETS DE LA MAILLE ESCLAVE
! IN  NNC    : NOMBRE DE NOEUDS DE CONTACT
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  NFAES  : NUMERO DE LA FACETTE DE CONTACT ESCLAVE
! IN  CFACE  : MATRICE DE CONECTIVITE DES FACETTES DE CONTACT
! IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFC    : FONCTIONS DE FORME DU POINT DE CONTACT DANS ELC
! IN  FFE    : FONCTIONS DE FORME DU POINT DE CONTACT DANS ESC
! IN  FFM    : FONCTIONS DE FORME DE LA PROJECTION DU PTC DANS MAIT
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  JPCAI  : POINTEUR VERS LE VECT DES ARRETES ESCLAVES INTERSECTEES
! IN  COEFFA : COEF_REGU_FROT
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! IN  TAU1   : PREMIERE TANGENTE
! IN  TAU2   : SECONDE TANGENTE
! IN  RESE   : PROJECTION DE LA BOULE UNITE POUR LE FROTTEMENT
! IN  MPROJ  : MATRICE DE L'OPERATEUR DE PROJECTION
! IN  DLAGRF : LAGRANGES DE FROTTEMENT AU POINT D'INTÉGRATION
! IN  TYPMAI : NOM DE LA MAILLE ESCLAVE D'ORIGINE (QUADRATIQUE)
! IN  NSINGE : NOMBRE DE FONCTION SINGULIERE ESCLAVE
! IN  NSINGM : NOMBRE DE FONCTION SINGULIERE MAITRE
! IN  RRE    : SQRT LST ESCLAVE
! IN  RRM    : SQRT LST MAITRE
! IN  NVIT   : POINT VITAL OU PAS
! IN  INADH  : POINT ADHERENT OU PAS
! I/O VTMP   : VECTEUR SECOND MEMBRE ELEMENTAIRE DE CONTACT/FROTTEMENT
! ----------------------------------------------------------------------
    integer :: i, j, k, ii, pli, iin, nddle, hea_fa(2)
    integer :: nne, nnes, nnem, nnm, nnms, ddles, ddlem, ddlms, ddlmm
    real(kind=8) :: vectt(3), tt(2), vv, t, iescl(3), imait(3)
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS
!
!  CETTE ROUTINE N AUTORISE QU UNE FACETTE MONOFISSUREE
!  ON DIMENSIONNE LES CHAMPS DE SIGNES SELON CETTE HYPOTHESE 
    iescl(1) = 1.d0
    iescl(2) = -1.d0
    iescl(3)= -rre
    imait(1) = 1.d0
    imait(2) = 1.d0
    imait(3)= rrm
    if (.not.lmulti) then
      hea_fa(1)=xcalc_code(1,he_inte=[-1])
      hea_fa(2)=xcalc_code(1,he_inte=[+1])
    endif
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
    do 100 i = 1, 3
        vectt(i) = 0.d0
100 continue
    do 110 i = 1, 2
        tt(i) = 0.d0
110 continue
!
! --- CALCUL DE RESE.C(*,I)
!
    do 120 i = 1, ndim
        do 130 k = 1, ndim
            vectt(i) = rese(k)*mproj(k,i) + vectt(i)
130     continue
120 continue
!
! --- CALCUL DE T.(T-P)
!
    do 140 i = 1, ndim
        t = dlagrf(1)*tau1(i)+dlagrf(2)*tau2(i)-rese(i)
        tt(1)= t*tau1(i)+tt(1)
        if (ndim .eq. 3) tt(2)= t*tau2(i)+tt(2)
140 continue
!
! --------------------- CALCUL DE [L1_FROT]-----------------------------
!
    if (nnm .ne. 0) then
!
        do 10 j = 1, ndim
            do 20 i = 1, ndeple
                if (lmulti) then
                    iescl(2)=xcalc_heav(heavn(nfhe*(i-1)+1),&
                                            heavfa(1),&
                                            heavn(nfhe*nne+nfhm*nnm+i))
                else
                    iescl(2)=xcalc_heav(heavn(i),&
                                            hea_fa(1),&
                                            heavn(nfhe*nne+nfhm*nnm+i))
                endif
! --- BLOCS ES,CL ; ES,EN ; (ES,SI)
                if (nconta .eq. 3 .and. ndim .eq. 3) then
                    vv = jacobi*hpg*coefff*(dlagrc-coefcr*jeu)*vectt( j)*ffe(i)
                else
                    vv = jacobi*hpg*coefff*dlagrc*vectt(j)*ffe(i)
                endif
                call indent(i, ddles, ddlem, nnes, iin)
                ii = iin + j
                vtmp(ii) = -vv
                ii = ii + ndim
                vtmp(ii) = -vv*iescl(2)
                do 25 k = 1, nsinge
                    ii = ii + ndim
                    vtmp(ii) = rre * vv
 25             continue
 20         continue
            do 30 i = 1, nnm
                if (lmulti) then
                    imait(2)=xcalc_heav(heavn(nfhe*nne+nfhm*(i-1)+1),&
                                            heavfa(2),&
                                            heavn((1+nfhe)*nne+nfhm*nnm+i))
                else
                    imait(2)=xcalc_heav(heavn(nne+i),&
                                        hea_fa(2),&
                                        heavn((1+nfhe)*nne+nfhm*nnm+i))
                endif
                if (nconta .eq. 3 .and. ndim .eq. 3) then
                    vv = jacobi*hpg*coefff* (dlagrc-coefcr*jeu)*vectt( j)*ffm(i)
                else
                    vv = jacobi*hpg*coefff* dlagrc*vectt(j)*ffm(i)
                endif
                call indent(i, ddlms, ddlmm, nnms, iin)
                ii = nddle + iin + j
                vtmp(ii) = vv
                ii = ii + ndim
                vtmp(ii) = vv*imait(2)
                do 35 k = 1, nsingm
                    ii = ii + ndim
                    vtmp(ii) = rrm * vv
 35             continue
 30         continue
 10     continue
    else
!
        do 60 j = 1, ndim
            do 70 i = 1, ndeple
! --- BLOCS ES,SI
                if (nconta .eq. 3 .and. ndim .eq. 3) then
                    vv = jacobi*hpg*coefff* (dlagrc-coefcr*jeu)*vectt( j)*ffe(i)
                else
                    vv = jacobi*hpg*coefff* dlagrc*vectt(j)*ffe(i)
                endif
                call indent(i, ddles, ddlem, nnes, iin)
                ii = iin + j
                vtmp(ii) = rre * vv
 70         continue
 60     continue
    endif
!
! --------------------- CALCUL DE [L3]----------------------------------
!
    if (nvit .eq. 1) then
        do 40 i = 1, nnc
            call xplma2(ndim, nne, nnes, ddles, i,&
                        nfhe, pli)
            do 50 j = 1, ndim-1
                ii = pli+j
                if (lpenaf) then
                    vtmp(ii) = jacobi*hpg*tt(j)*ffc(i)
                else
                    if (nconta .eq. 3 .and. ndim .eq. 3) then
                        vtmp(ii) = jacobi*hpg*tt(j)*ffc(i)/coeffr
                    else
                        vtmp(ii) = jacobi*hpg*tt(j)*ffc(i)*coefff* dlagrc/coeffr
                    endif
                endif
 50         continue
 40     continue
    endif
!
end subroutine
