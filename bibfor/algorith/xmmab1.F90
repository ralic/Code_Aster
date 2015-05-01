subroutine xmmab1(ndim, jnne, ndeple, nnc, jnnm,&
                  hpg, ffc, ffe,&
                  ffm, jacobi, lambda, coefcr,&
                  dvitet, coeffr, jeu,&
                  coeffp, coefff, lpenaf, tau1, tau2,&
                  rese, mproj, norm, nsinge,&
                  nsingm, rre, rrm, nvit, nconta,&
                  jddle, jddlm, nfhe, mmat)
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
#include "blas/ddot.h"
    integer :: ndim, jnne(3), nnc, jnnm(3)
    integer :: nsinge, nsingm, nconta, jddle(2), jddlm(2)
    integer :: nvit, ndeple, nfhe
    real(kind=8) :: hpg, ffc(8), ffe(20), ffm(20), jacobi, norm(3), coeffp
    real(kind=8) :: lambda, coefff, coeffr, rre, rrm, coefcr, dvitet(3)
    real(kind=8) :: tau1(3), tau2(3), rese(3), mmat(336, 336), mproj(3, 3)
    real(kind=8) :: jeu
    aster_logical :: lpenaf
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEMGG - CALCUL ELEM.)
!
! CALCUL DE B ET DE BT POUR LE CONTACT METHODE CONTINUE
! AVEC ADHERENCE
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
! IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFC    : FONCTIONS DE FORME DU PT CONTACT DANS ELC
! IN  FFE    : FONCTIONS DE FORME DU PT CONTACT DANS ESC
! IN  FFM    : FONCTIONS DE FORME DE LA PROJECTION DU PTC DANS MAIT
! IN  DDLES : NOMBRE DE DDLS D'UN NOEUD SOMMET ESCLAVE
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
!              INTERSECTEES
! IN  LAMBDA : VALEUR DU SEUIL_INIT
! IN  COEFFA : COEF_REGU_FROT
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! IN  TAU1   : PREMIERE TANGENTE
! IN  TAU2   : SECONDE TANGENTE
! IN  RESE   : PROJECTION DE LA BOULE UNITE POUR LE FROTTEMENT
! IN  MPROJ  : MATRICE DE L'OPERATEUR DE PROJECTION
! IN  TYPMAI : NOM DE LA MAILLE ESCLAVE D'ORIGINE (QUADRATIQUE)
! IN  NSINGE : NOMBRE DE FONCTION SINGULIERE ESCLAVE
! IN  NSINGM : NOMBRE DE FONCTION SINGULIERE MAITRE
! IN  RRE    : SQRT LST ESCLAVE
! IN  RRM    : SQRT LST MAITRE
! IN  NVIT   : POINT VITAL OU PAS
! I/O MMAT   : MATRICE ELEMENTAIRE DE CONTACT/FROTTEMENT
! ----------------------------------------------------------------------
    integer :: i, j, k, l, m, ii, jj, pli, plj
    integer :: jjn, iin, ddle
    integer :: nne, nnes, nnm, nnms, ddles, ddlem, ddlms, ddlmm
    real(kind=8) :: e(3, 3), a(3, 3), c(3, 3), mp, mb, mbt, mm, mmt
    real(kind=8) :: tt(3, 3), v(2)
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS
!
    nne=jnne(1)
    nnes=jnne(2)
    nnm=jnnm(1)
    nnms=jnnm(2)
    ddles=jddle(1)
    ddlem=jddle(2)
    ddlms=jddlm(1)
    ddlmm=jddlm(2)
    v(1)=0
    v(2)=0
!
    do 1 i = 1, 3
        do 2 j = 1, 3
            a(i,j) = 0.d0
            e(i,j) = 0.d0
            tt(i,j) = 0.d0
  2     continue
  1 continue
    v(1) = ddot(ndim,dvitet,1,tau1,1)
    if (ndim .eq. 3) v(2) = ddot(ndim,dvitet,1,tau2,2)
!     TT = ID
    do 301 i = 1, ndim
        tt(1,1) = tau1(i)*tau1(i) + tt(1,1)
        tt(1,2) = tau1(i)*tau2(i) + tt(1,2)
        tt(2,1) = tau2(i)*tau1(i) + tt(2,1)
        tt(2,2) = tau2(i)*tau2(i) + tt(2,2)
301 continue
!
! --- E = [P_TAU]T*[P_TAU]
!
! --- MPROJ MATRICE DE PROJECTION ORTHOGONALE SUR LE PLAN TANGENT
! --- E = [MPROJ]T*[MPROJ] = [MPROJ]*[MPROJ] = [MPROJ]
! ---        CAR ORTHOGONAL^   CAR PROJECTEUR^
    do 3 i = 1, ndim
        do 4 j = 1, ndim
            do 5 k = 1, ndim
                e(i,j) = mproj(k,i)*mproj(k,j) + e(i,j)
  5         continue
  4     continue
  3 continue
!
! --- A = [P_B,TAU1,TAU2]*[P_TAU]
!
! --- RESE = COEFFP*VIT
! ---        GT SEMI MULTIPLICATEUR AUGMENTE FROTTEMENT
    do 6 i = 1, ndim
        do 7 k = 1, ndim
            a(1,i) = rese(k)*mproj(k,i) + a(1,i)
            a(2,i) = tau1(k)*mproj(k,i) + a(2,i)
            a(3,i) = tau2(k)*mproj(k,i) + a(3,i)
  7     continue
  6 continue
!
! --- C = (P_B)[P_TAU]*(N)
!
! --- C = GT TENSORIEL N
    do 8 i = 1, ndim
        do 9 j = 1, ndim
            c(i,j) = a(1,i)*norm(j)
  9     continue
  8 continue
! ---- MP = MU*GN*WG*JAC
    if (nconta .eq. 3 .and. ndim .eq. 3) then
        mp = (lambda-coefcr*jeu)*coefff*hpg*jacobi
    else
        mp = lambda*coefff*hpg*jacobi
    endif
!
    ddle = ddles*nnes+ddlem*(nne-nnes)
    if (nnm .ne. 0) then
!
! --------------------- CALCUL DE [A] ET [B] -----------------------
!
        do 70 l = 1, ndim
            do 10 k = 1, ndim
! ROUTINE ADHERENTE, ON GARDE LA CONTRIBUTION A [A]
                if (l .eq. 1) then
                    mb = 0.d0
                    mbt = coefff*hpg*jacobi*a(l,k)
                else
                    if (.not.lpenaf) then
                        if (nconta .eq. 3 .and. ndim .eq. 3) then
                            mb = nvit*jacobi*hpg*a(l,k)
                        else
                            mb = nvit*mp*a(l,k)
                        endif
                    endif
                    if (lpenaf) mb = 0.d0
                    if (.not.lpenaf) mbt = mp*a(l,k)
                    if (lpenaf) mbt = 0.d0
                endif
                do 20 i = 1, nnc
                    call xplma2(ndim, nne, nnes, ddles, i,&
                                nfhe, pli)
                    ii = pli+l-1
                    do 30 j = 1, ndeple
                        mm = mb *ffc(i)*ffe(j)
                        mmt= mbt*ffc(i)*ffe(j)
                        call indent(j, ddles, ddlem, nnes, jjn)
                        jj = jjn+k
                        mmat(ii,jj) = -mm
                        mmat(jj,ii) = -mmt
                        jj = jj + ndim
                        mmat(ii,jj) = mm
                        mmat(jj,ii) = mmt
                        do 40 m = 1, nsinge
                            jj = jj + ndim
                            mmat(ii,jj) = rre * mm
                            mmat(jj,ii) = rre * mmt
 40                     continue
 30                 continue
                    do 50 j = 1, nnm
! --- BLOCS MA:CONT, CONT:MA
                        mm = mb *ffc(i)*ffm(j)
                        mmt= mbt*ffc(i)*ffm(j)
                        call indent(j, ddlms, ddlmm, nnms, jjn)
                        jj = ddle + jjn + k
                        mmat(ii,jj) = mm
                        mmat(jj,ii) = mmt
                        jj = jj + ndim
                        mmat(ii,jj) = mm
                        if (.not.lpenaf) mmat(jj,ii) = mmt
                        do 60 m = 1, nsingm
                            jj = jj + ndim
                            mmat(ii,jj) = rrm * mm
                            mmat(jj,ii) = rrm * mmt
 60                     continue
 50                 continue
 20             continue
 10         continue
 70     continue
!
! --------------------- CALCUL DE [BU] ---------------------------------
!
        do 100 k = 1, ndim
            do 110 l = 1, ndim
                if (lpenaf) then
                    mb = -mp*coeffp*e(l,k)
                    mbt = -mp*coeffp*e(l,k)
                else
                    if (nconta .eq. 3 .and. ndim .eq. 3) then
                        mb = -mp*coeffr*e(l,k)+coefcr*coefff*hpg* jacobi*c(l,k)
                        mbt = -mp*coeffr*e(l,k)+coefcr*coefff*hpg* jacobi*c(l,k)
                    else
                        mb = -mp*coeffr*e(l,k)
                        mbt = -mp*coeffr*e(l,k)
                    endif
                endif
                do 200 i = 1, ndeple
                    do 210 j = 1, ndeple
! --- BLOCS ES:ES
                        mm = mb *ffe(i)*ffe(j)
                        mmt= mbt*ffe(i)*ffe(j)
                        call indent(i, ddles, ddlem, nnes, iin)
                        call indent(j, ddles, ddlem, nnes, jjn)
                        ii = iin + l
                        jj = jjn + k
                        mmat(ii,jj) = mm
                        jj = jj + ndim
                        mmat(ii,jj) = -mm
                        mmat(jj,ii) = -mmt
                        ii = ii + ndim
                        mmat(ii,jj) = mm
                        do 215 m = 1, nsinge
                            jj = jj + ndim
                            ii = ii - ndim
                            mmat(ii,jj) = -rre * mm
                            mmat(jj,ii) = -rre * mmt
                            ii = ii + ndim
                            mmat(ii,jj) = rre * mm
                            mmat(jj,ii) = rre * mmt
                            ii = ii + ndim
                            mmat(ii,jj) = rre * rre * mm
215                     continue
210                 continue
                    do 220 j = 1, nnm
! --- BLOCS ES:MA, MA:ES
                        mm = mb *ffe(i)*ffm(j)
                        mmt= mbt*ffe(i)*ffm(j)
                        call indent(i, ddles, ddlem, nnes, iin)
                        call indent(j, ddlms, ddlmm, nnms, jjn)
                        ii = iin + l
                        jj = ddle + jjn + k
                        mmat(ii,jj) = -mm
                        mmat(jj,ii) = -mmt
                        jj = jj + ndim
                        mmat(ii,jj) = -mm
                        mmat(jj,ii) = -mmt
                        ii = ii + ndim
                        jj = jj - ndim
                        mmat(ii,jj) = mm
                        mmat(jj,ii) = mmt
                        jj = jj + ndim
                        mmat(ii,jj) = mm
                        mmat(jj,ii) = mmt
                        do 230 m = 1, nsingm
                            ii = ii - ndim
                            jj = jj + ndim
                            mmat(ii,jj) = -rrm * mm
                            mmat(jj,ii) = -rrm * mmt
                            ii = ii + ndim
                            mmat(ii,jj) = rrm * mm
                            mmat(jj,ii) = rrm * mmt
                            jj = jj - ndim
230                     continue
                        do 240 m = 1, nsinge
                            ii = ii + ndim
                            jj = jj - ndim
                            mmat(ii,jj) = rre * mm
                            mmat(jj,ii) = rre * mmt
                            jj = jj + ndim
                            mmat(ii,jj) = rre * mm
                            mmat(jj,ii) = rre * mmt
                            ii = ii - ndim
240                     continue
                        do 250 m = 1, nsinge*nsingm
                            ii = ii + ndim
                            jj = jj + ndim
                            mmat(ii,jj) = rre * rrm * mm
                            mmat(jj,ii) = rre * rrm * mmt
250                     continue
220                 continue
200             continue
                do 300 i = 1, nnm
                    do 320 j = 1, nnm
! --- BLOCS MA:MA
                        mm = mb *ffm(i)*ffm(j)
                        mmt= mbt*ffm(i)*ffm(j)
                        call indent(i, ddlms, ddlmm, nnms, iin)
                        call indent(j, ddlms, ddlmm, nnms, jjn)
                        ii = ddle + iin + l
                        jj = ddle + jjn + k
                        mmat(ii,jj) = mm
                        jj = jj + ndim
                        mmat(ii,jj) = mm
                        mmat(jj,ii) = mmt
                        ii = ii + ndim
                        mmat(ii,jj) = mm
                        do 330 m = 1, nsingm
                            jj = jj + ndim
                            ii = ii - ndim
                            mmat(ii,jj) = rrm * mm
                            mmat(jj,ii) = rrm * mmt
                            ii = ii + ndim
                            mmat(ii,jj) = rrm * mm
                            mmat(jj,ii) = rrm * mmt
                            ii = ii + ndim
                            mmat(ii,jj) = rrm * rrm * mm
330                     continue
320                 continue
300             continue
110         continue
100     continue
    else
!
! --------------------- CALCUL DE [A] ET [B] -----------------------
!
        do 550 l = 1, ndim
            do 510 k = 1, ndim
                if (l .eq. 1) then
                    mb = 0.d0
                    mbt = coefff*hpg*jacobi*a(l,k)
                else
                    if (.not.lpenaf) then
                        if (nconta .eq. 3 .and. ndim .eq. 3) then
                            mb = nvit*jacobi*hpg*a(l,k)
                        else
                            mb = nvit*mp*a(l,k)
                        endif
                    endif
                    if (lpenaf) mb = 0.d0
                    if (.not.lpenaf) mbt = mp*a(l,k)
                    if (lpenaf) mbt = 0.d0
                endif
                do 520 i = 1, nnc
                    call xplma2(ndim, nne, nnes, ddles, i,&
                                nfhe, pli)
                    ii = pli+l-1
                    do 530 j = 1, ndeple
! --- BLOCS ES:CONT, CONT:ES
                        mm = mb *ffc(i)*ffe(j)
                        mmt= mbt*ffc(i)*ffe(j)
                        call indent(j, ddles, ddlem, nnes, jjn)
                        jj = jjn + k
                        mmat(ii,jj) = rre * mm
                        if (.not.lpenaf) mmat(jj,ii) = rre * mmt
530                 continue
520             continue
510         continue
550     continue
!
! --------------------- CALCUL DE [BU] ---------------------------------
!
        do 600 k = 1, ndim
            do 610 l = 1, ndim
                if (lpenaf) then
                    mb = -mp*coeffp*e(l,k)
                else
                    if (nconta .eq. 3 .and. ndim .eq. 3) then
                        mb = -mp*coeffr*e(l,k)+coefcr*coefff*hpg* jacobi*c(l,k)
                    else
                        mb = -mp*coeffr*e(l,k)
                    endif
                endif
                do 620 i = 1, ndeple
                    do 630 j = 1, ndeple
! --- BLOCS ES:ES
                        mm = mb *ffe(i)*ffe(j)
                        call indent(i, ddles, ddlem, nnes, iin)
                        call indent(j, ddles, ddlem, nnes, jjn)
                        ii = iin + l
                        jj = jjn + k
                        mmat(ii,jj) = rre * rre * mm
630                 continue
620             continue
610         continue
600     continue
    endif
! --------------------- CALCUL DE [F] ----------------------------------
!
! ---------------SEULEMENT EN METHODE PENALISEE-------------------------
    if (lpenaf) then
        if (nvit .eq. 1) then
            do 400 i = 1, nnc
                do 410 j = 1, nnc
                    call xplma2(ndim, nne, nnes, ddles, i,&
                                nfhe, pli)
                    call xplma2(ndim, nne, nnes, ddles, j,&
                                nfhe, plj)
                    do 420 l = 1, ndim-1
                        do 430 k = 1, ndim-1
                            ii = pli+l
                            jj = plj+k
                            mmat(ii,jj) = ffc(i)*ffc(j)*hpg*jacobi*tt( l,k)
430                     continue
420                 continue
410             continue
400         continue
        endif
    endif
! ------------------- CALCUL DE [E] ------------------------------------
!
! ------------- COUPLAGE MULTIPLICATEURS CONTACT-FROTTEMENT ------------
    if (nvit .eq. 1) then
        do 800 i = 1, nnc
            do 810 j = 1, nnc
                call xplma2(ndim, nne, nnes, ddles, i,&
                            nfhe, pli)
                call xplma2(ndim, nne, nnes, ddles, j,&
                            nfhe, plj)
                do 830 k = 1, ndim-1
                    ii = pli+k
                    jj = plj
                    if (lpenaf) then
                        mmat(ii,jj) = 0.d0
                    else
                        if (nconta .eq. 3 .and. ndim .eq. 3) then
                            mmat(ii,jj) = 0.d0
                        else
                            mmat(ii,jj) = -ffc(i)*ffc(j)*coefff*hpg* jacobi*v(k)
                        endif
                    endif
830             continue
!
810         continue
800     continue
    endif
!
end subroutine
