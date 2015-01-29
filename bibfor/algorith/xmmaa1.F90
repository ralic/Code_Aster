subroutine xmmaa1(ndim, jnne, ndeple, nnc, jnnm,&
                  hpg, ffc, ffe,&
                  ffm, jacobi, coefcr, coefcp,&
                  lpenac, norm, nsinge, nsingm,&
                  rre, rrm, jddle, jddlm,&
                  nfhe, nfhm, lmulti, heavno, heavfa,&
                  mmat)
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "asterfort/indent.h"
#include "asterfort/xplma2.h"
    integer :: ndim, jnne(3), jnnm(3)
    integer :: nsinge, nsingm
    integer :: nfhe, nfhm, heavno(8), heavfa(*)
    real(kind=8) :: mmat(336, 336), norm(3)
    real(kind=8) :: hpg, ffc(8), ffe(20), ffm(20), jacobi
    real(kind=8) :: coefcr, coefcp, rre, rrm
    integer :: ndeple, nnc, jddle(2), jddlm(2)
    aster_logical :: lpenac, lmulti
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
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEMGG - CALCUL ELEM.)
!
! CALCUL DE A ET DE AT
!
!
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  JNNE   : MAILLE ESCL : (1) NB NDS
!                            (2) NB NDS SOMMETS
!                            (3) NB NDS MILIEU
! OUT NDEPLE : NOMBRE DE NOEUDS ESCL POSSEDANT DES DDLS DE DEPLACEMENT
! IN  NNC    : NOMBRE DE NOUEDS DE CONTACT
! IN  JNNM   : MAILLE MAIT : (1) NB NDS
!                            (2) NB NDS SOMMETS
!                            (3) NB NDS MILIEU
! IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFC    : FONCTIONS DE FORME DU POINT DE CONTACT DANS ELC
! IN  FFE    : FONCTIONS DE FORME DU POINT DE CONTACT DANS ESC
! IN  FFM    : FONCTIONS DE FORME DE LA PROJECTION DU PTC DANS MAIT
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  COEFCA : COEF_REGU_CONT
! IN  NORM   : VALEUR DE LA NORMALE AU POINT DE CONTACT
! IN  MAILLE : NOM DE LA MAILLE ESCLAVE D'ORIGINE (QUADRATIQUE)
! IN  NSINGE : NOMBRE DE FONCTION SINGULIERE ESCLAVE
! IN  NSINGM : NOMBRE DE FONCTION SINGULIERE MAITRE
! IN  RRE    : SQRT LST ESCLAVE
! IN  RRM    : SQRT LST MAITRE
! IN  NCONTA : TYPE DE CONTACT (1=P1P1, 2=P1P1A, 3=P2P1)
! IN  JDDLE  : MAILLE ESCL : (1) DDLS D'UN NOEUD SOMMET
!                            (2) DDLS D'UN NOEUD MILIEU
! IN  JDDLM  : MAILLE MAIT : (1) DDLS D'UN NOEUD SOMMET
!                            (2) DDLS D'UN NOEUD MILIEU
! I/O MMAT   : MATRICE ELEMENTAIRE DE CONTACT/FROTTEMENT
!
! ----------------------------------------------------------------------
!
    integer :: i, j, k, l, ii, jj, pl, jjn, iin, nddle
    integer :: nne, nnes, nnm, nnms, ddles, ddlem, ddlms, ddlmm
    integer :: pli, plj, ifh, iddl, jddl
    real(kind=8) :: mm, iescl(6), jescl(6), imait(6), jmait(6)
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATION
!
    iescl(1) = 1
    iescl(2) =-1
    iescl(2+nfhe)=-rre
    jescl(1) = 1
    jescl(2) =-1
    jescl(2+nfhe)=-rre
    imait(1) = 1
    imait(2) = 1
    imait(2+nfhm)= rrm
    jmait(1) = 1
    jmait(2) = 1
    jmait(2+nfhm)= rrm
!
! --------------------- CALCUL DE [A] ----------------------------------
!
    nne=jnne(1)
    nnes=jnne(2)
    nnm=jnnm(1)
    nnms=jnnm(2)
    ddles=jddle(1)
    ddlem=jddle(2)
    ddlms=jddlm(1)
    ddlmm=jddlm(2)
    nddle = ddles*nnes+ddlem*(nne-nnes)
!
    if (nnm .ne. 0) then
        do 10 k = 1, ndim
            do 20 i = 1, nnc
                call xplma2(ndim, nne, nnes, ddles, i,&
                            nfhe, pl)
                if (lmulti) pl = pl + (heavno(i)-1)*ndim
                do 30 j = 1, ndeple
                    mm = hpg*ffc(i)*ffe(j)*jacobi*norm(k)
                    call indent(j, ddles, ddlem, nnes, jjn)
                    if (lmulti) then
                        do 35 ifh = 1, nfhe
                            jescl(1+ifh)=heavfa(nfhe*(j-1)+ifh)
 35                     continue
                    endif
                    do 40 jddl = 1, 1+nfhe+nsinge
                        jj = jjn + (jddl-1)*ndim + k
                        mmat(pl,jj) = -jescl(jddl)*mm
                        mmat(jj,pl) = -jescl(jddl)*mm
 40                 continue
 30             continue
                do 50 j = 1, nnm
                    mm = hpg*ffc(i)*ffm(j)*jacobi*norm(k)
                    call indent(j, ddlms, ddlmm, nnms, jjn)
                    jjn = jjn + nddle
                    if (lmulti) then
                        do 55 ifh = 1, nfhm
                            jmait(1+ifh)=heavfa(nfhe*ndeple+nfhm*(j-1)&
                            +ifh)
 55                     continue
                    endif
                    do 60 jddl = 1, 1+nfhm+nsingm
                        jj = jjn + (jddl-1)*ndim + k
                        mmat(pl,jj) = jmait(jddl)*mm
                        mmat(jj,pl) = jmait(jddl)*mm
 60                 continue
 50             continue
 20         continue
 10     continue
!
! --------------------- CALCUL DE [AU]----------------------------------
!
        do 100 k = 1, ndim
            do 110 l = 1, ndim
                do 200 i = 1, ndeple
                    call indent(i, ddles, ddlem, nnes, iin)
                    do 210 j = 1, ndeple
                        call indent(j, ddles, ddlem, nnes, jjn)
                        if (lpenac) then
                            mm = 0.d0
                        else
                            mm = hpg*coefcr*ffe(i)*norm(l)*ffe(j)* jacobi*norm(k)
                        endif
                        if (lmulti) then
                            do 220 ifh = 1, nfhe
                                iescl(1+ifh)=heavfa(nfhe*(i-1)+ifh)
                                jescl(1+ifh)=heavfa(nfhe*(j-1)+ifh)
220                         continue
                        endif
                        do 230 iddl = 1, 1+nfhe+nsinge
                            ii = iin + (iddl-1)*ndim + l
                            do 240 jddl = 1, 1+nfhe+nsinge
                                jj = jjn + (jddl-1)*ndim + k
                                mmat(ii,jj) = iescl(iddl)*jescl(jddl)* mm
240                         continue
230                     continue
210                 continue
                    do 250 j = 1, nnm
                        call indent(j, ddlms, ddlmm, nnms, jjn)
                        jjn = jjn + nddle
                        if (lpenac) then
                            mm = 0.d0
                        else
                            mm = hpg*coefcr*ffe(i)*norm(l)*ffm(j)* jacobi*norm(k)
                        endif
                        if (lmulti) then
                            do 260 ifh = 1, nfhe
                                iescl(1+ifh)=heavfa(nfhe*(i-1)+ifh)
260                         continue
                            do 270 ifh = 1, nfhm
                                jmait(1+ifh)=heavfa(nfhe*ndeple+nfhm*(&
                                j-1)+ifh)
270                         continue
                        endif
                        do 280 iddl = 1, 1+nfhe+nsinge
                            ii = iin + (iddl-1)*ndim + l
                            do 290 jddl = 1, 1+nfhm+nsingm
                                jj = jjn + (jddl-1)*ndim + k
                                mmat(ii,jj) = -iescl(iddl)*jmait(jddl) *mm
                                mmat(jj,ii) = -iescl(iddl)*jmait(jddl) *mm
290                         continue
280                     continue
250                 continue
200             continue
                do 300 i = 1, nnm
                    call indent(i, ddlms, ddlmm, nnms, iin)
                    iin = iin + nddle
                    do 320 j = 1, nnm
                        call indent(j, ddlms, ddlmm, nnms, jjn)
                        jjn = jjn + nddle
                        if (lpenac) then
                            mm = 0.d0
                        else
                            mm = hpg*coefcr*ffm(i)*norm(l)*ffm(j)* jacobi*norm(k)
                        endif
                        if (lmulti) then
                            do 330 ifh = 1, nfhm
                                imait(1+ifh)=heavfa(nfhe*ndeple+nfhm*(&
                                i-1)+ifh)
                                jmait(1+ifh)=heavfa(nfhe*ndeple+nfhm*(&
                                j-1)+ifh)
330                         continue
                        endif
                        do 340 iddl = 1, 1+nfhm+nsingm
                            ii = iin + (iddl-1)*ndim + l
                            do 350 jddl = 1, 1+nfhm+nsingm
                                jj = jjn + (jddl-1)*ndim + k
                                mmat(ii,jj) = imait(iddl)*jmait(jddl)* mm
350                         continue
340                     continue
320                 continue
300             continue
!
110         continue
100     continue
    else
!
! --------------------- CALCUL DE [A] ----------------------------------
!
        do 510 k = 1, ndim
            do 520 i = 1, nnc
                call xplma2(ndim, nne, nnes, ddles, i,&
                            nfhe, pl)
                if (lmulti) pl = pl + (heavno(i)-1)*ndim
                do 530 j = 1, ndeple
! --- BLOCS ES:CONT, CONT:ES
                    call indent(j, ddles, ddlem, nnes, jjn)
                    jj = jjn + k
                    mm = hpg*ffc(i)*ffe(j)*jacobi*norm(k)
                    mmat(pl,jj) = rre * mm
                    mmat(jj,pl) = rre * mm
530             continue
520         continue
510     continue
!
! --------------------- CALCUL DE [AU]----------------------------------
!
        do 600 k = 1, ndim
            do 610 l = 1, ndim
                do 620 i = 1, ndeple
                    do 630 j = 1, ndeple
! --- BLOCS ES:ES
                        if (lpenac) then
                            mm = 0.d0
                        else
                            mm = hpg*coefcr*ffe(i)*norm(l)*ffe(j)* jacobi*norm(k)
                        endif
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
! --------------------- CALCUL DE [C] ----------------------------------
!
!-------------- SEULEUMENT EN METHODE PENALISEE ------------------------
!
    if (lpenac) then
        do 710 i = 1, nnc
            do 720 j = 1, nnc
                call xplma2(ndim, nne, nnes, ddles, i,&
                            nfhe, pli)
                call xplma2(ndim, nne, nnes, ddles, j,&
                            nfhe, plj)
                mmat(pli,plj) = -hpg*ffc(j)*ffc(i)*jacobi/coefcp
720         continue
710     continue
    endif
!
end subroutine
