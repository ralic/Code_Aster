subroutine xmelet(nomte, typmai, elrees, elrema, elreco,&
                  ndim, nddl, jnne, jnnm, nnc,&
                  jddle, jddlm, nconta, ndeple, nsinge,&
                  nsingm, nfhe, nfhm)
!
! ----------------------------------------------------------------------
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/elref2.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/iselli.h"
#include "asterfort/ismali.h"
#include "asterfort/teattr.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: nomte
    character(len=8) :: typmai, elrees, elrema, elreco
    integer :: ndim, nddl, nnc
    integer :: nsinge, nsingm, nconta
    integer :: jnne(3), jnnm(3), ndeple
    integer :: jddle(2), jddlm(2), nfhe, nfhm
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEMGG - CALCUL ELEM.)
!
! RETOURNE QUELQUES INFOS SUR LES ELEMENTS DE CONTACT FORMES ENTRE
! DEUX ELEMENTS  X-FEM
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
!
! IN  NOMTE  : NOM DU TE DE L'ELEMENT DE CONTACT EN JEU
! OUT TYPMAI : NOM DE LA MAILLE ESCLAVE D'ORIGINE
! OUT ELREES : ELREFE DE LA MAILLE ESCLAVE
! OUT ELREMA : ELREFE DE LA MAILLE MAITRE
! OUT ELRECO : ELREFE DE LA MAILLE DE CONTACT
! OUT NDIM   : DIMENSION DE LA MAILLE DE CONTACT
! OUT NDDL   : NOMBRE TOTAL DE DEGRES DE LIBERTE DE LA MAILLE DE CONTACT
! OUT JNNE   : MAILLE ESCL : (1) NB NDS
!                            (2) NB NDS SOMMETS
!                            (3) NB NDS MILIEU
! OUT JNNM   : MAILLE MAIT : (1) NB NDS
!                            (2) NB NDS SOMMETS
!                            (3) NB NDS MILIEU
! OUT NNC    : NOMBRE DE NOEUDS DE LA MAILLE DE CONTACT
! OUT JDDLE  : MAILLE ESCL : (1) DDLS D'UN NOEUD SOMMET
!                            (2) DDLS D'UN NOEUD MILIEU
! OUT JDDLM  : MAILLE MAIT : (1) DDLS D'UN NOEUD SOMMET
!                            (2) DDLS D'UN NOEUD MILIEU
! OUT NCONTA : TYPE DE CONTACT (1=P1P1, 2=P1P1A, 3=P2P1)
! OUT NDEPLE : NOMBRE DE NOEUDS ESCL POSSEDANT DES DDLS DE DEPLACEMENT
! OUT NSINGE : NOMBRE DE FONCTIONS SINGULIERE ESCLAVES
! OUT NSINGM : NOMBRE DE FONCTIONS SINGULIERE MAITRES
! OUT NFHE   : NOMBRE DE DDL HEAVISIDES ESCLAVE
! OUT NFHM   : NOMBRE DE DDL HEAVISIDE MAITRE
!
!
! ----------------------------------------------------------------------
!
!
    character(len=8) :: elrefp, enre, enrm, typma
    character(len=8) :: lielrf(10)
    integer :: ntrou, ilie, ndimd, nnod, nnosd, ier, i
    integer :: iadzi, iazk24
!
! ----------------------------------------------------------------------
!
    call elref1(elrefp)
    if (elrefp .eq. 'HE8') typmai = 'HEXA8'
    if (elrefp .eq. 'PE6') typmai = 'PENTA6'
    if (elrefp .eq. 'TE4') typmai = 'TETRA4'
    if (elrefp .eq. 'QU4') typmai = 'QUAD4'
    if (elrefp .eq. 'QU8') typmai = 'QUAD8'
    if (elrefp .eq. 'TR3') typmai = 'TRIA3'
    if (elrefp .eq. 'TR6') typmai = 'TRIA6'
    if (elrefp .eq. 'H20') typmai = 'HEXA20'
    if (elrefp .eq. 'P15') typmai = 'PENTA15'
    if (elrefp .eq. 'T10') typmai = 'TETRA10'
!
    call teattr('S', 'XFEM_E', enre, ier, typel=nomte)
    call teattr('S', 'XFEM_M', enrm, ier, typel=nomte)
!
    do 11 i = 1, 3
        jnne(i)=0
        jnnm(i)=0
11  end do
!
    do 12 i = 1, 2
        jddle(i)=0
        jddlm(i)=0
12  end do
!
!
! --- NOMBRE DE FONCTIONS SINGULIERES
!
    if (enre(1:1) .eq. 'H') then
        nsinge = 0
        nfhe = 1
        if (enre(2:2) .eq. '2') nfhe = 2
        if (enre(2:2) .eq. '3') nfhe = 3
        if (enre(2:2) .eq. '4') nfhe = 4
    else if (enre.eq.'C') then
        nsinge = 1
        nfhe = 1
    else if (enre.eq.'T') then
        nsinge = 1
        nfhe = 0
    else
        call utmess('F', 'DVP_4', sk=nomte)
    endif
!
    if (enrm(1:1) .eq. 'H') then
        nsingm = 0
        nfhm = 1
        if (enrm(2:2) .eq. '2') nfhm = 2
        if (enrm(2:2) .eq. '3') nfhm = 3
        if (enrm(2:2) .eq. '4') nfhm = 4
    else if (enrm.eq.'C') then
        nsingm = 1
        nfhm = 1
    else if (enrm.eq.'T') then
        nsingm = 0
        nfhm = 0
    else
        call utmess('F', 'DVP_4', sk=nomte)
    endif
!
    call elref2(nomte, 10, lielrf, ntrou)
!
    do 190 ilie = 1, ntrou
        call elrefe_info(elrefe=lielrf(ilie),fami='NOEU',ndim=ndimd,nno=nnod,nnos=nnosd)
        if (ilie .eq. 1) then
            ndim = ndimd
            jnne(1)= nnod
            jnne(2)= nnosd
            jnne(3)= nnod - nnosd
            elrees = lielrf(ilie)
        endif
!
        if (ilie .eq. 2 .and. ntrou .eq. 3) then
            jnnm(1) = nnod
            jnnm(2) = nnosd
            jnnm(3) = nnod - nnosd
            elrema = lielrf(ilie)
        endif
!
        if (ilie .eq. 2 .and. ntrou .eq. 2) then
            jnnm(1) = jnne(1)
            elrema = elrees
            nnc = nnod
            elreco = lielrf(ilie)
            jnnm(1)= jnne(1)
            jnnm(2)= jnne(2)
            jnnm(3)= jnne(3)
        endif
!
        if (ilie .eq. 3 .and. ntrou .eq. 3) then
            nnc = nnod
            elreco = lielrf(ilie)
        endif
190  end do
!
    call tecael(iadzi, iazk24)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)(1:8)
    if (typma(1:2) .eq. typma(4:5)) then
        elrees = elrema
    endif
!
    if (enre .eq. 'T') then
        jnnm(1) = 0
        jnnm(2) = 0
        elrema = '  '
    endif
!
! --- RECUPERATION DU TYPE DE CONTACT
!
    nconta=0
    if (ismali(typmai)) then
        nconta=1
    else
        if (iselli(elrema)) then
            nconta=2
        else
            nconta=3
        endif
    endif
!
! --- NOMBRE DE DDLS D'UN NOEUD SOMMET ESCLAVE
!
    if (enre .eq. 'T') then
        jddle(1) = 2*ndim
    else
        jddle(1) = ndim *(3+nsinge)
        jddle(1) = ndim *(1+2*nfhe+nsinge)
    endif
!
! --- NOMBRE DE DDLS D'UN NOEUD MILIEU ESCLAVE
!
    if (nconta .eq. 2) then
        jddle(2) = ndim
    else if (nconta.eq.3) then
        jddle(2) = ndim*2
    endif
!
! --- NOMBRE DE DDLS D'UN NOEUD MAITRE
!
    if (enre .eq. 'T') then
        jddlm(1) = 0
        jddlm(2) = 0
    else
        jddlm(1) = ndim *(1+nfhm+nsingm)
        if (.not.iselli(elrema)) jddlm(2) = jddlm(1)
    endif
!
! --- CALCUL DU NOMBRE TOTAL DE DDL
!
    if (enre .eq. 'T') then
        nddl = jddle(1)*jnne(1)
    else
!        IF (LMALIN) THEN
!          NDDL = NDIM * (NNE*(3+NSINGE) + NNM*(2+NSINGM))
!        ELSE
!          NDDL = NDIM * (NNE + 4*NNM)
!        ENDIF
        nddl=0
        do 13 i = 1, 2
            nddl = nddl + jnne(i+1)*jddle(i) + jnnm(i+1)*jddlm(i)
13      continue
    endif
!
    if (nconta .eq. 1 .or. nconta .eq. 3) then
        ndeple = jnne(1)
    else if (nconta.eq.2) then
        ndeple = jnne(2)
    else
        ASSERT(.false.)
    endif
!
end subroutine
