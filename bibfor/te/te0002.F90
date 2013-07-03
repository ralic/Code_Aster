subroutine te0002(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
!     CALCUL DES TERMES ELEMENTAIRES DE LAGRANGE
!     ELEMENTS  D_DEPL_R_.... / D_TEMP_R_.... / D_PRES_C_....
!
!     EN ENTREE :
!        OPTION : NOM DE L'OPTION A CALCULER
!        NOMTE  : NOM DU TYPE_ELEMENT
!
!
!
!
!
!
#include "jeveux.h"
!
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
    real(kind=8) :: k(10, 10), valpar(4), result
    integer :: i, ier, j, ndl, ip, jalpha, jmat, jdimp, jdmul, jgeom, jlagr
    integer :: jvec, jtime, nbpar
    complex(kind=8) :: kc(10, 10)
!
    character(len=8) :: nompar(4)
    character(len=19) :: nomfon
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!
    ndl = 1
!
    if (option(6:11) .eq. 'DDLM_R') then
        call jevech('PDDLMUR', 'L', jdmul)
        do 110 i = 1, ndl + 2
            do 100 j = 1, ndl + 2
                k(i,j) = 0.d0
100          continue
110      continue
!
        k(ndl+1,ndl+1) = -1.0d0
        k(ndl+2,ndl+2) = -1.0d0
        k(ndl+1,ndl+2) = 1.0d0
!
        do 120 i = 1, ndl
            k(i,ndl+1) = zr(jdmul-1+i)
            k(i,ndl+2) = zr(jdmul-1+i)
120      continue
        if (option(1:4) .eq. 'MECA') call jevech('PMATUUR', 'E', jmat)
        if (option(1:4) .eq. 'THER') call jevech('PMATTTR', 'E', jmat)
        ip = 0
        do 140 i = 1, ndl + 2
            do 130 j = 1, i
                zr(jmat-1+ip+j) = k(j,i)
130          continue
            ip = ip + i
140      continue
!
    else if (option(6:11) .eq. 'DDLM_C') then
        call jevech('PDDLMUC', 'L', jdmul)
        do 115 i = 1, ndl + 2
            do 105 j = 1, ndl + 2
                kc(i,j) = (0.d0,0.d0)
105          continue
115      continue
        kc(ndl+1,ndl+1) = (-1.0d0, 0.0d0)
        kc(ndl+2,ndl+2) = (-1.0d0, 0.0d0)
        kc(ndl+1,ndl+2) = ( 1.0d0, 0.0d0)
        do 125 i = 1, ndl
            kc(i,ndl+1) = zc(jdmul-1+i)
            kc(i,ndl+2) = zc(jdmul-1+i)
125      continue
        if (option(1:4) .eq. 'ACOU') call jevech('PMATTTC', 'E', jmat)
        ip = 0
        do 145 i = 1, ndl + 2
            do 135 j = 1, i
                zc(jmat-1+ip+j) = kc(j,i)
135          continue
            ip = ip + i
145      continue
!
    else if (option(6:11) .eq. 'BTLA_R') then
        call jevech('PDDLMUR', 'L', jdmul)
        call jevech('PLAGRAR', 'L', jlagr)
        if (option(1:4) .eq. 'MECA') call jevech('PVECTUR', 'E', jmat)
        if (option(1:4) .eq. 'THER') call jevech('PVECTTR', 'E', jmat)
        zr(jmat )=zr(jdmul)*(zr(jlagr+1)+zr(jlagr+2))
        zr(jmat+1)=0.d0
        zr(jmat+2)=0.d0
!
    else if (option(6:9) .eq. 'BU_R') then
        call jevech('PDDLMUR', 'L', jdmul)
        call jevech('PDDLIMR', 'L', jdimp)
        call jevech('PALPHAR', 'L', jalpha)
        if (option(1:4) .eq. 'MECA') call jevech('PVECTUR', 'E', jmat)
        if (option(1:4) .eq. 'THER') call jevech('PVECTTR', 'E', jmat)
        zr(jmat )=0.d0
        zr(jmat+1)=zr(jdmul)*zr(jdimp) -zr(jalpha)*(zr(jdimp+1)-zr(&
        jdimp+2))
        zr(jmat+2)=zr(jdmul)*zr(jdimp) +zr(jalpha)*(zr(jdimp+1)-zr(&
        jdimp+2))
!
    else if (option(6:11) .eq. 'DDLI_R') then
        call jevech('PDDLIMR', 'L', jdimp)
        if (option(1:4) .eq. 'MECA') call jevech('PVECTUR', 'E', jvec)
        if (option(1:4) .eq. 'THER') call jevech('PVECTTR', 'E', jvec)
        zr(jvec-1+ndl+1) = zr(jdimp-1+1)
        zr(jvec-1+ndl+2) = zr(jdimp-1+1)
!
    else if (option(6:11) .eq. 'DDLI_C') then
        call jevech('PDDLIMC', 'L', jdimp)
        if (option(1:4) .eq. 'MECA') call jevech('PVECTUC', 'E', jvec)
        if (option(1:4) .eq. 'THER') call jevech('PVECTTC', 'E', jvec)
        if (option(1:4) .eq. 'ACOU') call jevech('PVECTTC', 'E', jvec)
        zc(jvec-1+ndl+1) = zc(jdimp-1+1)
        zc(jvec-1+ndl+2) = zc(jdimp-1+1)
!
    else if (option(6:11) .eq. 'DDLI_F') then
        call jevech('PDDLIMF', 'L', jdimp)
        call jevech('PGEOMER', 'L', jgeom)
        call jevech('PTEMPSR', 'L', jtime)
        if (option(1:4) .eq. 'MECA') call jevech('PVECTUR', 'E', jvec)
        if (option(1:4) .eq. 'THER') call jevech('PVECTTR', 'E', jvec)
        if (option(1:4) .eq. 'ACOU') call jevech('PVECTTC', 'E', jvec)
        nomfon = zk24(jdimp-1+1)
        nbpar = 4
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        nompar(4) = 'INST'
        valpar(1) = zr(jgeom-1+1)
        valpar(2) = zr(jgeom-1+2)
        valpar(3) = zr(jgeom-1+3)
        valpar(4) = zr(jtime-1+1)
        call fointe('FM', nomfon, nbpar, nompar, valpar,&
                    result, ier)
        if (option(1:4) .eq. 'ACOU') then
            zc(jvec-1+ndl+1) = result
            zc(jvec-1+ndl+2) = result
        else
            zr(jvec-1+ndl+1) = result
            zr(jvec-1+ndl+2) = result
        endif
    endif
! ----------------------------------------------------------------------
end subroutine
