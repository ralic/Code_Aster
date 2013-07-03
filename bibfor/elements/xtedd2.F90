subroutine xtedd2(ndim, jnne, ndeple, jnnm, nddl,&
                  option, lesclx, lmaitx, lcontx, stano,&
                  lact, jddle, jddlm, nfhe, nfhm,&
                  lmulti, heavno, mmat, vtmp)
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
! person_in_charge: patrick.massin at edf.fr
!
!     BUT: SUPPRIMER LES DDLS "EN TROP" (VOIR BOOK III 09/06/04
!     ET  BOOK IV  30/07/07) POUR LE CONTACT XFEM GRAND GLISSEMENT
!
!     TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
!
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/indent.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: ndim, jnnm(3), nddl, stano(*), lact(8)
    character(len=16) :: option
    real(kind=8) :: mmat(336, 336), vtmp(336)
    logical :: lesclx, lmaitx, lcontx, lmulti
    integer :: jnne(3), ndeple, jddle(2)
    integer :: jddlm(2), nfhe, nfhm, heavno(8)
!
! IN   NDIM   : DIMENSION DE L'ESPACE
! IN   NNES   : NOMBRE DE NOEUDS SOMMETS DE LA MAILLE ESCLAVE
! IN   NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN   NDDL   : NOMBRE DE DDL TOTAL DE L'ÉLÉMENT
! IN   NDLS   : NOMBRE DE DDLS D'UN NOEUD SOMMET ESCLAVE
! IN   OPTION : OPTION DE CALCUL DU TE
! IN   STANO  : LITE DU STATUT DES NOEUDS
! IN   LACT   : LITE DES LAGRANGES ACTIFS
! IN   LESCLX : LA MAILLE ESCLAVE EST HEAVISIDE CRACK-TIP
! IN   LMAITX : LA MAILLE MAITRE EST HEAVISIDE CRACK-TIP
! IN   LCONTX : CERTAINS DDL DE CONTACT NE SONT PAS ACTIFS
! IN/OUT  MAT : MATRICE DE RIGIDITÉ
! IN/OUT VECT : VECTEUR SECOND MEMBRE
!
!
!
!
    integer :: i, j, nddle, in, nnes, nnem, ddles, ddlem, nnm, ddlmax
    parameter     (ddlmax=336)
    integer :: ddlms, ddlmm, ifh, posddl(ddlmax)
    real(kind=8) :: dmax
!
!----------------------------------------------------------------------
!
    call jemarq()
!
    nnes=jnne(2)
    nnem=jnne(3)
    nnm=jnnm(1)
    ddles=jddle(1)
    ddlem=jddle(2)
    ddlms=jddlm(1)
    ddlmm=jddlm(2)
    nddle = ddles*nnes+ddlem*nnem
!
! --- REMPLISSAGE DU VECTEUR POS : POSITION DES DDLS A SUPPRIMER
!
    call assert(nddl.le.ddlmax)
    do 9 i = 1, ddlmax
        posddl(i)=0
 9  end do
!
    if (lesclx) then
        do 10 i = 1, ndeple
            call indent(i, ddles, ddlem, nnes, in)
!
            if (stano(i) .eq. 1) then
! --- NOEUD HEAVISIDE, ON ELIMINE LES DDL CRACK-TIP
                do 20 j = 1, ndim
                    posddl(in+2*ndim+j)=1
20              continue
            else if (stano(i).eq.2) then
! --- NOEUD CRACK-TIP, ON ELIMINE LES DDL HEAVISIDE
                do 30 j = 1, ndim
                    posddl(in+ndim+j)=1
30              continue
            endif
10      continue
    else
        do 130 i = 1, ndeple
            call indent(i, ddles, ddlem, nnes, in)
            do 135 ifh = 1, nfhe
                if (stano(nfhe*(i-1)+ifh) .eq. 0) then
! --- DANS LE CAS DE MAILLE HEAVISIDE, ON ELIMINE LE DDL HEAVISIDE
                    do 140 j = 1, ndim
                        posddl(in+ndim*ifh+j)=1
140                  continue
                endif
135          continue
130      continue
    endif
    if (lmaitx) then
        do 40 i = 1, nnm
            call indent(i, ddlms, ddlmm, nnm, in)
            in = in + nddle
            if (stano(ndeple+i) .eq. 1) then
! --- NOEUD HEAVISIDE, ON ELIMINE LES DDL CRACK-TIP
                do 50 j = 1, ndim
                    posddl(in+2*ndim+j)=1
50              continue
            else if (stano(ndeple+i).eq.2) then
! --- LE NOEUD EST CRACK-TIP, ON ELIMINE HEAVISIDE
                do 60 j = 1, ndim
                    posddl(in+ndim+j)=1
60              continue
            endif
40      continue
    else
        do 150 i = 1, nnm
            call indent(i, ddlms, ddlmm, nnm, in)
            in = in + nddle
            do 155 ifh = 1, nfhm
                if (stano(nfhe*ndeple+nfhm*(i-1)+ifh) .eq. 0) then
! --- DANS LE CAS DE MAILLE HEAVISIDE, ON ELIMINE LE DDL HEAVISIDE
                    do 160 j = 1, ndim
                        posddl(in+ndim*ifh+j)=1
160                  continue
                endif
155          continue
150      continue
    endif
    if (lcontx) then
        do 110 i = 1, nnes
            if (lact(i) .eq. 0) then
! --- CONTACT NON ACTIF POUR CE NOEUD, ON ELIMINE LES DDL DE CONTACT
                do 120 j = 1, ndim
                    if (.not.lmulti) posddl(ddles*i-ndim+j)=1
                    if (lmulti) posddl(ddles*(i-1)+ndim*(nfhe+heavno(i)) +j)=1
120              continue
            endif
110      continue
    endif
!
    if (option .eq. 'RIGI_CONT' .or. option .eq. 'RIGI_FROT') then
! --- CALCUL DU COEFFICIENT DIAGONAL POUR
! --- L'ELIMINATION DES DDLS HEAVISIDE
! --- MISE A ZERO DES TERMES HORS DIAGONAUX (I,J)
! --- ET MISE A UN DES TERMES DIAGONAUX (I,I)
!        DMIN=R8MAEM()
        dmax=1.d0
!          DO 70 I = 1, NDDL
!            DMAX = DMAX + ABS(MMAT(I,I))
!  70      CONTINUE
        do 80 i = 1, nddl
            if (posddl(i) .eq. 0) goto 80
            do 90 j = 1, nddl
                if (j .ne. i) then
                    mmat(i,j) = 0.d0
                    mmat(j,i) = 0.d0
                endif
                if (j .eq. i) mmat(i,j) = dmax
90          continue
80      continue
!
        else if (option.eq.'CHAR_MECA_CONT' .or.&
     &         option.eq.'CHAR_MECA_FROT') then
! --- MISE A ZERO DES TERMES I
        do 100 i = 1, nddl
            if (posddl(i) .eq. 0) goto 100
            vtmp(i) = 0.d0
100      continue
    endif
!
    call jedema()
end subroutine
