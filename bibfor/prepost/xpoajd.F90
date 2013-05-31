subroutine xpoajd(elrefp, ino, nnop, lsn, lst,&
                  ninter, iainc, typma, co, igeom,&
                  jdirno, nfiss, jfisno, he, ndime,&
                  ndim, cmp, nbcmp, nfh, nfe,&
                  ddlc, ima, jconx1, jconx2, jcnsv1,&
                  jcnsv2, jcnsl2, nbnoc, inntot, inn,&
                  nnn, contac, lmeca)
! aslint: disable=W1306,W1504
    implicit none
!
    include 'jeveux.h'
    include 'asterc/r8maem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/elelin.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/vecini.h'
    include 'asterfort/xdeffe.h'
    include 'asterfort/xlacti.h'
    include 'asterfort/xmoffc.h'
    include 'asterfort/xpoffo.h'
    integer :: ino, nnop, igeom, ndim, ndime, ddlc, jdirno
    integer :: nbcmp, cmp(nbcmp), nfe, ima, jconx1, jconx2, jcnsv1
    integer :: jcnsv2, jcnsl2, nbnoc, inntot, iainc, contac
    integer :: nfiss, jfisno, he(nfiss), nfh, inn, nnn, ninter
    logical :: lmeca
    character(len=8) :: elrefp, typma
    real(kind=8) :: co(3), lsn(nfiss), lst(nfiss)
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
! person_in_charge: samuel.geniaut at edf.fr
!
!     BUT:  CALCUL DES DEPLACEMENTS AUX SOMMENTS DES SOUS-ELEMENTS
!           ET REPORT DES LAGRANGES SI CONTACT
!
!   IN
!     ELREFP : ÉLÉMENT DE RÉFÉRENCE PARENT
!     INO   : NUMÉRO DU NOEUD OU DU POINT D'INTERSECTION
!     NNOP   : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
!     LSN    : LEVEL SETS NORMALES EN INO
!     LST    : LEVEL SET TANGENTE EN INO
!     NINTER : NOMBRE D'ARETES INTERSECTÉS DE L'ELEMENT PARENT
!     IAINC  : ADRESSE DE TOPOFAC.AI DE L'ELEMENT PARENT
!     TYPMA  : TYPE DE LA MAILLE PARENTE
!     CO     : COORDONNÉES INITIALES DE INO
!     IGEOM  : COORDONNÉES DES NOEUDS DE L'ÉLÉMENT PARENT
!     JDIRNO : ADRESSE DU TABLEAU DIRNO LOCAL
!     NFISS  : NOMBRE DE FISSURES "VUES" PAR L'ÉLÉMENT PARENT
!     JFISNO : POINTEUR DE FISSNO DANS L'ÉLÉMENT PARENT
!     HE     : VALEURS DE(S) FONCTION(S) HEAVISIDE SUR LE SOUS ÉLÉMENT
!     NDIME  : DIMENSION TOPOLOGIQUE DE LA MAILLE PARENT
!     NDIM   : DIMENSION DU MAILLAGE
!     CMP    : POSITION DES DDLS DE DEPL X-FEM DANS LE CHAMP_NO DE DEPL1
!     NBCMP  : NOMBRE DE COMPOSANTES DU CHAMP_NO DE DEPL1
!     NFH    : NOMBRE DE FONCTIONS HEAVISIDE (PAR NOEUD)
!     NFE    : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT (1 A 4)
!     DDLC   : NOMBRE DE DDL DE CONTACT DE L'ÉLÉMENT PARENT
!     IMA    : NUMERO DE MAILLE COURANTE PARENT
!     JCONX1 : ADRESSE DE LA CONNECTIVITE DU MAILLAGE SAIN
!              (CONNECTIVITE QUADRATIQUE SI LAGRANGES DE CONTACT
!              AUX ARETES)
!     JCONX2 : LONGUEUR CUMULEE DE LA CONNECTIVITE DU MAILLAGE SAIN
!              (CONNECTIVITE QUADRATIQUE SI LAGRANGES DE CONTACT
!              AUX ARETES)
!     JCNSV1 : ADRESSE DU CNSV DU CHAM_NO DE DEPLACEMENT 1
!     NBNOC  : NOMBRE DE NOEUDS CLASSIQUES DU MAILLAGE FISSURE
!     INN    : COMPTEUR LOCAL DU NOMBRE DE NOUVEAU NOEUDS CREES
!     NNN    : NOMBRE DE NOUVEAU NOEUDS A CREER SUR LA MAILLE PARENT
!     LMECA  : VRAI DANS LE CAS MECANIQUE (SINON CAS THERMIQUE)
!
!   OUT
!     JCNSV2 : ADRESSE DU CNSV DU CHAM_NO DE DEPLACEMENT 2
!     JCNSL2 : ADRESSE DU CNSL DU CHAM_NO DE DEPLACEMENT 2
!     INNTOT : COMPTEUR TOTAL DU NOMBRE DE NOUVEAU NOEUDS CREES
!      INN   : COMPTEUR LOCAL DU NOMBRE DE NOUVEAU NOEUDS CREES
!
!
!
!
    character(len=8) :: elrefc
    real(kind=8) :: ff(nnop), ffc(nnop), fe(4), crilsn, minlsn
    real(kind=8) :: r, theta, chpri(3), lagrs(3)
    integer :: i, j, iad, ipos, ig, ino2, ndimc, idecv2, idecl2
    integer :: nnol, ngl(8), ibid, ifiss, fiss
    integer :: lact(8), nlact
    logical :: lpint, lcont
    parameter    (crilsn = 1.d-4)
!
!     ------------------------------------------------------------------
    call jemarq()
!
! --- LPINT EST VRAI SI LE NOEUD DU MAILLAGE X-FEM EST SUR LA FISSURE
! --- SI LA MAILLE PARENTE POSSEDE DES DDLS DE CONTACT, ON CALCULERA
! --- ALORS LES LAGRANGES DE CONTACT FROTTEMENT POUR CE NOEUDS
! --- ATTENTION, IL SERA VRAI SEULEMENT SI ON EST DU COTÉ ESCAVE.
    if (ino .lt. 1000) then
        lpint = .false.
        do 10 ifiss = 1, nfiss
            if (lsn(ifiss) .eq. 0.d0) lpint = .true.
10      continue
    else if (ino.gt.1000.and.ino.lt.2000) then
        lpint = .true.
    else if (ino.gt.2000) then
        lpint = .false.
        do 20 ifiss = 1, nfiss
            if (abs(lsn(ifiss)) .lt. crilsn) lpint = .true.
20      continue
    endif
!
    if (lpint) then
        minlsn = r8maem()
        do 50 ifiss = 1, nfiss
!     ON DETECTE LA FISSURE CORESPONDANTE AU POINT D'INTERSECTION
            if (abs(lsn(ifiss)) .lt. minlsn) then
                minlsn = abs(lsn(ifiss))
                fiss = ifiss
            endif
50      continue
        if (he(fiss) .eq. 1) then
            lpint = .false.
        endif
    endif
    if (ddlc .gt. 0) call assert(lmeca)
    lcont = (ddlc.gt.0).and.(ndime.eq.ndim) .and.(ninter.gt.0).and.(nfiss.eq.1)
!
    inn = inn + 1
    inntot = inntot + 1
    call assert(inn.le.nnn)
!
    zi(jdirno-1+(2+nfiss)*(inn-1)+1) = ino
    zi(jdirno-1+(2+nfiss)*(inn-1)+2) = nbnoc + inntot
    do 30 ifiss = 1, nfiss
        zi(jdirno-1+(2+nfiss)*(inn-1)+2+ifiss) = he(ifiss)
30  end do
!
!     FF : FONCTIONS DE FORMES AU NOEUD SOMMET OU D'INTERSECTION
    call xpoffo(ndim, ndime, elrefp, nnop, igeom,&
                co, ff)
!
!     RQ : "NDIMC" CORRESPOND AU NOMBRE DE COMPOSANTE VECTORIELLE DU
!     CHAMP PRIMAL (DEPL EN MECA -> NDIM CMP / TEMP EN THERMIQUE
!     SOIT 1 CMP)
    if (lmeca) then
        ndimc = ndim
    else
        ndimc = 1
    endif
    call vecini(ndimc, 0.d0, chpri)
!
    if (nfe .ne. 0) then
!       FE : FONCTIONS D'ENRICHISSEMENT
        r = sqrt(lsn(1)**2+lst(1)**2)
        if (r .gt. r8prem()) then
!         LE POINT N'EST PAS SUR LE FOND DE FISSURE
            theta = he(1)*abs(atan2(lsn(1),lst(1)))
        else
!         LE POINT EST SUR LE FOND DE FISSURE :
!         L'ANGLE N'EST PAS DÉFINI, ON LE MET À ZÉRO
            theta=0.d0
        endif
!
        call xdeffe(r, theta, fe)
    endif
!
!     CALCUL DE L'APPROXIMATION DU CHAMP PRIMAL "CHPRI" (DEPLACEMENT
!     EN MECA / TEMPERATURE EN THERMIQUE)
    do 100 j = 1, nnop
!
!       ADRESSE DE LA 1ERE CMP DU CHAMP PRIMAL DU NOEUD INO
        iad=jcnsv1-1+nbcmp*(zi(jconx1-1+zi(jconx2+ima-1)+j-1)-1)
!
        ipos=0
!
!       DDLS CLASSIQUES
        do 110 i = 1, ndimc
            ipos=ipos+1
            chpri(i) = chpri(i) + ff(j) * zr(iad+cmp(ipos))
110      continue
!
!       DDLS HEAVISIDE
        do 120 ig = 1, nfh
            do 130 i = 1, ndimc
                ipos=ipos+1
                chpri(i) = chpri(i) + he(zi(jfisno-1+(j-1)*nfh+ig)) * ff(j) * zr(iad+cmp(ipos))
130          continue
120      continue
!
!       DDL ENRICHIS EN FOND DE FISSURE
        do 140 ig = 1, nfe
            do 150 i = 1, ndimc
                ipos=ipos+1
                chpri(i) = chpri(i) + fe(ig) * ff(j) * zr(iad+cmp( ipos))
150          continue
140      continue
!
100  end do
!
!     CALCUL DES LAGRANGES DE CONTACT FROTTEMENT
!     SEULEMENT POUR LES POINTS D'INTERSECTION
!
    call vecini(ndim, 0.d0, lagrs)
    if (lpint .and. lcont) then
!
!       NOEUD(S) GLOBAUX PORTANT LE(S) LAMBDA(S)
        call xlacti(typma, ninter, iainc, lact, nlact)
        call assert(nlact.gt.0)
        if (contac .eq. 1) then
            nnol = nnop
        else if (contac.eq.3) then
! --- FONCTIONS DE FORMES LINEAIRES POUR LE P2P1
            call elelin(contac, elrefp, elrefc, ibid, nnol)
            call xpoffo(ndim, ndime, elrefc, nnol, igeom,&
                        co, ff)
        endif
! --- FONCTIONS DE FORMES MODIFIÉES
        call xmoffc(lact, nlact, nnol, ff, ffc)
        do 200 j = 1, nnol
            ngl(j)=zi(jconx1-1+zi(jconx2+ima-1)+j-1)
200      continue
!
        do 310 i = 1, ddlc
! --- CALCUL AVEC LES FF DE CONTACT FFC, LINÉAIRES ET MODIFIÉES
            do 330 j = 1, nnol
                lagrs(i)=lagrs(i)+zr(jcnsv1-1+nbcmp*(ngl(j)-1)&
                +cmp((1+nfh+nfe)*ndim+i))*ffc(j)
330          continue
310      continue
    endif
!
999  continue
!
!       ECRITURE DANS LE .VALE2 POUR LE NOEUD INO2
    ino2 = nbnoc + inntot
    if (lmeca) then
!       POUR LA MECA
        idecv2 = jcnsv2-1+2*ndimc*(ino2-1)
        idecl2 = jcnsl2-1+2*ndimc*(ino2-1)
    else
!       POUR LA THERMIQUE
        idecv2 = jcnsv2-1+ndimc*(ino2-1)
        idecl2 = jcnsl2-1+ndimc*(ino2-1)
    endif
    do 400 i = 1, ndimc
        zr(idecv2+i)=chpri(i)
        zl(idecl2+i)=.true.
        if (lpint .and. lcont) then
!         POUR LES LAGRANGES DE CONTACT EN MECA
            zr(idecv2+ndimc+i)=lagrs(i)
            zl(idecl2+ndimc+i)=.true.
        endif
400  end do
!
    call jedema()
!
end subroutine
