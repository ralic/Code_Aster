subroutine xpoajm(maxfem, jtypm2, itypse, jcnse, im,&
                  n, nnose, prefno, jdirno, nnm,&
                  inm, inmtot, nbmac, he, jnivgr,&
                  iagma, ngrm, jdirgr, opmail, nfiss,&
                  ndim, ndime, jconx1, jconx2, jconq1,&
                  jconq2, ima, iad1, nnn, inn,&
                  inntot, nbnoc, nbnofi, inofi, iacoo1,&
                  iacoo2, iad9, ninter, iainc, elrefp,&
                  jlsn, jlst, typma, igeom, jfisno,&
                  contac, cmp, nbcmp, nfh, nfe,&
                  ddlc, jcnsv1, jcnsv2, jcnsl2, lmeca)
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
! aslint: disable=W1306,W1504
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
#include "asterfort/xpoajd.h"
#include "asterfort/xpoajn.h"
#include "asterfort/xpolsn.h"
    integer :: nfiss, nnn, inn, inntot, ndim, jconx1, jconx2
    integer :: jconq1, jconq2, iacoo1, iacoo2, jcnsl2
    integer :: nbnoc, nbnofi, inofi
    integer :: ima, iad1, jlsn, jlst, igeom, ndime, iad9
    integer :: jfisno, cmp(*), nbcmp, nfh, nfe, ddlc, jcnsv1, jcnsv2
    integer :: ninter, iainc, contac
    character(len=2) :: prefno(4)
    character(len=8) :: maxfem, elrefp, typma
    integer :: jtypm2, itypse, nnm, inm, inmtot, nbmac, jdirgr
    integer :: jcnse, im, n, nnose, jdirno, he(nfiss), jnivgr, iagma, ngrm
    logical :: opmail, lmeca
!
!
!   ON AJOUTE UNE NOUVELLE MAILLE AU NOUVEAU MAILLAGE X-FEM
!
!   IN
!
!     ITYPSE : NUMEROS DU TYPE DE SOUS-ELEMENT
!     JCNSE  : ADRESSE DE LA CONNECTIVITÉ LOCALE DES SOUS-ELEMENTS
!     IM     : POSITION LOCALE DU SOUS-ELEMENT
!     N      : NOMBRE DE NOEUDS DE LA MAILLE PARENT
!     NNOSE  : NOMBRE DE NOEUDS DU SOUS ÉLÉMENT
!     PREFNO : PREFERENCES POUR LE NOMAGE DES NOUVELLES ENTITES
!     JDIRNO : ADRESSE DU TABLEAU DIRNO LOCAL
!     NNM    : NOMBRE DE NOUVELLES MAILLES A CREER SUR LA MAILLE PARENT
!     NBMAC  : NOMBRE DE MAILLES CLASSIQUES DU MAILLAGE FISSURE
!     HE     : VALEURS DE(S) FONCTION(S) HEAVISIDE SUR LE SOUS ÉLÉMENT
!     JNIVGR : ADRESSE DU VECTEUR DE REMPLISSAGE DES GROUP_MA DE MAXFEM
!     IAGMA  : ADRESSE DES NUMEROS DANS MA1 DES GROUP_MA CONTENANT IMA
!     NGRM   : NOMBRE DE GROUP_MA CONTENANT IMA
!     JDIRGR : ADRESSE DU TABLEAU D'INDIRECTION GLOBAL DES GROUP_MA
!     OPMAIL : .TRUE. SI POST_MAIL_XFEM
!     NFISS  : NOMBRE DE FISSURES "VUES" PAR LA MAILLE
!     NDIM   : DIMENSION DU MAILLAGE
!     NDIME  : DIMENSION TOPOLOGIQUE DE LA MAILLE PARENT
!     JCONX1 : ADRESSE DE LA CONNECTIVITE DU MAILLAGE SAIN
!     JCONX2 : LONGUEUR CUMULEE DE LA CONNECTIVITE DU MAILLAGE SAIN
!     JCONQ1 : ADRESSE DE LA CONNECTIVITE DU MAILLAGE DU MODÈLE
!     JCONQ2 : LONGUEUR CUMULEE DE LA CONNECTIVITE DU MAILLAGE DU MODÈLE
!     IMA    : NUMERO DE MAILLE COURANTE PARENT
!     IAD1   : POINTEUR DES COORDONÉES DES POINTS D'INTERSECTION
!     NNN    : NOMBRE DE NOUVEAU NOEUDS A CREER SUR L'ÉLÉMENT PARENT
!     NBNOC  : NOMBRE DE NOEUDS CLASSIQUES DU MAILLAGE FISSURE
!     IACOO1 : ADRESSE DES COORDONNES DES NOEUDS DU MAILLAGE SAIN
!     IACOO2 :  ADRESSE DES COORDONNES DES NOEUDS DU MAILLAGE FISSURE
!     IAD9   : POINTEUR DES COOR DES POINTS MILIEUX DES SE QUADRATIQUES
!     NINTER : NOMBRE D'ARETES INTERSECTÉS DE L'ELEMENT PARENT
!     IAINC  : ADRESSE DE TOPOFAC.AI DE L'ELEMENT PARENT
!     ELREFP : ÉLÉMENT DE RÉFÉRENCE PARENT
!     JLSN   : ADRESSE DU CHAM_NO_S DE LA LEVEL NORMALE
!     JLST   : ADRESSE DU CHAM_NO_S DE LA LEVEL TANGENTE
!     TYPMA  : TYPE DE LA MAILLE PARENTE
!     IGEOM  : COORDONNÉES DES NOEUDS DE L'ÉLÉMENT PARENT
!     JFISNO : POINTEUR DE FISSNO DANS L'ÉLÉMENT PARENT
!     CMP    : POSITION DES DDLS DE DEPL X-FEM DANS LE CHAMP_NO DE DEPL1
!     NBCMP  : NOMBRE DE COMPOSANTES DU CHAMP_NO DE DEPL1
!     NFH    : NOMBRE DE FONCTIONS HEAVISIDE (PAR NOEUD)
!     NFE    : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT (1 A 4)
!     DDLC   : NOMBRE DE DDL DE CONTACT DE L'ÉLÉMENT PARENT
!     JCNSV1 : ADRESSE DU CNSV DU CHAM_NO DE DEPLACEMENT 1
!     LMECA  : VRAI DANS LE CAS MECANIQUE (SINON CAS THERMIQUE)
!
!   OUT
!     MAXFEM : NOM DU MAILLAGE FISSURE
!     JTYPM2 : ADRESSE DE L'OBJET .TYPMAIL DU MAILLAGE FISSURE
!     INM    : COMPTEUR LOCAL DU NOMBRE DE NOUVELLES MAILLES CREEES
!     INMTOT : COMPTEUR TOTAL DU NOMBRE DE NOUVELLES MAILLES CREEES
!     INN    : COMPTEUR LOCAL DU NOMBRE DE NOUVEAUX NOEUDS CREEES
!     INNTOT : COMPTEUR TOTAL DU NOMBRE DE NOUVEAUX NOEUDS CREEES
!     NBNOFI : NOMBRE DE NOEUDS SITUES SUR LA FISSURE
!     INOFI  : LISTE DES NOEUDS SITUES SUR LA FISSURE
!     NBNOLA : NOMBRE DE NOEUDS SITUES SUR LA FISSURE AVEC DES LAGS
!     INOLA  : LISTE DES NOEUDS SITUES SUR LA FISSURE AVEC DES LAGS
!     JCNSV2 : ADRESSE DU CNSV DU CHAM_NO DE DEPLACEMENT 2
!     JCNSL2 : ADRESSE DU CNSL DU CHAM_NO DE DEPLACEMENT 2
!
!
    integer :: iacon2, j, ino, ig1, ig2, iagma2, i
    integer :: ifiss, iad
    real(kind=8) :: lsn(nfiss), lst(nfiss), co(3)
    character(len=6) :: chn
    character(len=8) :: valk(2)
    character(len=19) :: ma2con
    logical :: lnoeud
    data          valk /'MAILLES','XPOAJM'/
!
!     ------------------------------------------------------------------
!
    call jemarq()
!
    if (inmtot .ge. 999999) call u2mesk('F', 'XFEM_8', 1, valk)
!
    if (opmail) then
        inm = inm + 1
        inmtot = inmtot + 1
        ASSERT(inm.le.nnm)
        call codent(inmtot, 'G', chn)
        call jecroc(jexnom(maxfem//'.NOMMAI', prefno(4)//chn))
        zi(jtypm2-1+nbmac + inmtot) = itypse
        ma2con = maxfem//'.CONNEX'
        call jeecra(jexnum(ma2con, nbmac+inmtot), 'LONMAX', nnose)
        call jeveuo(jexnum(ma2con, nbmac+inmtot), 'E', iacon2)
!
!       ON INCREMENTE LES GROUP_MA
!       BOUCLE SUR LES GROUP_MA CONTENANT LA MAILLE IMA
        do 110 i = 1, ngrm
!         NUMEROS DU GROUP_MA DANS MA1 ET MA2
            ig1 = zi(iagma-1+i)
            ig2 = zi(jdirgr-1+ig1)
!         SI CE GROUP_MA N'EXISTE PAS DANS MA2, ON PASSE AU SUIVANT
            if (ig2 .eq. 0) goto 110
            call jeveuo(jexnum(maxfem//'.GROUPEMA', ig2), 'E', iagma2)
!         NIVEAU DE REMPLISSAGE DU GROUP_MA
            zi(jnivgr-1+ig2) = zi(jnivgr-1+ig2) + 1
            zi(iagma2-1+ zi(jnivgr-1+ig2)) = nbmac + inmtot
110      continue
    endif
    do 410 j = 1, nnose
        ino = zi(jcnse-1+nnose*(im-1)+j)
! --- ON REGARDE SI LE NOEUD APPARTIENT À LA LISTE
        do 420 i = 1, inn
            if (zi(jdirno-1+(2+nfiss)*(i-1)+1) .eq. ino) then
                lnoeud = .true.
                do 430 ifiss = 1, nfiss
                    lnoeud = lnoeud .and. zi(jdirno-1+(2+nfiss)*(i-1)+ 2+ifiss) .eq. he(ifiss)
430              continue
! --- IL APPARTIENT A LA LISTE, ON L'ATTACHE À LA MAILLE
                if (lnoeud) then
                    if (opmail) zi(iacon2-1+j)=zi(jdirno-1+(2+nfiss)*( i-1)+2)
                    goto 410
                endif
            endif
420      continue
        if (ino .lt. 1000) then
            iad = iacoo1
        else if (ino.gt.1000.and.ino.lt.2000) then
            iad = iad1+ndim*(ino-1001)
        else if (ino.gt.2000.and.ino.lt.3000) then
            iad = iad9+ndim*(ino-2001)
        else if (ino.gt.3000) then
            iad = iad9+ndim*(ino-3001)
        endif
        if (opmail) then
! --- IL N'APPARTIENT PAS A LA LISTE, ON LE CREE AVANT DE L'ATTACHER
            call xpolsn(elrefp, ino, n, jlsn, jlst,&
                        ima, iad, igeom, nfiss, ndime,&
                        ndim, jconx1, jconx2, co, lsn,&
                        lst)
            call xpoajn(maxfem, ino, lsn, jdirno, prefno,&
                        nfiss, he, nnn, inn, inntot,&
                        nbnoc, nbnofi, inofi, co, iacoo2)
            zi(iacon2-1+j)= zi(jdirno-1+(2+nfiss)*(inn-1)+2)
        else
! --- IL N'APPARTIENT PAS A LA LISTE, ON CALCULE SON DÉPLACEMENT
            call xpolsn(elrefp, ino, n, jlsn, jlst,&
                        ima, iad, igeom, nfiss, ndime,&
                        ndim, jconq1, jconq2, co, lsn,&
                        lst)
            call xpoajd(elrefp, ino, n, lsn, lst,&
                        ninter, iainc, typma, co, igeom,&
                        jdirno, nfiss, jfisno, he, ndime,&
                        ndim, cmp, nbcmp, nfh, nfe,&
                        ddlc, ima, jconq1, jconq2, jcnsv1,&
                        jcnsv2, jcnsl2, nbnoc, inntot, inn,&
                        nnn, contac, lmeca)
        endif
!
410  end do
!
    call jedema()
end subroutine
