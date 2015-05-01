subroutine prasml(option, nugene, tminbl, nomprn, modgen,&
                  tmnobl, tmadbl, knombl, inumbl, conleq,&
                  conlbl)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
!***********************************************************************
!    P. RICHARD     DATE 13/10/92
!-----------------------------------------------------------------------
!  BUT:      < PREPARATION ASSEMBLAGE MATRICE LIAISONS >
!
!  PREPARER L'ASSEMBLAGE POUR UN LIGREL CORRESPONDANT AUX MATRICES
!   DES LIAISONS
!   ON CONSIDERE POUR L'ASSEMBLAGE UN LISTE GENERALE DES BLOC
!   ELEMENTAIRES A ASSEMBLER DANS UNE MATRICE STOCKEE PROFIL BLOC
!   (EN GENERAL MATRICE PROJETEE=1BLOC,MATRICE DE LIAISON=NBLOCS)
!    CHAQUE LIAISON CORRESPOND A 2 MATRICES DE LIAISON
!  PLUS UNE MATRICE DE LAGRANGE-LAGRANGE AUTANT DE FOIS QU'IL Y A DE
!     LIGNE DANS LA MATRICE DE LIAISON
!
!   NOEUD TARDIF = NOEUD FICTIF SUPPORTANT UNE MATRICE DE LIAISON
!
!   ON REMPLIT TMNOBL TMADBL KNOMBL INUMBL CONLEC CONLBL
!
!-----------------------------------------------------------------------
!
! NOM----- / /:
!
! OPTION   /I/: NOM K11 DE L'OPTION D'ASSEMBLAGE
! NUGENE   /I/: NOM K14 DE LA NUMEROTATION GENERALISEE
! NOMPRN   /I/: NOM K8 DU LIGREL COURANT A TRAITER
! TMINBL   /I/: NOM K24 DE LA FAMILLE NOMMEE AU NOM DES LIGRELS
!               ET DONNANT POUR CHAQUE NOEUD TARDIF DU LIGREL
!               LE NUMERO DE SON 1 BLOC DANS LA LISTE GENERALE ET
!               LE NOMBRE DE BLOC, LES DEUX NOEUDS TARDIF D'UNE LIAISON
!               SONT CONSECUTIFS
! MODGEN   /I/: NOM K8 MODELE_GENERALISE AMONT
! TMNOBL   /I/: NOM K24 DE LA FAMILLE NUMEROTE DONNANT POUR CHAQUE
!               TERME D'UN BLOC ELEMENTAIRE LE NUMERO DU BLOC ASSEMBLE
!               D'ARRIVE
! TMADBL   /I/: NOM K24 DE LA FAMILLE NUMEROTE DONNANT POUR CHAQUE
!               TERME D'UN BLOC ELEMENTAIRE LE RANG D'ARRIVEE
!               DANS LE  BLOC ASSEMBLE
! KNOMBL   /M/: VECTEUR DES NOM K24 DES OBJETS OU FAMILLE CONTENANT
!               LES BLOCS ELEMENTAIRES
! INUMBL   /M/: VECTEUR NUMERO  BLOCS ELEMNTAIRE DANS LEUR FAMILLE OU 0
!               LES BLOCS ELEMENTAIRES
! CONLEQ   /M/: VECTEUR REEL DES COEF DE CONDITIONNEMENT  AFFECTE
!               AUX EQUATION
! CONLBL   /M/: VECTEUR REEL DES COEF DE CONDITIONNEMENT  AFFECTE
!               AUX BLOCS ELEMNTAIRE
!
!
!
#include "jeveux.h"
!
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/maxblo.h"
#include "asterfort/nueq_chck.h"
!
!
    character(len=8) :: modgen, sst(2), nomprn
    character(len=14) :: nugene
    character(len=19) :: prgene, stolci
    character(len=9) :: rigopt
    character(len=11) :: option, ricopt
    character(len=24) :: tmadbl, tmnobl, tminbl
    character(len=24) :: nomlia, knombl(*)
    real(kind=8) :: zero, conlbl(*), conleq(*)
    integer :: inumbl(*), ibl(3)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iad, ibid, iblc, ieqc, ieql, inuc, inul
    integer :: ivc, ivl, j, k, l, lc, ll, i_ligr_ss, i_ligr
    integer ::  lldefl,   llorl, llors, llprl
    integer ::  llprs, ltadbl, ltinbl, ltnobl, nbcol, nblig
    integer :: nbsst, ntail, ntprno, nuant, nublo, nulia, nusst
    integer :: nutars
    real(kind=8) :: sconl
    integer, pointer :: nueq(:) => null()
    integer, pointer :: lipr(:) => null()
    integer, pointer :: scib(:) => null()
    integer, pointer :: scdi(:) => null()
!-----------------------------------------------------------------------
    data rigopt,ricopt/'RIGI_GENE','RIGI_GENE_C'/
    data zero / 0.0d+00 /
!-----------------------------------------------------------------------
!
!    TEST SI ON EST SUR LES LAGRANGES ET SI OPTION=RIGI_GENE(_C)
!
    call jemarq()
!
    if (nomprn .eq. '&SOUSSTR' .or. (option.ne.rigopt.and. option.ne.ricopt)) then
        goto 999
    endif
!
!------------------RECUPERATION DU NOMBRE DE SOUS-STRUCTURE-------------
    prgene=nugene//'.NUME'
    stolci=nugene//'.SLCS'

    call nueq_chck(prgene)
!
    call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), i_ligr_ss)
    call jelira(jexnum(prgene//'.PRNO', i_ligr_ss), 'LONMAX', nbsst)
    nbsst=nbsst/2
!
!--------------------RECUPERATION DES CARACTERISTIQUES BLOCS------------
!
!
!---------------------REMPLISSAGE DES OBJETS DE TRAVAIL-----------------
!
    call jeveuo(prgene//'.NUEQ', 'L', vi=nueq)
    call jeveuo(stolci//'.SCDI', 'L', vi=scdi)
    call jeveuo(stolci//'.SCIB', 'L', vi=scib)
!
    call jenonu(jexnom('&&ASSGEN.REP.NOM.PROF', nomprn), ibid)
    call jeveuo(jexnum(tminbl, ibid), 'L', ltinbl)
    call jelira(jexnum(tminbl, ibid), 'LONMAX', ntprno)
    ntprno=ntprno/3
!
    call jenonu(jexnom(prgene//'.LILI', nomprn), i_ligr)
    call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), i_ligr_ss)


    call jeveuo(jexnum(prgene//'.ORIG', i_ligr), 'L', llorl)
    call jeveuo(jexnum(prgene//'.ORIG', i_ligr_ss), 'L', llors)
    call jeveuo(jexnum(prgene//'.PRNO', i_ligr), 'L', llprl)
    call jeveuo(jexnum(prgene//'.PRNO', i_ligr_ss), 'L', llprs)
    call jeveuo(modgen//'      .MODG.LIPR', 'L', vi=lipr)
!
    nomlia=modgen//'      .MODG.LIMA'
!
!     BOUCLE SUR LES ELEMENTS DU LIGREL
!
    do j = 1, ntprno
!       NUMERO DE LA LIAISON
        nulia=zi(llorl+j-1)
!       RECUPERATION DU PROFIL BLOC ELEMENTAIRE
        ibl(1)=zi(ltinbl+(j-1)*3)
        ibl(2)=zi(ltinbl+(j-1)*3+1)
        ibl(3)=zi(ltinbl+(j-1)*3+2)
!  RECUPERATION NOM SOUS-STRUCTURE MISE EN JEU
        call jeveuo(jexnum(modgen//'      .MODG.LIDF', nulia), 'L', lldefl)
        sst(1)=zk8(lldefl)
        sst(2)=zk8(lldefl+2)
        call jelibe(jexnum(modgen//'      .MODG.LIDF', nulia))
!  RECUPERATION DIMENSIONS ET NUMERO PREMIERE EQUATION DANS NUEQ
        nblig=zi(llprl+(j-1)*2+1)
        inul=zi(llprl+(j-1)*2)
!
!   TRAITEMENT DES BLOCS DES MATRICES DE  LIAISON
!
!  BOUCLE SUR LES BLOCS ELEMENTAIRE (CHAQUE BLOC CORRESPOND
!  A LA MATRICE DE LIAISON SUR UNE SOUS-STRUCTURE
!  SI LA MATRICE CONTIENT PLUSIEURS BLOCS FAIRE UNE BOUCLE SUR LES BLOCS
!
!   BOUCLE SUR LES DEUX MATRICE DE LIAISON DE LA LIAISON
        do k = 1, 2
!  NUMERO BLOC ELEMENTAIRE COURANT
            iblc=ibl(k)
            call jenonu(jexnom(modgen//'      .MODG.SSNO', sst(k)), nusst)
!  ECRITURE NOM DU BLOC
            knombl(iblc)=nomlia
            nublo=lipr(1+(nulia-1)*9+(k-1)*3+2)
            inumbl(iblc)=nublo
!   RECUPERATION DU NUMERO TARDIF DE LA SOUS-STRUCTURE
            do l = 1, nbsst
                if (zi(llors+l-1) .eq. nusst) nutars=l
            end do
            nbcol=zi(llprs+(nutars-1)*2+1)
            inuc=zi(llprs+(nutars-1)*2)
            ntail=nblig*nbcol
!
            call jecroc(jexnum(tmadbl, iblc))
            call jeecra(jexnum(tmadbl, iblc), 'LONMAX', ntail)
            call jeveuo(jexnum(tmadbl, iblc), 'E', ltadbl)
            call jecroc(jexnum(tmnobl, iblc))
            call jeecra(jexnum(tmnobl, iblc), 'LONMAX', ntail)
            call jeveuo(jexnum(tmnobl, iblc), 'E', ltnobl)
! DETERMINATION MAX DU BLOC
            sconl=zero
            call maxblo(jexnum(nomlia, nublo), sconl)
!     BOUCLE SUR LES TERMES DU BLOC ELEMENTAIRE
            do ll = 1, nblig
!  NUMERO D'EQUATION LIGNE
                ieql=nueq(1+(inul-1)+(ll-1))
                conleq(ieql)=max(conleq(ieql),sconl)
                conlbl(iblc)=max(conlbl(iblc),sconl)
                do lc = 1, nbcol
!  ADRESSE DU TERME DANS LE BLOC ELEMENTAIRE
                    iad=nblig*(lc-1)+ll
!  NUMERO D'EQUATION COLONNE
                    ieqc=nueq(1+(inuc-1)+(lc-1))
!
! QUI DU TERME OU DE SON TRANSPOSE ARRIVE DANS LE TRIANGLE SUP ?
                    ivl=min(ieql,ieqc)
                    ivc=max(ieql,ieqc)
!
                    zi(ltnobl+iad-1)=scib(ivc)
                    zi(ltadbl+iad-1)=scdi(ivc)-(ivc-ivl)
                end do
            end do
            call jelibe(jexnum(tmadbl, iblc))
            call jelibe(jexnum(tmnobl, iblc))
        end do
!
!   TRAITEMENT DES BLOCS LAGRANGES LAGRANGES
!
! RECUPERATION NOEUD TARDIF ANTAGONISTE
!
        do l = 1, ntprno
            if (zi(llorl+l-1) .eq. nulia .and. l .ne. j) nuant=l
        end do
        iblc=ibl(3)
        knombl(iblc)=nomlia
        nublo=lipr(1+(nulia-1)*9+6+2)
        inumbl(iblc)=nublo
! DETERMINATION MAX DU BLOC
        sconl=zero
        call maxblo(jexnum(nomlia, nublo), sconl)
        conlbl(iblc)=max(conlbl(iblc),sconl)
        call jecroc(jexnum(tmnobl, iblc))
        call jeecra(jexnum(tmnobl, iblc), 'LONMAX', nblig*2)
        call jeveuo(jexnum(tmnobl, iblc), 'E', ltnobl)
        call jecroc(jexnum(tmadbl, iblc))
        call jeecra(jexnum(tmadbl, iblc), 'LONMAX', nblig*2)
        call jeveuo(jexnum(tmadbl, iblc), 'E', ltadbl)
        do k = 1, nblig
            ieql=nueq(1+(inul-1)+(k-1))
            inuc=zi(llprl+(nuant-1)*2)
            ieqc=nueq(1+(inuc-1)+(k-1))
            conleq(ieql)=max(conleq(ieql),sconl)
            iad=k
            zi(ltnobl+iad-1)=scib(ieql)
            zi(ltadbl+iad-1)=scdi(ieql)
! QUI DU TERME OU DE SON TRANSPOSE ARRIVE DANS LE TRIANGLE SUP ?
            ivl=min(ieql,ieqc)
            ivc=max(ieql,ieqc)
!
            iad=nblig+k
            zi(ltnobl+iad-1)=scib(ivc)
            zi(ltadbl+iad-1)=scdi(ivc)-(ivc-ivl)
        end do
        call jelibe(jexnum(tmadbl, iblc))
        call jelibe(jexnum(tmnobl, iblc))
    end do
!
    call jelibe(prgene//'.NUEQ')
    call jelibe(stolci//'.SCDI')
    call jelibe(stolci//'.SCIB')
    call jelibe(modgen//'      .MODG.LIPR')
!
999 continue
    call jedema()
end subroutine
