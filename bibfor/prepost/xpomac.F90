subroutine xpomac(malini, mailc, listno, nbnoc, nbmac,&
                  maxfem, nivgrm, cns1, cns2, ces1,&
                  ces2, cesvi1, cesvi2, resuco, comps1,&
                  comps2)
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/xpoco1.h"
#include "asterfort/xpoco2.h"
    integer :: nbnoc, nbmac
    character(len=8) :: malini, maxfem, resuco
    character(len=19) :: cns1, cns2, ces1, ces2, cesvi1, cesvi2
    character(len=19) :: comps1, comps2
    character(len=24) :: mailc, listno, nivgrm
!
!
!      TRAITEMENT DES MAILLES DE MAILC
!       - POUR POST_MAIL_XFEM : LES MAILLES DE MAILC ET LES NOEUDS
!                               ASSOCIÉS SONT COPIES DANS MAXFEM A
!                               L'IDENTIQUE
!       - POUR POST_CHAM_XFEM : LES NOEUDS ASSOCIÉS A MAILC
!                               SONT COPIES DANS RES_XFEM A
!                               L'IDENTIQUE
!
!   IN
!       MALINI : MAILLAGE SAIN
!       MAILC  : LISTE DES NUMEROS DES MAILLES NON SOUS-DECOUPEES
!       LISTNO : LISTE DES NUMEROS DES NOEUDS CLASSIQUES
!       NBNOC  : NOMBRE DE NOEUDS CLASSIQUES DU MAILLAGE FISSURE
!       NBMAC  : NOMBRE DE MAILLES CLASSIQUES DU MAILLAGE FISSURE
!       MAXFEM : MAILLAGE FISSURE
!       CNS1   : CHAMP_NO_S DU DEPLACEMENT EN ENTREE
!       CES1   : CHAMP_ELEM_S DE CONTRAINTES EN ENTREE
!       NIVGRM : VECTEUR DE REMPLISSAGE DES GROUP_MA DE MAXFEM
!       RESUCO : NOM DU CONCEPT RESULTAT D'ORIGINE
!
!   OUT
!       MAXFEM : MAILLAGE FISSURE (SI POST_MAIL_XFEM)
!       CNS2   : CHAMP_NO_S DU DEPLACEMENT EN SORTIE
!       CES2   : CHAMP_ELEM_S DE CONTRAINTES EN SORTIE
!       NIVGRM : VECTEUR DE REMPLISSAGE DES GROUP_MA DE MAXFEM
!
!
!
    integer :: jdirma, jmac, iret, nbno, jdirno, ino, i
    integer :: nbma, ima, jno, ier, jnivgr
    character(len=8) :: k8b
    character(len=16) :: k16b, nomcmd
    character(len=24) :: dirmai, dirnoe
!
!
    call jemarq()
!
    call jeexin(mailc, ier)
    if (ier .eq. 0) goto 999
!
!     NOM DE LA COMMANDE (POST_MAIL_XFEM OU POST_CHAM_XFEM)
    call getres(k8b, k16b, nomcmd)
!
!     CREATION DU TABLEAU DE CORRESPONDANCE DES NUMEROS DE MAILLES
!     SOIT IMA LE NUMERO D'UNE MAILLE DU MAILLAGE INITIAL
!     ALORS ZI(ITAB-1+IMA) EST LE NUMERO DE CETTE MAILLE DANS LE
!     MAILLAGE X-FEM
    call jeveuo(mailc, 'L', jmac)
    dirmai = '&&XPOMAC.DIRMAI'
    call dismoi('F', 'NB_MA_MAILLA', malini, 'MAILLAGE', nbma,&
                k8b, iret)
    call wkvect(dirmai, 'V V I', nbma, jdirma)
!
    do 100 i = 1, nbmac
!        NUMERO DE LA MAILLE DANS LE MAILLAGE INITIAL
        ima = zi(jmac-1+i)
!        NUMERO DE LA MAILLE DANS LE MAILLAGE X-FEM
        zi(jdirma-1+ima)=i
100  end do
!
!     CREATION DU TABLEAU DE CORRESPONDANCE DES NUMEROS DE NOEUDS
!     SOIT INO LE NUMERO D'UN NOEUD DU MAILLAGE INITIAL
!     ALORS ZI(ITAB-1+INO) EST LE NUMERO DE CE NOEUD DANS LE
!     MAILLAGE X-FEM
    dirnoe = '&&XPOMAC.DIRNOE'
    call dismoi('F', 'NB_NO_MAILLA', malini, 'MAILLAGE', nbno,&
                k8b, iret)
    call wkvect(dirnoe, 'V V I', nbno, jdirno)
    call jeexin(listno, ier)
    call assert(ier.ne.0)
    call jeveuo(listno, 'L', jno)
!
    do 110 i = 1, nbnoc
!        NUMERO DU NOEUD DANS LE MAILLAGE INITIAL
        ino = zi(jno-1+i)
!        NUMERO DU NOEUD DANS LE MAILLAGE X-FEM
        zi(jdirno-1+ino)=i
110  end do
!
    call jedetr(listno)
!
    if (nomcmd .eq. 'POST_MAIL_XFEM') then
!
!       RECUPERATION DE L'ADRESSE DU VECTEUR DE REMPLISSAGE DES GROUP_MA
        call jeexin(nivgrm, iret)
        if (iret .ne. 0) call jeveuo(nivgrm, 'E', jnivgr)
!
!       COPIE MAILLES DE MAILC ET NOEUDS SOUS-JACENTS DANS MAXFEM
        call xpoco1(zi(jdirma), nbma, zi(jdirno), nbno, malini,&
                    maxfem, jnivgr)
!
    else if (nomcmd.eq.'POST_CHAM_XFEM') then
!
!       COPIE DEPLACEMENT DES NOEUDS SOUS-JACENTS DANS MAXFEM
!       ET TRAITEMENT DES GROUPES, COMPORTEMENT....
        call xpoco2(malini, zi(jdirno), nbno, zi(jdirma), nbma,&
                    cns1, cns2, ces1, ces2, cesvi1,&
                    cesvi2, resuco, comps1, comps2)
!
    endif
!
    call jedetr(dirmai)
    call jedetr(dirnoe)
    if (nomcmd .eq. 'POST_MAIL_XFEM') call jedetr(mailc)
!
999  continue
!
    call jedema()
end subroutine
