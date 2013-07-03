subroutine xpocmp(elrefp, cns1, ima, n, jconx1,&
                  jconx2, ndim, nfh, nfe, ddlc,&
                  nbcmp, cmp, lmeca)
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
! aslint: disable=W1306
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elelin.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: ndim, nfh, nfe, ima, n, jconx1, jconx2, nbcmp, cmp(nbcmp)
    integer :: ddlc
    logical :: lmeca
    character(len=8) :: elrefp
    character(len=19) :: cns1
!
!   DETERMINER LES COMPOSANTES ACTIVES DU CHAMP DE DEPLACEMENT
!
!   IN
!     CNS1   : CHAMP_NO_S DU DEPLACEMENT EN ENTREE
!     IMA    : NUMERO DE MAILLE COURANTE PARENT
!     N      : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
!     JCONX1 : ADRESSE DE LA CONNECTIVITE DU MAILLAGE SAIN
!     JCONX2 : LONGUEUR CUMULEE DE LA CONNECTIVITE DU MAILLAGE SAIN
!     NDIM   : DIMENSION DU MAILLAGE
!     NBCMP  : NOMBRE DE COMPOSANTES DU CHAMP_NO DE DEPL1
!     LMECA  : VRAI DANS LE CAS MECANIQUE (SINON CAS THERMIQUE)
!
!   OUT
!     NFH    : NOMBRE DE FONCTIONS HEAVISIDE (PAR NOEUD)
!     NFE    : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT (1 A 4)
!     CMP    : POSITION DES DDLS DE DEPL X-FEM DANS LE CHAMP_NO DE DEPL1
!
!
    integer :: jcnsc1, jcnsl1, i, j, k, ino, icmp, ndc, ipos, nnos, ibid
    logical :: exist(n, nbcmp), contas
    character(len=8) :: nomcmp, k8bid
!
!     ------------------------------------------------------------------
!
    call jemarq()
!
!     COMPOSANTES DU CHAMP DE DEPLACEMENT 1
    call jeveuo(cns1//'.CNSC', 'L', jcnsc1)
    call jeveuo(cns1//'.CNSL', 'L', jcnsl1)
!
    do 110 j = 1, n
!       INO : NUMÉRO DU NOEUD DANS MALINI
        ino=zi(jconx1-1+zi(jconx2+ima-1)+j-1)
        do 111 icmp = 1, nbcmp
            exist(j,icmp)= zl(jcnsl1-1+(ino-1)*nbcmp + icmp)
111      continue
110  end do
!
!     ON REGARDE LES COMPOSANTES ACTIVES EN CHAQUE NOEUD
    ipos = 0
    ndc = 0
    nfh = 0
    nfe = 0
    ddlc=0
    contas=.true.
    call elelin(1, elrefp, k8bid, ibid, nnos)
!
    do 21 i = 1, nbcmp
        nomcmp = zk8(jcnsc1-1+i)
!
        if (nomcmp(1:4) .eq. 'LAGS') then
            do 22 k = 1, nnos
                if (.not. exist(k,i)) contas=.false.
22          continue
            if (contas) goto 1
        endif
!
        do 24,j=1,n
        if (.not.exist(j,i)) goto 21
24      continue
!
 1      continue
!
        if (nomcmp(1:1) .eq. 'D' .or. nomcmp(1:1) .eq. 'T') then
            ipos = ipos +1
            ndc = ndc +1
            cmp(ipos)=i
        endif
        if (nomcmp(1:1) .eq. 'H') then
            ipos = ipos +1
            nfh = nfh +1
            cmp(ipos)=i
        endif
        if (nomcmp(1:2) .eq. 'E1' .or. nomcmp(1:2) .eq. 'E2' .or. nomcmp(1:2) .eq. 'E3'&
            .or. nomcmp(1:2) .eq. 'E4') then
            ipos = ipos +1
            nfe = nfe +1
            cmp(ipos)=i
        endif
        if (nomcmp(1:4) .eq. 'LAGS') then
            ipos=ipos+1
            ddlc=ddlc+1
            cmp(ipos)=i
        endif
!
21  end do
!
    if (lmeca) then
!       CAS DE LA MECANIQUE
        nfe = nfe/ndim
        nfh = nfh/ndim
        call assert(ndim.eq.ndc)
    else
!       CAS DE LA THERMIQUE
        call assert(ndc.eq.1)
    endif
!
    call jedema()
end subroutine
