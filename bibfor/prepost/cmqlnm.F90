subroutine cmqlnm(main, nomaqu, nbma, nonomi, nbnm)
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
! person_in_charge: nicolas.sellenet at edf.fr
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cncinv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: nbma, nbnm
    character(len=8) :: main
    character(len=24) :: nomaqu, nonomi
! ----------------------------------------------------------------------
!         TRANSFORMATION DES MAILLES QUADRATIQUES -> LINEAIRE
!-----------------------------------------------------------------------
!     -   ON RECUPERE LES NOEUDS MILIEUX
!     -   ON VERIFIE QUE LES NOEUDS MILIEUX COMMUNS A PLUSIEURS MAILLES
!         QUADRATIQUES APPARTIENNENT AUX MAILLES REFERENCEES.
!         SI DEUX MAILLES QUADRATIQUES SE PARTAGENT UN NOEUD MILIEU ET
!         QUE L'UTILISATEUR NE LINEARISE QU'UNE DES 2 MAILLES, ALORS
!         ON EMET UNE ALARME
!-----------------------------------------------------------------------
! IN        MAIN   K8  NOM DU MAILLAGE INITIAL
! IN        NOMAQU K24 NOM DE L'OBJET JEVEUX QUI CONTIENT LE NUMERO
!                      DES MAILLES QUADRATIQUES
! IN        NBMA   I   NOMBRE DE MAILLES QUADRATIQUES TRAITEES
! IN        NONOMI K24 NOM DE L'OBJET JEVEUX A ALLOUER CONTENANT LES
!                      NUMEROS DES NOEUDS A SUPPRIMER
! OUT       NBNM   I   NOMBRE DE NOEUDS MILIEUX A RECUPERER
!-----------------------------------------------------------------------
!
!  ======      ====      =================   ========================
!  MAILLE      TYPE      NB NOEUDS MILIEUX   POSITION DU PREMIER NOEUD
!  ======      ====      =================   MILIEU DANS LA MAILLE
!                                            ========================
!
!  SEG3         4             1                    3
!  TRIA6        9             3                    4
!  QUAD8       14             4                    5
!  QUAD9       16             5                    5
!  TETRA10     19             6                    5
!  PENTA15     21             9                    7
!  PENTA18     22            12                    7
!  PYRAM13     24             8                    6
!  HEXA20      26            12                    9
!  HEXA27      27            19                    9
!
!
    logical :: isasup
!
    integer :: jtyp, iacnx1, ilcnx1, jdime, ii, nbmato,  numma, iacnx2
    integer :: ilcnx2, nbtyma, nbnoto, jj, jmaqu, nbnosu, numamo, nbnomi
    integer :: ponomi, jco, nunomi, nbm1, kk, numa2, jnomi
    parameter(nbtyma=27)
    integer :: nbnmtm(nbtyma), ppnm(nbtyma)
    integer, pointer :: tab_ma(:) => null()
    integer, pointer :: tab_no(:) => null()
!
!     NBNMTM: NOMBRE DE NOEUDS MILIEU PAR TYPE DE MAILLE
!     PPNM:   POSITION DU PREMIER NOEUD MILIEU PAR TYPE DE MAILLE
!
    data nbnmtm /3*0,1,4*0,3,4*0,4,0,5,2*0, 6,0,9,12,0,8,0,12,19/
    data ppnm   /3*0,3,4*0,4,4*0,5,0,5,2*0, 5,0,7,7, 0,6,0,9, 9/
!
    call jemarq()
!
    ASSERT(nbma.gt.0)
!
    call jeveuo(nomaqu, 'L', jmaqu)
    call jeveuo(main//'.TYPMAIL', 'L', jtyp)
    call jeveuo(main//'.CONNEX', 'L', iacnx1)
    call jeveuo(jexatr(main//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
!
    call jeveuo(main//'.DIME', 'L', jdime)
    nbmato = zi(jdime+2)
    nbnoto = zi(jdime)
!
    AS_ALLOCATE(vi=tab_ma, size=nbmato)
    do 10 ii = 1, nbmato
        tab_ma(ii) = 0
10  end do
!
    do 20 ii = 1, nbma
        numma = zi(jmaqu+ii-1)
        tab_ma(numma) = 1
20  end do
!
    AS_ALLOCATE(vi=tab_no, size=nbnoto)
    do 70 ii = 1, nbnoto
        tab_no(ii) = 0
70  end do
!
!     CREATION DE LA CONNECTIVITE INVERSE
    call cncinv(main, [0], 0, 'V', '&&CMQLNM.CONINV')
    call jeveuo('&&CMQLNM.CONINV', 'L', iacnx2)
    call jeveuo(jexatr('&&CMQLNM.CONINV', 'LONCUM'), 'L', ilcnx2)
!
!     BOUCLE SUR LES MAILLES A MODIFIER
    nbnosu = 0
    do 30 ii = 1, nbma
        numamo = zi(jmaqu+ii-1)
        nbnomi = nbnmtm(zi(jtyp+numamo-1))
        ponomi = ppnm(zi(jtyp+numamo-1))
!
        jco=iacnx1+ zi(ilcnx1-1+numamo)-1
!       BOUCLE SUR LES NOEUDS MILIEUX DE CES MAILLES
        do 40 jj = 1, nbnomi
            nunomi = zi(jco+ponomi-1+jj-1)
!
!         SI LE NOEUD A DEJA ETE TRAITE ON NE LE TRAITE PAS
            if (tab_no(nunomi) .ne. 0) goto 40
!
            nbm1 = zi(ilcnx2+nunomi)-zi(ilcnx2-1+nunomi)
!
!         BOUCLE SUR LES MAILLES AUXQUELLES SONT LIEES CE NOEUD
            isasup = .true.
            do 50 kk = 1, nbm1
                numa2 = zi(iacnx2+zi(ilcnx2-1+nunomi)-1+kk-1)
!           SI UNE DE CES MAILLES N'EST PAS A MODIFIER ALORS ON
!           NE DOIT PAS SUPPRIMER LE NOEUD
                if (tab_ma(numa2) .eq. 0) isasup = .false.
50          continue
            if (isasup) then
                tab_no(nunomi) = 2
                nbnosu = nbnosu + 1
            else
                tab_no(nunomi) = 1
            endif
40      continue
30  end do
!
    AS_DEALLOCATE(vi=tab_ma)
!
    if (nbnosu .eq. 0) then
        call utmess('F', 'MODELISA4_3')
    endif
    call wkvect(nonomi, 'V V I', nbnosu, jnomi)
!
    nbnm = nbnosu
!
    nbnosu = 0
    do 60 ii = 1, nbnoto
        if (tab_no(ii) .eq. 2) then
            nbnosu = nbnosu+1
            zi(jnomi+nbnosu-1) = ii
        endif
60  end do
!
    ASSERT(nbnosu.eq.nbnm)
!
    AS_DEALLOCATE(vi=tab_no)
!
!
    call jedema()
!
end subroutine
