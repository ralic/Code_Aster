subroutine nocart(chinz, code, groupz, modez, nma,&
                  limano, limanu, nmligz, ncmp)
    implicit none
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
#include "jeveux.h"
!
#include "asterfort/agcart.h"
#include "asterfort/assert.h"
#include "asterfort/editgd.h"
#include "asterfort/jeagco.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
    integer :: code, nma, ncmp, limanu(*)
    character(len=3) :: mode
    character(len=24) :: groupe
    character(len=*) :: limano(*)
    character(len=8) :: limanz
    character(len=19) :: chin, nomlig
    character(len=*) :: chinz, nmligz, groupz, modez
! ----------------------------------------------------------------------
!     ENTREES:
!     CHINZ : NOM DE LA CARTE A ENRICHIR
!     CODE : 1: 'TOUT' LES MAILLES DU MAILLAGE.
!           -1: 'TOUT' LES MAILLES SUPPL. D'1 LIGREL.
!            2: 1 GROUPE NOMME DE MAILLES DU MAILLAGE.
!            3: 1 LISTE TARDIVE DE MAILLES DU MAILLAGE.
!           -3: 1 LISTE TARDIVE DE MAILLES TARDIVES D'1 LIGREL.
!     GROUPZ : NOM D' 1 GROUPE DE MAILLES DU MAILLAGE
!              ( UNIQUEMENT SI CODE= 2)
!     MODEZ : 'NOM' OU 'NUM' :
!             SI 'NOM' ON UTILISE LA LISTE LIMANO (NOMS DES MAILLES)
!                 ( UNIQUEMENT SI CODE= 3)
!             SI 'NUM' ON UTILISE LA LISTE LIMANU (NUMERO DES MAILLES)
!                 ( UNIQUEMENT SI CODE= 3 OU -3)
!     NMA  : NOMBRE DE MAILLES DANS LIMANO OU LIMANU
!                 ( UNIQUEMENT SI CODE= 3 OU -3)
!     LIMANO : NOMS DES MAILLES DU GROUPE_TARDIF (CODE=3)
!     LIMANU : NUMEROS DES MAILLES DU GROUPE_TARDIF (CODE=3 OU -3)
!     NMLIGZ : NOM DU LIGREL OU SONT EVENTUELLEMENT DEFINIES LES MAILLES
!         TARDIVES QUE L'ON VEUT AFFECTER.
!         NOMLIG EST NON_BLANC UNIQUEMENT SI CODE=-3 OU CODE=-1
!     NCMP : NOMBRE DE COMPOSANTES DECRITES
!            DANS CHIN.NCMP ET CHIN.VALV
!            -- REMARQUE : ON PEUT SUR-DIMENSIONNER NCMP A CONDITION
!                          QUE LA LISTE DES NOMS DE CMPS CONTIENNE DES
!                          "BLANCS". LES CMPS REELLEMENT NOTEES SONT
!                          LES COMPOSANTES NON-BLANCHES.
!
!     SORTIES:
!     ON ENRICHIT LE CONTENU DE LA CARTE CHIN
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!-----------------------------------------------------------------------
    integer :: nec, nedit, ngdmx, iaddg, gr, dim, i, numero, gd
    integer :: jdesc, jlima, ldim
    character(len=8) :: ma, kbid, base
    integer :: noma, noli
    character(len=24) :: clima, trav
    logical :: laggr
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ibid, illima, lontap, lontav
!-----------------------------------------------------------------------
    call jemarq()
    chin = chinz
    nomlig = nmligz
    groupe = groupz
    mode = modez
    laggr=.false.
!
    call jeveuo(chin//'.NOMA', 'L', noma)
    ma = zk8(noma-1+1)
!
    call jeveuo(chin//'.DESC', 'E', jdesc)
    gd = zi(jdesc-1+1)
    nec = nbec(gd)
    ngdmx = zi(jdesc-1+2)
    nedit = zi(jdesc-1+3) + 1
!
!
!     -- FAUT-IL AGRANDIR LA CARTE ?
!     -------------------------------
    if (nedit .gt. ngdmx) then
        laggr=.true.
        ngdmx=2*ngdmx
        call agcart(ngdmx, chin)
        call jeveuo(chin//'.DESC', 'E', jdesc)
    endif
!
    zi(jdesc-1+3) = nedit
!
    call jeveuo(chin//'.NOLI', 'E', noli)
    if ((code.eq.-1) .or. (code.eq.-3)) then
        call assert(nomlig(1:8).ne.' ')
        zk24(noli-1+nedit) = nomlig
    endif
!
!     APPEL A EDITGD QUI REMPLIT .VALE ET MET A JOUR  LE DESC_GRANDEUR
    iaddg = 3 + 2*ngdmx + (nedit-1)*nec + 1
    call editgd(chin, ncmp, gd, nedit, zi(jdesc-1+iaddg))
!
!
!     MISE A JOUR DE DESC :
!     ----------------------
    zi(jdesc-1+3+2*nedit-1) = code
    dim = 0
    if (abs(code) .eq. 1) then
!        -- ON NOTE LE NUMERO D'ENTITE CONVENTIONNEL RELATIF
!        -- A "TOUT":   9999
        zi(jdesc-1+3+2*nedit) = 9999
    else if (code.eq.2) then
        call jenonu(jexnom(ma//'.GROUPEMA', groupe), gr)
        zi(jdesc-1+3+2*nedit) = gr
    else if (abs(code).eq.3) then
        zi(jdesc-1+3+2*nedit) = nedit
        dim = nma
    else
        call assert(.false.)
    endif
!
!
!
!     MISE A JOUR DE LIMA :
!     ----------------------
!     RECOPIE DANS LIMA DES NUMEROS DE MAILLES DU GROUPE TARDIF:
!
!     -- FAUT-IL AGRANDIR .LIMA ?
    call jelira(chin//'.LIMA', 'LONT', lontav, kbid)
    call jeveuo(jexatr(chin//'.LIMA', 'LONCUM'), 'L', illima)
    lontap=zi(illima-1+nedit)+max(dim,1)
    if (lontap .gt. lontav) then
        laggr=.true.
        lontap=max(2*lontav,lontap)
    endif
    if (laggr) then
        clima=chin//'.LIMA'
        trav=chin//'.TRAV'
        call jedupo(clima, 'V', trav, .false.)
        call jelira(clima, 'CLAS', ibid, base)
        call jedetr(chin//'.LIMA')
        call jeagco(trav, clima, ngdmx, lontap, base)
        call jedetr(trav)
    endif
!
    call jecroc(jexnum(chin//'.LIMA', nedit))
    ldim = max(dim,1)
    call jeecra(jexnum(chin//'.LIMA', nedit), 'LONMAX', ldim, ' ')
    call jeveuo(jexnum(chin//'.LIMA', nedit), 'E', jlima)
    do 100 i = 1, dim
        if (mode .eq. 'NUM') then
!             MAILLES NUMEROTEES ( DU MAILLAGE (>0) OU TARDIVES(<0) )
            zi(jlima-1+i) = limanu(i)
        else if (mode.eq.'NOM') then
!             MAILLES NOMMEES DU MAILLAGE
            limanz = limano(i)
            call jenonu(jexnom(ma//'.NOMMAI', limanz), numero)
            zi(jlima-1+i) = numero
        else
            call assert(.false.)
        endif
100  end do
!
    call jedema()
end subroutine
