subroutine nocart(carte, code, ncmp, groupma, mode, nma,&
                  limano, limanu, ligrel)
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
    character(len=*), intent(in) :: carte
    integer, intent(in) :: code
    integer, intent(in) :: ncmp
    character(len=*), intent(in), optional :: groupma
    character(len=*),intent(in), optional :: mode
    integer, intent(in), optional :: nma
    character(len=*), intent(in), optional :: limano(*)
    integer, intent(in), optional :: limanu(*)
    character(len=*), intent(in), optional ::  ligrel
! ----------------------------------------------------------------------
!     ENTREES:
!     --------
!     carte : NOM DE LA CARTE A ENRICHIR
!
!     CODE : 1: 'TOUT(ES)' LES MAILLES DU MAILLAGE.
!           -1: 'TOUT(ES)' LES MAILLES SUPPL. D'1 LIGREL.
!            2: 1 GROUPE_MA DU MAILLAGE.
!            3: 1 LISTE DE MAILLES DU MAILLAGE.
!           -3: 1 LISTE DE MAILLES TARDIVES D'1 LIGREL.
!
!     NCMP : NOMBRE DES COMPOSANTES DECRITES
!            DANS CHIN.NCMP ET CHIN.VALV
!            -- REMARQUE : ON PEUT SUR-DIMENSIONNER NCMP A CONDITION
!                          QUE LA LISTE DES NOMS DE CMPS CONTIENNE DES
!                          "BLANCS". LES CMPS REELLEMENT NOTEES SONT
!                          LES COMPOSANTES NON-BLANCHES.
!
!     groupma : NOM D' 1 GROUP_MA DU MAILLAGE
!              ( UNIQUEMENT SI CODE= 2)
!
!     mode : 'NOM' OU 'NUM' :
!             SI 'NOM' ON UTILISE LA LISTE LIMANO (NOMS DES MAILLES)
!                 ( UNIQUEMENT SI CODE= 3)
!             SI 'NUM' ON UTILISE LA LISTE LIMANU (NUMERO DES MAILLES)
!                 ( UNIQUEMENT SI CODE= 3 OU -3)
!
!     NMA  : NOMBRE DE MAILLES DANS LIMANO OU LIMANU
!                 ( UNIQUEMENT SI CODE= 3 OU -3)
!
!     LIMANO : NOMS DES MAILLES DU GROUPE_TARDIF (CODE=3)
!
!     LIMANU : NUMEROS DES MAILLES DU GROUPE_TARDIF (CODE=3 OU -3)
!
!     ligrel : NOM DU LIGREL OU SONT EVENTUELLEMENT DEFINIES LES MAILLES
!         TARDIVES QUE L'ON VEUT AFFECTER.
!         NOMLIG EST NON_BLANC UNIQUEMENT SI CODE=-3 OU CODE=-1
!
!     SORTIES:
!     ON ENRICHIT LE CONTENU DE LA CARTE CHIN
!
! ----------------------------------------------------------------------
    character(len=24) :: groupe
    character(len=19) :: chin, nomlig
    character(len=8) :: nomail
    integer :: nec, nedit, ngdmx, iaddg, gr, dim, i, numero, gd
    integer :: jdesc, jlima, ldim
    character(len=8) :: ma, base,mode2
    integer :: noma, noli
    character(len=24) :: clima, trav
    logical :: laggr
    integer ::  illima, lontap, lontav
!-----------------------------------------------------------------------
    call jemarq()
    chin = carte
    laggr=.false.
    nomlig=' '
    groupe=' '

!   -- verification des arguments :
!   --------------------------------------------
    if (code.eq.1) then
    elseif (code.eq.-1) then
       ASSERT(present(ligrel))
       nomlig = ligrel
    elseif (code.eq.2) then
       ASSERT(present(groupma))
       groupe = groupma
    elseif (code.eq.3) then
       ASSERT(present(mode))
       ASSERT(mode.eq.'NUM' .or. mode.eq.'NOM')
       mode2=mode
       ASSERT(present(nma))
       ASSERT(nma.gt.0)
       if (mode2.eq.'NUM') then
          ASSERT(present(limanu))
       else
          ASSERT(present(limano))
       endif
    elseif (code.eq.-3) then
       ASSERT(present(ligrel))
       nomlig = ligrel
       ASSERT(present(nma))
       ASSERT(nma.gt.0)
       ASSERT(present(limanu))
       mode2='NUM'
    else
       ASSERT(.false.)
    endif




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
        ASSERT(nomlig(1:8).ne.' ')
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
        call jenonu(jexnom(ma//'.GROUPEMA', groupma), gr)
        zi(jdesc-1+3+2*nedit) = gr
    else if (abs(code).eq.3) then
        zi(jdesc-1+3+2*nedit) = nedit
        dim = nma
    else
        ASSERT(.false.)
    endif
!
!
!
!     MISE A JOUR DE LIMA :
!     ----------------------
!     RECOPIE DANS LIMA DES NUMEROS DE MAILLES DU GROUPE TARDIF:
!
!     -- FAUT-IL AGRANDIR .LIMA ?
    call jelira(chin//'.LIMA', 'LONT', lontav)
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
        call jelira(clima, 'CLAS', cval=base)
        call jedetr(chin//'.LIMA')
        call jeagco(trav, clima, ngdmx, lontap, base)
        call jedetr(trav)
    endif
!
    call jecroc(jexnum(chin//'.LIMA', nedit))
    ldim = max(dim,1)
    call jeecra(jexnum(chin//'.LIMA', nedit), 'LONMAX', ldim)
    call jeveuo(jexnum(chin//'.LIMA', nedit), 'E', jlima)
    do 100 i = 1, dim
        if (mode2.eq.'NUM') then
!             MAILLES NUMEROTEES ( DU MAILLAGE (>0) OU TARDIVES(<0) )
            zi(jlima-1+i) = limanu(i)
        else if (mode2.eq.'NOM') then
!             MAILLES NOMMEES DU MAILLAGE
            nomail = limano(i)
            call jenonu(jexnom(ma//'.NOMMAI', nomail), numero)
            zi(jlima-1+i) = numero
        else
            ASSERT(.false.)
        endif
100  end do
!
    call jedema()
end subroutine
