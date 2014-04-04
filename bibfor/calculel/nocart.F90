subroutine nocart(carte, code, ncmp, groupma, mode, nma,&
                  limano, limanu, ligrel,&
                  jdesc,jnoma,jncmp,jnoli,jvale,&
                  jvalv,jnocmp,ncmpmx,nec, ctype,jlclima,lontav)

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
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/jecroc.h"
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

    integer, intent(inout), optional ::  jdesc
    integer, intent(inout), optional ::  jnoma
    integer, intent(inout), optional ::  jncmp
    integer, intent(inout), optional ::  jnoli
    integer, intent(inout), optional ::  jvale
    integer, intent(inout), optional ::  jvalv
    integer, intent(in)   , optional ::  jnocmp
    integer, intent(in)   , optional ::  ncmpmx
    integer, intent(in)   , optional ::  nec
    character(len=8), intent(in), optional ::  ctype
    integer, intent(inout), optional ::  jlclima
    integer, intent(inout), optional ::  lontav
! ----------------------------------------------------------------------
!     entrees:
!     --------
!     carte : nom de la carte a enrichir
!
!     code : 1: 'tout(es)' les mailles du maillage.
!           -1: 'tout(es)' les mailles suppl. d'1 ligrel.
!            2: 1 groupe_ma du maillage.
!            3: 1 liste de mailles du maillage.
!           -3: 1 liste de mailles tardives d'1 ligrel.
!
!     ncmp : nombre des composantes decrites
!            dans chin.ncmp et chin.valv
!            -- remarque : on peut sur-dimensionner ncmp a condition
!                          que la liste des noms de cmps contienne des
!                          "blancs". les cmps reellement notees sont
!                          les composantes non-blanches.
!
!     groupma : nom d' 1 group_ma du maillage
!              ( uniquement si code= 2)
!
!     mode : 'nom' ou 'num' :
!             si 'nom' on utilise la liste limano (noms des mailles)
!                 ( uniquement si code= 3)
!             si 'num' on utilise la liste limanu (numero des mailles)
!                 ( uniquement si code= 3 ou -3)
!
!     nma  : nombre de mailles dans limano ou limanu
!                 ( uniquement si code= 3 ou -3)
!
!     limano : noms des mailles du groupe_tardif (code=3)
!
!     limanu : numeros des mailles du groupe_tardif (code=3 ou -3)
!
!     ligrel : nom du ligrel ou sont eventuellement definies les mailles
!         tardives que l'on veut affecter.
!         ligrel est utilise uniquement si code=-3 ou code=-1
!
!     Tous les arguments suivants sont facultatifs.
!     ---------------------------------------------
!     Ils ne sont pas documentes. Il ne doivent etre renseignes que dans le
!     cas ou la routine nocart est appelee de (trop) nombreuses fois.
!     Exemple d'utilisation : aflrch.F90
!
!     jdesc : adresse de l'objet chin.DESC
!     jnoma : adresse de l'objet chin.NOMA
!     jnoli : adresse de l'objet chin.NOLI
!     jvale : adresse de l'objet chin.VALE
!     jvalv : adresse de l'objet chin.VALV
!     + jncmp, jnocmp, ncmpmx, nec, ctype, jlclima, lontav

!     sorties:
!        on enrichit le contenu de la carte chin
!
! ----------------------------------------------------------------------
    character(len=24) :: groupe
    character(len=19) :: chin, nomlig
    character(len=8) :: nomail
    integer :: nedit, ngdmx, iaddg, gr, dim, i, numero, gd
    integer :: jlima, ldim,jdesc2,jnoma2,jncmp2,jnoli2,jvale2,jvalv2
    integer :: jnocmp2,ncmpmx2,nec2
    character(len=8) :: ma, base,mode2,ctype2
    character(len=24) :: clima, trav
    logical :: laggr
    integer ::  jlclima2, lontap, lontav2
!-----------------------------------------------------------------------
    chin = carte
    laggr=.false.
    nomlig=' '
    groupe=' '


!   -- gestion des derniers arguments facultatifs (pour gains CPU) :
!   ----------------------------------------------------------------
    if (.not.present(jdesc)) then
        call jeveuo(chin//'.DESC', 'E', jdesc2)
    else
        jdesc2=jdesc
    endif
    gd = zi(jdesc2-1+1)

    if (.not.present(jnoma)) then
        call jeveuo(chin//'.NOMA', 'E', jnoma2)
    else
        jnoma2=jnoma
    endif
    if (.not.present(jncmp)) then
        call jeveuo(chin//'.NCMP', 'E', jncmp2)
    else
        jncmp2=jncmp
    endif
    if (.not.present(jnoli)) then
        call jeveuo(chin//'.NOLI', 'E', jnoli2)
    else
        jnoli2=jnoli
    endif
    if (.not.present(jvale)) then
        call jeveuo(chin//'.VALE', 'E', jvale2)
    else
        jvale2=jvale
    endif
    if (.not.present(jvalv)) then
        call jeveuo(chin//'.VALV', 'E', jvalv2)
    else
        jvalv2=jvalv
    endif
    if (.not.present(ctype)) then
        call jelira(chin//'.VALV', 'TYPELONG', cval=ctype2)
    else
        ctype2=ctype
    endif
    if (.not.present(lontav)) then
        call jelira(chin//'.LIMA', 'LONT', lontav2)
    else
        lontav2=lontav
    endif

    if (.not.present(jnocmp)) then
        call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', jnocmp2)
    else
        jnocmp2=jnocmp
    endif
    if (.not.present(ncmpmx)) then
        call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx2)
    else
        ncmpmx2=ncmpmx
    endif
    if (.not.present(nec)) then
        nec2 = nbec(gd)
    else
        nec2=nec
    endif
    if (.not.present(jlclima)) then
        call jeveuo(jexatr(chin//'.LIMA', 'LONCUM'), 'L', jlclima2)
    else
        jlclima2=jlclima
    endif


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


    ma = zk8(jnoma2-1+1)
    ngdmx = zi(jdesc2-1+2)
    nedit = zi(jdesc2-1+3) + 1


!   -- faut-il agrandir la carte ?
!   -------------------------------
    if (nedit .gt. ngdmx) then
        laggr=.true.
        ngdmx=2*ngdmx
        call agcart(ngdmx, chin)
        ! agcart a deplace les objets .DESC, .NOLI et .VALE :
        call jeveuo(chin//'.DESC', 'E', jdesc2)
        call jeveuo(chin//'.NOLI', 'E', jnoli2)
        call jeveuo(chin//'.VALE', 'E', jvale2)
        if (present(jdesc)) jdesc=jdesc2
        if (present(jnoli)) jnoli=jnoli2
        if (present(jvale)) jvale=jvale2
    endif

    zi(jdesc2-1+3) = nedit

    if ((code.eq.-1) .or. (code.eq.-3)) then
        ASSERT(nomlig(1:8).ne.' ')
        zk24(jnoli2-1+nedit) = nomlig
    endif

!   -- appel a editgd qui remplit .vale et met a jour  le desc_grandeur
    iaddg = 3 + 2*ngdmx + (nedit-1)*nec2 + 1
    call editgd(ncmp,nedit,zi(jdesc2-1+iaddg),ncmpmx2,&
                ctype2,jnocmp2,jncmp2,jvalv2,jvale2)
!
!
!   -- mise a jour de .desc :
!   --------------------------
    zi(jdesc2-1+3+2*nedit-1) = code
    dim = 0
    if (abs(code) .eq. 1) then
!        -- ON NOTE LE NUMERO D'ENTITE CONVENTIONNEL RELATIF
!        -- A "TOUT":   9999
        zi(jdesc2-1+3+2*nedit) = 9999
    else if (code.eq.2) then
        call jenonu(jexnom(ma//'.GROUPEMA', groupma), gr)
        zi(jdesc2-1+3+2*nedit) = gr
    else if (abs(code).eq.3) then
        zi(jdesc2-1+3+2*nedit) = nedit
        dim = nma
    else
        ASSERT(.false.)
    endif
!
!
!   -- mise a jour de lima :
!   ------------------------
!
!   -- faut-il agrandir .lima ?
   !call jelira(chin//'.LIMA', 'LONT', lontav)
    lontap=zi(jlclima2-1+nedit)+max(dim,1)
    if (lontap .gt. lontav2) then
        laggr=.true.
        lontap=max(2*lontav2,lontap)
    endif
    if (laggr) then
        clima=chin//'.LIMA'
        trav=chin//'.TRAV'
        call jedupo(clima, 'V', trav, .false.)
        call jelira(clima, 'CLAS', cval=base)
        call jedetr(chin//'.LIMA')
        call jeagco(trav, clima, ngdmx, lontap, base)
        call jedetr(trav)
        call jeveuo(jexatr(chin//'.LIMA', 'LONCUM'), 'L', jlclima2)
        if (present(jlclima)) jlclima=jlclima2
        call jelira(chin//'.LIMA', 'LONT', lontav2)
        if (present(lontav)) lontav=lontav2
    endif
!
    call jecroc(jexnum(chin//'.LIMA', nedit))
    ldim = max(dim,1)
    call jeecra(jexnum(chin//'.LIMA', nedit), 'LONMAX', ldim)
    call jeveuo(jexnum(chin//'.LIMA', nedit), 'E', jlima)
    do i = 1, dim
        if (mode2.eq.'NUM') then
!           --  mailles numerotees ( du maillage (>0) ou tardives(<0) )
            zi(jlima-1+i) = limanu(i)
        else if (mode2.eq.'NOM') then
!           --  mailles nommees du maillage
            nomail = limano(i)
            call jenonu(jexnom(ma//'.NOMMAI', nomail), numero)
            zi(jlima-1+i) = numero
        else
            ASSERT(.false.)
        endif
    end do
!
end subroutine
