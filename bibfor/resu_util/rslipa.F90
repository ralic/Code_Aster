subroutine rslipa(nomsd, nopara, nomobj, jadd, nbval)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsadpa.h"
#include "asterfort/wkvect.h"
!
    integer :: jadd, nbval, n1, j1
    character(len=*) :: nomsd, nopara, nomobj
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
! ----------------------------------------------------------------------
!   extraire d'une sd_resultat, la liste des valeurs d'un parametre
!   et recopier ces valeurs dans l'objet nomobj dont on rend l'adresse.
! ----------------------------------------------------------------------
! in  : nomsd  : nom de la structure "resultat".
! in  : nopara : nom du parametre ('inst','freq', ...)
! in  : nomobj : nom de l'objet jeveux a creer (k24)
! out : jadd   : adresse de l'objet nomobj
! out : nbval  : longueur de l'objet nomobj
!-----------------------------------------------------------------------
! remarques :
!  - l'objet retourne (nomobj) contient les valeurs du parametre dans
!    l'ordre des numeros de rangement.
!    il est "parallele" a l'objet .ordr :
!    do k=1,lonuti(.ordr) :
!       iordr=.ordr(k)
!       nomobj(k) == "rsadpa(nopara,iordr)"
!  - si le parametre est absent pour certains numeros d'ordre, la valeur
!    stockee est "NaN".
!  - cette routine ne fait pas jemarq/jedema pour ne pas
!    invalider l'adresse jeveux jadd
! ----------------------------------------------------------------------
    integer :: kk,  jpara, i1, jtava, l1
    character(len=8) :: k8b, tsca
    character(len=5) :: nom1
    character(len=24) :: nomk24
    character(len=16) :: nompar
    character(len=19) :: noms2
    integer, pointer :: ordr(:) => null()
! ----------------------------------------------------------------------
!
    noms2 = nomsd
    nompar= nopara
    nomk24 = nomobj
!
    call jenonu(jexnom(noms2//'.NOVA', nompar), i1)
    ASSERT(i1.gt.0)
    call jeveuo(jexnum(noms2//'.TAVA', i1), 'L', jtava)
    nom1 = zk8(jtava-1+1)(1:5)
    call jelira(noms2//nom1, 'TYPE', cval=tsca)
    if (tsca .eq. 'K') then
        call jelira(noms2//nom1, 'LTYP', l1)
        if (l1 .eq. 8) then
            tsca='K8'
        else if (l1.eq.16) then
            tsca='K16'
        else if (l1.eq.24) then
            tsca='K24'
        else if (l1.eq.32) then
            tsca='K32'
        else if (l1.eq.80) then
            tsca='K80'
        else
            ASSERT(.false.)
        endif
    endif
!
    call jeveuo(noms2//'.ORDR', 'L', vi=ordr)
    call jelira(noms2//'.ORDR', 'LONUTI', n1)
!
    call wkvect(nomk24, 'V V '//tsca, n1, j1)
!
    do kk = 1, n1
        call rsadpa(noms2, 'L', 1, nompar, ordr(kk),&
                    0, sjv=jpara, styp=k8b, istop=0)
        if (tsca .eq. 'R') then
            zr(j1-1+kk)=zr(jpara)
        else if (tsca.eq.'C') then
            zc(j1-1+kk)=zc(jpara)
        else if (tsca.eq.'I') then
            zi(j1-1+kk)=zi(jpara)
        else if (tsca.eq.'K8') then
            zk8(j1-1+kk)=zk8(jpara)
        else if (tsca.eq.'K16') then
            zk16(j1-1+kk)=zk16(jpara)
        else if (tsca.eq.'K24') then
            zk24(j1-1+kk)=zk24(jpara)
        else if (tsca.eq.'K32') then
            zk32(j1-1+kk)=zk32(jpara)
        else if (tsca.eq.'K80') then
            zk80(j1-1+kk)=zk80(jpara)
        else
            ASSERT(.false.)
        endif
    end do
!
!     -- pour eviter les effets de bord (,ibid,ibid):
    jadd=j1
    nbval=n1
!
end subroutine
