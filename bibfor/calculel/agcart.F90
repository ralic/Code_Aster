subroutine agcart(ngdmxn, chinz)
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
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/juveca.h"
#include "asterfort/nbec.h"
#include "asterfort/wkvect.h"
    integer :: ngdmxn
    character(len=19) :: chin
    character(len=*) :: chinz
! --------------------------------------------------------------------
! Agrandissement de la carte chin, ngdmxn etant le nouveau nombre
!   maximum de couples (entite,valeur) a stocker
!
!   Attention : agcart n'agrandit pas l'objet .lima
!               celui-ci est agrandi directement par nocart
!     il est donc dangereux d'appeler agcart en dehors de nocart
! --------------------------------------------------------------------
!  ngdmxn       - in     - i    - : nouveau nombre max de couples
!               -        -      -   (entite,valeur) a stocker
! --------------------------------------------------------------------
!  chinz        - in     - k*(*)- : nom de la carte a redimensionner -
!               - jxvar  -      -   on realloue et on recopie leurs
!               -        -      -   anciennes valeurs pour les objets-
!               -        -      -   chin.desc
!               -        -      -   chin.vale
!               -        -      -   chin.noma
!               -        -      -   chin.noli
! --------------------------------------------------------------------
    character(len=1) :: base
    character(len=24) :: descav
    integer :: jdesca, jdesc, nec, iec, ngdmxa, nedit, ied, ideca, idec
    integer ::  ncmp, igd
! ----------------------------------------------------------------------
    call jemarq()
    chin = chinz
    call jelira(chin//'.DESC', 'CLAS', cval=base)
!
!
! --- AGRANDISSEMENT DE .DESC:
! ------------------------------
    descav='&&AGCART.DESCAV'
    call jedupo(chin//'.DESC', 'V', descav, .false._1)
    call jeveuo(descav, 'E', jdesca)
    igd = zi(jdesca-1+1)
    nec = nbec(igd)
    ngdmxa = zi(jdesca-1+2)
    nedit = zi(jdesca-1+3)
    ASSERT(ngdmxn.gt.ngdmxa)
!
    call jedetr(chin//'.DESC')
    call wkvect(chin//'.DESC', base//' V I', 3+ngdmxn*(2+nec), jdesc)
    call jeecra(chin//'.DESC', 'DOCU', cval='CART')
!
    zi(jdesc-1+1)=igd
    zi(jdesc-1+2)=ngdmxn
    zi(jdesc-1+3)=nedit
!
    do ied=1,nedit
        zi(jdesc-1+3+(ied-1)*2+1)=zi(jdesca-1+3+(ied-1)*2+1)
        zi(jdesc-1+3+(ied-1)*2+2)=zi(jdesca-1+3+(ied-1)*2+2)
    enddo
!
    do ied=1,nedit
        ideca=3+2*ngdmxa + nec*(ied-1)
        idec =3+2*ngdmxn + nec*(ied-1)
        do iec=1,nec
            zi(jdesc-1+idec+iec)=zi(jdesca-1+ideca+iec)
        enddo
    enddo
    call jedetr(descav)
!
!
!
! ---  AGRANDISSEMENT DE VALE:
! ------------------------------
    call jelira(jexnum('&CATA.GD.NOMCMP', igd), 'LONMAX', ncmp)
    call juveca(chin//'.VALE', ngdmxn*ncmp)
!
!
! ---  AGRANDISSEMENT DE NOLI
! ------------------------------
    call juveca(chin//'.NOLI', ngdmxn)
!
!
! ---  AGRANDISSEMENT DE LIMA : ON NE FAIT RIEN :
!      C'EST NOCART QUI AGRANDIT .LIMA SI NECESSAIRE
!
!
    call jedema()
!
end subroutine
