subroutine jeccta(colle1)
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
    character(len=*) :: colle1
!     ------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
!     RETASSAGE D'UNE COLLECTION CONTIGUE ALLOUEE TROP GRANDE
!     ------------------------------------------------------------------
! IN  COLLE1  : K24 : NOM DE LA COLLECTION CONTIGUE A RETASSER
!     ------------------------------------------------------------------
!
    character(len=8) :: base, type, acces, stock, modelo
    character(len=24) :: colle2
    integer :: k, nbobj, lont1, lont2, jloncu
    integer :: n1, jcoll1, jcoll2
!-----------------------------------------------------------------------
    call jemarq()
!
!
!     -- CALCUL DE :
!        BASE, TYPE, NBOBJ,
!        ACCES, STOCK, MODELO, LONT1 ET LONT2 :
!     -------------------------------------------------------
    call jelira(colle1, 'CLAS', cval=base)
    call jelira(colle1, 'TYPE', cval=type)
    call jelira(colle1, 'NMAXOC', nbobj)
    call jelira(colle1, 'ACCES', cval=acces)
    call jelira(colle1, 'MODELONG', cval=modelo)
    call jelira(colle1, 'STOCKAGE', cval=stock)
    call jelira(colle1, 'LONT', lont1)
    ASSERT(type.ne.'K')
    ASSERT(stock.eq.'CONTIG')
    ASSERT(modelo.eq.'VARIABLE')
    ASSERT(acces.eq.'NU')
!
    call jeveuo(jexatr(colle1, 'LONCUM'), 'L', jloncu)
    lont2=zi(jloncu-1+nbobj+1)
!
!
!     -- ALLOCATION ET REMPLISAGE DE COLLE2 :
!     ------------------------------------------------
    colle2='&&JECCTA.COLLEC2'
    call jecrec(colle2, 'V V '//type, acces, stock, modelo,&
                nbobj)
    call jeecra(colle2, 'LONT', lont2)
    call jeveuo(colle1, 'L', jcoll1)
    call jeveuo(colle2, 'E', jcoll2)
    if (type .eq. 'I') then
        do 10,k=1,lont2
        zi(jcoll2-1+k)=zi(jcoll1-1+k)
10      continue
    else if (type.eq.'R') then
        do 20,k=1,lont2
        zr(jcoll2-1+k)=zr(jcoll1-1+k)
20      continue
    else if (type.eq.'C') then
        do 30,k=1,lont2
        zc(jcoll2-1+k)=zc(jcoll1-1+k)
30      continue
    else
        ASSERT(.false.)
    endif
!
    do 40,k=1,nbobj
    n1=zi(jloncu-1+k+1)-zi(jloncu-1+k)
    call jecroc(jexnum(colle2, k))
    call jeecra(jexnum(colle2, k), 'LONMAX', n1)
    40 end do
!
!
!     RECOPIE DE COLLE2 DANS COLLE1 :
!     ------------------------------
    call jedetr(colle1)
    call jedupo(colle2, base, colle1, .false.)
!
!
!     -- MENAGE :
    call jedetr(colle2)
    call jedema()
end subroutine
