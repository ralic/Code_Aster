subroutine cpmcq8(conloc, jmacsu, indno, indma, conneo)
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
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
#include "asterfort/assert.h"

!
    integer, intent(in) :: indma
    integer, intent(in) :: indno
    integer, intent(in) :: jmacsu
    integer, intent(in) :: conneo(*)
    character(len=24), intent(in) :: conloc
!
! -------------------------------------------------------------------------------------------------
!         CREATION DES MAILLES DES NOUVELLES MAILLES DE PEAU 
!         SUR LA FACE DE LA ZONE DE CONTACT ESCLAVE
!         CAS QUAD 8
! -------------------------------------------------------------------------------------------------
! IN        CONLOC  CONNECTIVITE LOCALE
! IN        NUMA    NUMERO DE LA MAILLE COURANTE
! IN        INDNO   INDICE DU PREMIER NOEUD AJOUTE
! IN        INDMA   INDICE DE LA PREMIERE MAILLE AJOUTEE
! -------------------------------------------------------------------------------------------------
    integer :: lino(8), jconloc
! -------------------------------------------------------------------------------------------------
    call jemarq()
! -------------------------------------------------------------------------------------------------
    if (conneo(1) .ne. 0 .and. conneo(2) .ne. 0 .and. conneo(3) .ne. 0 .and. conneo(4) .ne. 0) then
! -------------------------------------------------------------------------------------------------
        lino(1) = 4
        lino(2) = 3
        lino(3) = 2
        lino(4) = 1
        lino(5) = 8
        lino(6) = 7
        lino(7) = 6
        lino(8) = 5
        !write(*,*) '1'
! -------------------------------------------------------------------------------------------------
    elseif (conneo(1) .ne. 0 .and. conneo(2) .ne. 0 .and.&
            conneo(6) .ne. 0 .and. conneo(5) .ne. 0) then
!--------------------------------------------------------------------------------------------------
        lino(1) = 1
        lino(2) = 2
        lino(3) = 6
        lino(4) = 5
        lino(5) = 4
        lino(6) = 3
        lino(7) = 7
        lino(8) = 8
        !write(*,*) '2'
! -------------------------------------------------------------------------------------------------
    elseif (conneo(2) .ne. 0 .and. conneo(3) .ne. 0 .and.&
            conneo(7) .ne. 0 .and. conneo(6) .ne. 0) then
!--------------------------------------------------------------------------------------------------
        lino(1) = 2
        lino(2) = 3
        lino(3) = 7
        lino(4) = 6
        lino(5) = 1
        lino(6) = 4
        lino(7) = 8
        lino(8) = 5
       !write(*,*) '3'
! -------------------------------------------------------------------------------------------------
    elseif (conneo(3) .ne. 0 .and. conneo(4) .ne. 0 .and.&
            conneo(8) .ne. 0 .and. conneo(7) .ne. 0) then
!--------------------------------------------------------------------------------------------------
        lino(1) = 3
        lino(2) = 4
        lino(3) = 8
        lino(4) = 7
        lino(5) = 2
        lino(6) = 1
        lino(7) = 5
        lino(8) = 6
        !write(*,*) '4'
! -------------------------------------------------------------------------------------------------
    elseif (conneo(4) .ne. 0 .and. conneo(1) .ne. 0 .and.&
            conneo(5) .ne. 0 .and. conneo(8) .ne. 0) then
!--------------------------------------------------------------------------------------------------
        lino(1) = 4
        lino(2) = 1
        lino(3) = 5
        lino(4) = 8
        lino(5) = 3
        lino(6) = 2
        lino(7) = 6
        lino(8) = 7
        !write(*,*) '5'
! -------------------------------------------------------------------------------------------------
    elseif (conneo(5) .ne. 0 .and. conneo(6) .ne. 0 .and.&
            conneo(7) .ne. 0 .and. conneo(8) .ne. 0) then
!--------------------------------------------------------------------------------------------------
        lino(1) = 5
        lino(2) = 6
        lino(3) = 7
        lino(4) = 8
        lino(5) = 1
        lino(6) = 2
        lino(7) = 3
        lino(8) = 4
        !write(*,*) '6'
! -------------------------------------------------------------------------------------------------
    else
        ASSERT(.false.)
    endif
! -------------------------------------------------------------------------------------------------
    call jeecra(jexnum(conloc,indma), 'LONMAX', ival=8)
    call jeecra(jexnum(conloc,indma), 'LONUTI', ival=8)
    call jeveuo(jexnum(conloc,indma), 'E', jconloc)
    zi(jconloc+4-1) =zi(jmacsu+lino(1)-1)
    zi(jconloc+3-1) =zi(jmacsu+lino(2)-1)
    zi(jconloc+2-1) = indno+conneo(lino(2))-1
    zi(jconloc+1-1) = indno+conneo(lino(1))-1
    zi(jconloc+8-1) = zi(jmacsu+lino(5)-1)
    zi(jconloc+7-1) = zi(jmacsu+lino(6)-1)
    zi(jconloc+6-1) = indno+4+conneo(lino(2))-1
    zi(jconloc+5-1) = indno+4+conneo(lino(1))-1
    call jeecra(jexnum(conloc,indma+1), 'LONMAX', ival=8)
    call jeecra(jexnum(conloc,indma+1), 'LONUTI', ival=8)
    call jeveuo(jexnum(conloc,indma+1), 'E', jconloc)
    zi(jconloc+4-1) = zi(jmacsu+lino(2)-1)
    zi(jconloc+3-1) = zi(jmacsu+lino(3)-1)
    zi(jconloc+2-1) = indno+conneo(lino(3))-1
    zi(jconloc+1-1) = indno+conneo(lino(2))-1
    zi(jconloc+8-1) = zi(jmacsu+lino(6)-1)
    zi(jconloc+7-1) = zi(jmacsu+lino(7)-1)
    zi(jconloc+6-1) = indno+4+conneo(lino(3))-1
    zi(jconloc+5-1) = indno+4+conneo(lino(2))-1
    call jeecra(jexnum(conloc,indma+2), 'LONMAX', ival=8)
    call jeecra(jexnum(conloc,indma+2), 'LONUTI', ival=8)
    call jeveuo(jexnum(conloc,indma+2), 'E', jconloc)
    zi(jconloc+4-1) = zi(jmacsu+lino(3)-1)
    zi(jconloc+3-1) = zi(jmacsu+lino(4)-1)
    zi(jconloc+2-1) = indno+conneo(lino(4))-1
    zi(jconloc+1-1) = indno+conneo(lino(3))-1
    zi(jconloc+8-1) = zi(jmacsu+lino(7)-1)
    zi(jconloc+7-1) = zi(jmacsu+lino(8)-1)
    zi(jconloc+6-1) = indno+4+conneo(lino(4))-1
    zi(jconloc+5-1) = indno+4+conneo(lino(3))-1
    call jeecra(jexnum(conloc,indma+3), 'LONMAX', ival=8)
    call jeecra(jexnum(conloc,indma+3), 'LONUTI', ival=8)
    call jeveuo(jexnum(conloc,indma+3), 'E', jconloc)
    zi(jconloc+4-1) = zi(jmacsu+lino(4)-1)
    zi(jconloc+3-1) = zi(jmacsu+lino(1)-1)
    zi(jconloc+2-1) = indno+conneo(lino(1))-1
    zi(jconloc+1-1) = indno+conneo(lino(4))-1
    zi(jconloc+8-1) = zi(jmacsu+lino(8)-1)
    zi(jconloc+7-1) = zi(jmacsu+lino(5)-1)
    zi(jconloc+6-1) = indno+4+conneo(lino(1))-1
    zi(jconloc+5-1) = indno+4+conneo(lino(4))-1
    call jeecra(jexnum(conloc,indma+4), 'LONMAX', ival=8)
    call jeecra(jexnum(conloc,indma+4), 'LONUTI', ival=8)
    call jeveuo(jexnum(conloc,indma+4), 'E', jconloc)
    zi(jconloc+4-1) = indno+4+conneo(lino(1))-1
    zi(jconloc+3-1) = indno+4+conneo(lino(2))-1
    zi(jconloc+2-1) = indno+4+conneo(lino(3))-1
    zi(jconloc+1-1) = indno+4+conneo(lino(4))-1
    zi(jconloc+8-1) = zi(jmacsu+lino(5)-1)
    zi(jconloc+7-1) = zi(jmacsu+lino(6)-1)
    zi(jconloc+6-1) = zi(jmacsu+lino(7)-1)
    zi(jconloc+5-1) = zi(jmacsu+lino(8)-1)
    call jeecra(jexnum(conloc,indma+5), 'LONMAX', ival=8)
    call jeecra(jexnum(conloc,indma+5), 'LONUTI', ival=8)
    call jeveuo(jexnum(conloc,indma+5), 'E', jconloc)
    zi(jconloc+4-1) = indno
    zi(jconloc+3-1) = indno+1
    zi(jconloc+2-1) = indno+2
    zi(jconloc+1-1) = indno+3
    zi(jconloc+8-1) = indno+4
    zi(jconloc+7-1) = indno+5
    zi(jconloc+6-1) = indno+6
    zi(jconloc+5-1) = indno+7
! -------------------------------------------------------------------------------------------------
    call jedema()
end subroutine
