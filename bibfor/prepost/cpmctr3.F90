subroutine cpmctr3(conloc, jmacsu, indno, indma, conneo)
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
!         CREATION DES MAILLES DES NOUVELLES MAILLES VOLUMIQUE 
!         ASSOCIEE A LA ZONE DE CONTACT ESCLAVE
!         CAS QUAD 20
! -------------------------------------------------------------------------------------------------
! IN        CONLOC  CONNECTIVITE LOCALE
! IN        NUMA    NUMERO DE LA MAILLE COURANTE
! IN        INDNO   INDICE DU PREMIER NOEUD AJOUTE
! IN        INDMA   INDICE DE LA PREMIERE MAILLE AJOUTEE
! -------------------------------------------------------------------------------------------------
    integer :: lino(20), jconloc
! -------------------------------------------------------------------------------------------------
    call jemarq()
! -------------------------------------------------------------------------------------------------
    if (conneo(1) .ne. 0 .and. conneo(2) .ne. 0) then
! -------------------------------------------------------------------------------------------------
        lino(1) = 1
        lino(2) = 2
        lino(3) = 3
        !write(*,*) '1'
! -------------------------------------------------------------------------------------------------
    elseif (conneo(2) .ne. 0 .and. conneo(3) .ne. 0 ) then
!--------------------------------------------------------------------------------------------------
        lino(1) = 2
        lino(2) = 3
        lino(3) = 1
        !write(*,*) '2'
! -------------------------------------------------------------------------------------------------
    elseif (conneo(3) .ne. 0 .and. conneo(1) .ne. 0) then
!--------------------------------------------------------------------------------------------------
        lino(1) = 3
        lino(2) = 1
        lino(3) = 2
        !write(*,*) '3'

! -------------------------------------------------------------------------------------------------
    else
        ASSERT(.false.)
    endif
! -------------------------------------------------------------------------------------------------
    call jeecra(jexnum(conloc,indma), 'LONMAX', ival=3)
    call jeecra(jexnum(conloc,indma), 'LONUTI', ival=3)
    call jeveuo(jexnum(conloc,indma), 'E', jconloc)
    zi(jconloc+1-1) = zi(jmacsu+lino(1)-1)
    zi(jconloc+2-1) = indno
    zi(jconloc+3-1) = zi(jmacsu+lino(3)-1)
   
! =================================================================================================
    call jeecra(jexnum(conloc,indma+1), 'LONMAX', ival=3)
    call jeecra(jexnum(conloc,indma+1), 'LONUTI', ival=3)
    call jeveuo(jexnum(conloc,indma+1), 'E', jconloc)
    zi(jconloc+1-1) = indno
    zi(jconloc+2-1) = zi(jmacsu+lino(2)-1)
    zi(jconloc+3-1) = zi(jmacsu+lino(3)-1)
   

! -------------------------------------------------------------------------------------------------
    call jedema()
end subroutine
