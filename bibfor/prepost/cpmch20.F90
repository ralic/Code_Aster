subroutine cpmch20(conloc, jmacsu, indno, indma, conneo)
!
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
        lino(9) = 11
        lino(10) = 10
        lino(11) = 9
        lino(12) = 12
        lino(13) = 16
        lino(14) = 15
        lino(15) = 14
        lino(16) = 13
        lino(17) = 19
        lino(18) = 18
        lino(19) = 17
        lino(20) = 20
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
        lino(9) = 9
        lino(10) = 14
        lino(11) = 17
        lino(12) = 13
        lino(13) = 12
        lino(14) = 10
        lino(15) = 18
        lino(16) = 20
        lino(17) = 11
        lino(18) = 15
        lino(19) = 19
        lino(20) =16
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
        lino(9) = 10
        lino(10) = 15
        lino(11) = 18
        lino(12) = 14
        lino(13) = 9
        lino(14) = 11
        lino(15) = 19
        lino(16) = 17
        lino(17) = 12
        lino(18) = 16
        lino(19) = 20
        lino(20) =13
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
        lino(9) = 11
        lino(10) = 16
        lino(11) = 19
        lino(12) = 15
        lino(13) = 10
        lino(14) = 12
        lino(15) = 20
        lino(16) = 18
        lino(17) = 9
        lino(18) = 13
        lino(19) = 17
        lino(20) = 14
       !write(*,*) '4'
! -------------------------------------------------------------------------------------------------
    elseif (conneo(1) .ne. 0 .and. conneo(5) .ne. 0 .and.&
            conneo(8) .ne. 0 .and. conneo(4) .ne. 0) then
!--------------------------------------------------------------------------------------------------
        lino(1) = 1
        lino(2) = 5
        lino(3) = 8
        lino(4) = 4
        lino(5) = 2
        lino(6) = 6
        lino(7) = 7
        lino(8) = 3
        lino(9) = 13
        lino(10) = 20
        lino(11) = 16
        lino(12) = 12
        lino(13) = 9
        lino(14) = 17
        lino(15) = 19
        lino(16) = 11
        lino(17) = 14
        lino(18) = 18
        lino(19) = 15
        lino(20) = 10
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
        lino(9) = 17
        lino(10) = 18
        lino(11) = 19
        lino(12) = 20
        lino(13) = 13
        lino(14) = 14
        lino(15) = 15
        lino(16) = 16
        lino(17) = 9
        lino(18) = 10
        lino(19) = 11
        lino(20) = 12
        !write(*,*) '6'
! -------------------------------------------------------------------------------------------------
    else
        ASSERT(.false.)
    endif
! -------------------------------------------------------------------------------------------------
    call jeecra(jexnum(conloc,indma), 'LONMAX', ival=20)
    call jeecra(jexnum(conloc,indma), 'LONUTI', ival=20)
    call jeveuo(jexnum(conloc,indma), 'E', jconloc)
    zi(jconloc+4-1) = zi(jmacsu+lino(1)-1)
    zi(jconloc+3-1) = zi(jmacsu+lino(2)-1)
    zi(jconloc+2-1) = indno+conneo(lino(2))-1
    zi(jconloc+1-1) = indno+conneo(lino(1))-1
    zi(jconloc+8-1) = zi(jmacsu+lino(5)-1)
    zi(jconloc+7-1) = zi(jmacsu+lino(6)-1)
    zi(jconloc+6-1) = indno+12+conneo(lino(2))-1
    zi(jconloc+5-1) = indno+12+conneo(lino(1))-1
    zi(jconloc+11-1) = zi(jmacsu+lino(9)-1)
    zi(jconloc+10-1) = indno+8+conneo(lino(2))-1
    zi(jconloc+9-1) = indno+conneo(lino(9))-1
    zi(jconloc+12-1) = indno+8+conneo(lino(1))-1
    zi(jconloc+16-1) = zi(jmacsu+lino(13)-1)
    zi(jconloc+15-1) = zi(jmacsu+lino(14)-1)
    zi(jconloc+14-1) = indno+20+conneo(lino(2))-1
    zi(jconloc+13-1) = indno+20+conneo(lino(1))-1
    zi(jconloc+19-1) = zi(jmacsu+lino(17)-1)
    zi(jconloc+18-1) = indno+24+conneo(lino(2))-1
    zi(jconloc+17-1) = indno+12+conneo(lino(9))-1
    zi(jconloc+20-1) = indno+24+conneo(lino(1))-1
! =================================================================================================
    call jeecra(jexnum(conloc,indma+1), 'LONMAX', ival=20)
    call jeecra(jexnum(conloc,indma+1), 'LONUTI', ival=20)
    call jeveuo(jexnum(conloc,indma+1), 'E', jconloc)
    zi(jconloc+4-1) = zi(jmacsu+lino(2)-1)
    zi(jconloc+3-1) = zi(jmacsu+lino(3)-1)
    zi(jconloc+2-1) = indno+conneo(lino(3))-1
    zi(jconloc+1-1) = indno+conneo(lino(2))-1
    zi(jconloc+8-1) = zi(jmacsu+lino(6)-1)
    zi(jconloc+7-1) = zi(jmacsu+lino(7)-1)
    zi(jconloc+6-1) = indno+12+conneo(lino(3))-1
    zi(jconloc+5-1) = indno+12+conneo(lino(2))-1
    zi(jconloc+11-1) = zi(jmacsu+lino(10)-1)
    zi(jconloc+10-1) = indno+8+conneo(lino(3))-1
    zi(jconloc+9-1) = indno+conneo(lino(10))-1
    zi(jconloc+12-1) = indno+8+conneo(lino(2))-1
    zi(jconloc+16-1) = zi(jmacsu+lino(14)-1)
    zi(jconloc+15-1) = zi(jmacsu+lino(15)-1)
    zi(jconloc+14-1) = indno+20+conneo(lino(3))-1
    zi(jconloc+13-1) = indno+20+conneo(lino(2))-1
    zi(jconloc+19-1) = zi(jmacsu+lino(18)-1)
    zi(jconloc+18-1) = indno+24+conneo(lino(3))-1
    zi(jconloc+17-1) = indno+12+conneo(lino(10))-1
    zi(jconloc+20-1) = indno+24+conneo(lino(2))-1
! =================================================================================================
    call jeecra(jexnum(conloc,indma+2), 'LONMAX', ival=20)
    call jeecra(jexnum(conloc,indma+2), 'LONUTI', ival=20)
    call jeveuo(jexnum(conloc,indma+2), 'E', jconloc)
    zi(jconloc+4-1) = zi(jmacsu+lino(3)-1)
    zi(jconloc+3-1) = zi(jmacsu+lino(4)-1)
    zi(jconloc+2-1) = indno+conneo(lino(4))-1
    zi(jconloc+1-1) = indno+conneo(lino(3))-1
    zi(jconloc+8-1) = zi(jmacsu+lino(7)-1)
    zi(jconloc+7-1) = zi(jmacsu+lino(8)-1)
    zi(jconloc+6-1) = indno+12+conneo(lino(4))-1
    zi(jconloc+5-1) = indno+12+conneo(lino(3))-1
    zi(jconloc+11-1) = zi(jmacsu+lino(11)-1)
    zi(jconloc+10-1) = indno+8+conneo(lino(4))-1
    zi(jconloc+9-1) = indno+conneo(lino(11))-1
    zi(jconloc+12-1) = indno+8+conneo(lino(3))-1
    zi(jconloc+16-1) = zi(jmacsu+lino(15)-1)
    zi(jconloc+15-1) = zi(jmacsu+lino(16)-1)
    zi(jconloc+14-1) = indno+20+conneo(lino(4))-1
    zi(jconloc+13-1) = indno+20+conneo(lino(3))-1
    zi(jconloc+19-1) = zi(jmacsu+lino(19)-1)
    zi(jconloc+18-1) = indno+24+conneo(lino(4))-1
    zi(jconloc+17-1) = indno+12+conneo(lino(11))-1
    zi(jconloc+20-1) = indno+24+conneo(lino(3))-1
! =================================================================================================
    call jeecra(jexnum(conloc,indma+3), 'LONMAX', ival=20)
    call jeecra(jexnum(conloc,indma+3), 'LONUTI', ival=20)
    call jeveuo(jexnum(conloc,indma+3), 'E', jconloc)
    zi(jconloc+4-1) = zi(jmacsu+lino(4)-1)
    zi(jconloc+3-1) = zi(jmacsu+lino(1)-1)
    zi(jconloc+2-1) = indno+conneo(lino(1))-1
    zi(jconloc+1-1) = indno+conneo(lino(4))-1
    zi(jconloc+8-1) = zi(jmacsu+lino(8)-1)
    zi(jconloc+7-1) = zi(jmacsu+lino(5)-1)
    zi(jconloc+6-1) = indno+12+conneo(lino(1))-1
    zi(jconloc+5-1) = indno+12+conneo(lino(4))-1
    zi(jconloc+11-1) = zi(jmacsu+lino(12)-1)
    zi(jconloc+10-1) = indno+8+conneo(lino(1))-1
    zi(jconloc+9-1) = indno+conneo(lino(12))-1
    zi(jconloc+12-1) = indno+8+conneo(lino(4))-1
    zi(jconloc+16-1) = zi(jmacsu+lino(16)-1)
    zi(jconloc+15-1) = zi(jmacsu+lino(13)-1)
    zi(jconloc+14-1) = indno+20+conneo(lino(1))-1
    zi(jconloc+13-1) = indno+20+conneo(lino(4))-1
    zi(jconloc+19-1) = zi(jmacsu+lino(20)-1)
    zi(jconloc+18-1) = indno+24+conneo(lino(1))-1
    zi(jconloc+17-1) = indno+12+conneo(lino(12))-1
    zi(jconloc+20-1) = indno+24+conneo(lino(4))-1
! =================================================================================================
    call jeecra(jexnum(conloc,indma+4), 'LONMAX', ival=20)
    call jeecra(jexnum(conloc,indma+4), 'LONUTI', ival=20)
    call jeveuo(jexnum(conloc,indma+4), 'E', jconloc)
    zi(jconloc+4-1) = indno+12+conneo(lino(1))-1
    zi(jconloc+3-1) = indno+12+conneo(lino(2))-1
    zi(jconloc+2-1) = indno+12+conneo(lino(3))-1
    zi(jconloc+1-1) = indno+12+conneo(lino(4))-1
    zi(jconloc+8-1) = zi(jmacsu+lino(5)-1)
    zi(jconloc+7-1) = zi(jmacsu+lino(6)-1)
    zi(jconloc+6-1) = zi(jmacsu+lino(7)-1)
    zi(jconloc+5-1) = zi(jmacsu+lino(8)-1)
    zi(jconloc+11-1) = indno+12+conneo(lino(9))-1
    zi(jconloc+10-1) = indno+12+conneo(lino(10))-1
    zi(jconloc+9-1) = indno+12+conneo(lino(11))-1
    zi(jconloc+12-1) = indno+12+conneo(lino(12))-1
    zi(jconloc+16-1) = indno+24+conneo(lino(1))-1
    zi(jconloc+15-1) = indno+24+conneo(lino(2))-1
    zi(jconloc+14-1) = indno+24+conneo(lino(3))-1
    zi(jconloc+13-1) = indno+24+conneo(lino(4))-1
    zi(jconloc+19-1) = zi(jmacsu+lino(17)-1)
    zi(jconloc+18-1) = zi(jmacsu+lino(18)-1)
    zi(jconloc+17-1) = zi(jmacsu+lino(19)-1)
    zi(jconloc+20-1) = zi(jmacsu+lino(20)-1)
! =================================================================================================
    call jeecra(jexnum(conloc,indma+5), 'LONMAX', ival=20)
    call jeecra(jexnum(conloc,indma+5), 'LONUTI', ival=20)
    call jeveuo(jexnum(conloc,indma+5), 'E', jconloc)
    zi(jconloc+4-1) = indno
    zi(jconloc+3-1) = indno+1
    zi(jconloc+2-1) = indno+2
    zi(jconloc+1-1) = indno+3
    zi(jconloc+8-1) = indno+12
    zi(jconloc+7-1) = indno+13
    zi(jconloc+6-1) = indno+14
    zi(jconloc+5-1) = indno+15
    zi(jconloc+11-1) = indno+4 
    zi(jconloc+10-1) = indno+5
    zi(jconloc+9-1) = indno+6
    zi(jconloc+12-1) = indno+7
    zi(jconloc+16-1) = indno+20
    zi(jconloc+15-1) = indno+21
    zi(jconloc+14-1) = indno+22
    zi(jconloc+13-1) = indno+23
    zi(jconloc+19-1) = indno+16
    zi(jconloc+18-1) = indno+17
    zi(jconloc+17-1) = indno+18
    zi(jconloc+20-1) = indno+19
! -------------------------------------------------------------------------------------------------
    call jedema()
end subroutine
