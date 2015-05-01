subroutine tresu_mail(nommai, tbtxt, refi, iocc,&
                     epsi, crit, llab, ssigne)
    implicit none
#include "asterf_types.h"
#include "asterfort/getvtx.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/tresu_print.h"
#include "asterfort/utmess.h"
#include "asterfort/assert.h"

    character(len=8), intent(in) :: nommai
    character(len=16), intent(in) :: tbtxt(2)
    integer, intent(in) :: refi
    integer, intent(in) :: iocc
    real(kind=8), intent(in) :: epsi
    character(len=*), intent(in) :: crit
    aster_logical, intent(in) :: llab
    character(len=*), intent(in) :: ssigne
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     entrees:
!        nommai : nom du maillage que l'on veut tester
!        tbtxt  : (1) : reference
!                 (2) : legende
!        refi   : valeur entiere attendue pour l'objet
!        crit   : 'RELATIF' ou 'ABSOLU'(precision relative ou absolue).
!        epsi   : precision esperee
!        llab   : flag d impression des labels
!     sorties:
!      listing ...
! ----------------------------------------------------------------------
!    -- variables locales:
    integer ::  n1, nb1, iexi, igr
    character(len=16) :: cara
    character(len=24) :: nomgr
! ----------------------------------------------------------------------

    call getvtx('MAILLAGE', 'CARA', iocc=iocc, scal=cara, nbret=n1)
    ASSERT(n1.eq.1)


!   -- calcul de la valeur (entiere) a tester : nb1
!   ------------------------------------------------
    if (cara.eq.'NB_MAILLE') then
       call jeexin(nommai//'.NOMMAI', iexi)
       if (iexi.eq.0) then
          nb1=0
          goto 100
       endif
       call jelira(nommai//'.NOMMAI', 'NOMMAX', nb1)

    elseif (cara.eq.'NB_NOEUD') then
       call jeexin(nommai//'.NOMNOE', iexi)
       if (iexi.eq.0) then
          nb1=0
          goto 100
       endif
       call jelira(nommai//'.NOMNOE', 'NOMMAX', nb1)

    elseif (cara.eq.'NB_GROUP_MA') then
       call jeexin(nommai//'.GROUPEMA', iexi)
       if (iexi.eq.0) then
          nb1=0
          goto 100
       endif
       call jelira(nommai//'.GROUPEMA', 'NMAXOC', nb1)

    elseif (cara.eq.'NB_GROUP_NO') then
       call jeexin(nommai//'.GROUPENO', iexi)
       if (iexi.eq.0) then
          nb1=0
          goto 100
       endif
       call jelira(nommai//'.GROUPENO', 'NMAXOC', nb1)

    elseif (cara.eq.'NB_MA_GROUP_MA') then
       nb1=0
       call jeexin(nommai//'.GROUPEMA', iexi)
       if (iexi.eq.0)  goto 100

       call getvtx('MAILLAGE', 'NOM_GROUP_MA', iocc=iocc, scal=nomgr, nbret=n1)
       ASSERT(n1.eq.1)
       call jenonu(jexnom(nommai//'.PTRNOMMAI', nomgr),igr)
       if (igr.eq.0)  goto 100

       call jelira(jexnum(nommai//'.GROUPEMA', igr),'LONMAX',nb1)

    elseif (cara.eq.'NB_NO_GROUP_NO') then
       nb1=0
       call jeexin(nommai//'.GROUPENO', iexi)
       if (iexi.eq.0) goto 100

       call getvtx('MAILLAGE', 'NOM_GROUP_NO', iocc=iocc, scal=nomgr, nbret=n1)
       ASSERT(n1.eq.1)
       call jenonu(jexnom(nommai//'.PTRNOMNOE', nomgr),igr)
       if (igr.eq.0)  goto 100

       call jelira(jexnum(nommai//'.GROUPENO', igr),'LONMAX',nb1)

    else
       ASSERT(.false.)
    endif



!   -- test de la valeur recuperee :
!   ---------------------------------
100 continue

    call tresu_print(tbtxt(1), tbtxt(2), llab, 1, crit,&
                     epsi, ssigne, refi=[refi], vali=nb1)

end subroutine
