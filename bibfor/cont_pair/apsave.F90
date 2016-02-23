subroutine apsave(vectap, resoco, nmactt)
   
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "jeveux.h"
#include "asterfort/wkvect.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jedetr.h"
#include "asterfort/assert.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=24) ::resoco
    integer :: nmactt
    integer, pointer :: vectap(:)
! ----------------------------------------------------------------------
!     RECUPERATION DES LISTES DE MAILLE ASSOCI A UNE ZONE DE CONTACT
! ----------------------------------------------------------------------
!   IN        VECTAP     VECTEUR D'APPARIEMENT
!   IN/OUT    RESOCO     SD RESOLUTION DU CONTACT   
! ----------------------------------------------------------------------
!
    integer :: ind1
    integer :: japlis
    character(len=24) :: aplist

! ----------------------------------------------------------------------
!
    call jemarq()
! --- Initialisation ---------------------------------------------------
!
    aplist=resoco(1:14)//'.APLIST'
    call jedetr(aplist)
    call wkvect(aplist,'V V I', 3*nmactt, japlis)
!    
    do ind1= 1, 3*nmactt
        zi(japlis+ind1-1)=vectap(ind1)        
    end do
        AS_DEALLOCATE(vi=vectap)
!
    call jedema()
end subroutine    
            