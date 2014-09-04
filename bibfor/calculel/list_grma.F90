subroutine list_grma(mailla,ima,n1,lgrma,nbgrma)
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/assert.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/jexnom.h"
!

    character(len=8), intent(in) :: mailla
    integer, intent(in) :: ima
    integer, intent(in) :: n1
    character(len=*), intent(out) :: lgrma(n1)
    integer, intent(out) :: nbgrma
!
!-----------------------------------------------------------------------
!   But :
!     retourner la liste des noms des GROUP_MA qui contiennent la maille ima
!
!   Entrees:
!     mailla     :  nom du maillage
!     ima        :  numero de la maille cherchee
!     n1         :  dimension du vecteur lgrma
!   Sorties:
!     lgrma      :  liste des noms des GROUP_MA
!     nbgrma     :  nombre de GROUP_MA renseignes  (<=n1)
!
!-----------------------------------------------------------------------
    character(len=24) :: nomgrma
    integer :: nbgroup,igrma,jgrma,nbma,kma,iexi

!-----------------------------------------------------------------------
!
    call jemarq()


    nbgrma=0
    call jeexin(mailla//'.GROUPEMA',iexi)
    if (iexi.eq.0) goto 999

    call jelira(mailla//'.GROUPEMA','NUTIOC',nbgroup)
    do igrma=1,nbgroup
       call jeveuo(jexnum(mailla//'.GROUPEMA', igrma), 'L',jgrma)
       call jelira(jexnum(mailla//'.GROUPEMA', igrma), 'LONMAX',nbma)
       do kma=1,nbma
           if (zi(jgrma-1+kma).eq.ima) then
               call jenuno(jexnum(mailla//'.PTRNOMMAI', igrma), nomgrma)
               nbgrma=nbgrma+1
               lgrma(nbgrma)=nomgrma
               goto 1
           endif
       enddo
1      continue
    enddo

999 continue
    call jedema()
end subroutine
