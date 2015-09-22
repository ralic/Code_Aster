subroutine cnpc(main, macou, macsu, conneo)
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
#include "asterfort/codent.h"
#include "asterfort/jecroc.h"
#include "asterfort/jeexin.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utlisi.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/wkvect.h"
!
    integer :: macou, macsu
    character(len=8) :: main
    character(len=24) :: conneo
!
!
! ----------------------------------------------------------------------
!        CONNECTIVITE DE L'ORDRE DES NOEUDS ENTRE MAILLE DE PEAU
!        ET MAILLE VOLUMIQUE CORRESPONDANTE 
! ----------------------------------------------------------------------
! IN        MAIN    MAILLAGE
! IN        MACOU   NUMERO DE LA MAILLE COURANTE
! IN        MACSU   NUMERO DE LA MAILLE VOLUMIQUE CORESPONDANTE
! OUT       CONNEO  CONNECTIVIT2 DES ORDRE DES NOEUDS
! ----------------------------------------------------------------------
!
    integer :: inc1, inc2, nbno1, nbno2, ntrou
    integer :: jmacsu, jmacou, jconneo, varaux(1)

! ----------------------------------------------------------------------
!
    call jemarq()    
!
    call jeveuo(jexnum(main//'.CONNEX',macsu),'L',jmacsu)
    call jeveuo(jexnum(main//'.CONNEX',macou),'L',jmacou)
    call jelira(jexnum(main//'.CONNEX',macou),'LONMAX',nbno1)
    call jelira(jexnum(main//'.CONNEX',macsu),'LONMAX',nbno2)
    call wkvect(conneo, 'V V I', nbno2, jconneo )
    do inc1= 1, nbno2
        zi(jconneo+inc1-1)=0
        do inc2=1,nbno1
            call utlisi('INTER',zi(jmacsu+inc1-1),1,zi(jmacou+inc2-1),1,&
                        varaux,1,ntrou)
            if (ntrou .eq. 1) then
                zi(jconneo+inc1-1) = inc2
            end if
        end do
    end do
!
    call jedema()
!
! rajouter un test d'erreur
!
end subroutine    

