subroutine cnmpmc(main,nbma, lima,mpmc)
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
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/cncinv.h"
#include "asterfort/utmess.h"
#include "asterfort/utlisi.h"
#include "asterfort/jelira.h"
#include "asterfort/assert.h"
!
    integer :: nbma, lima(nbma), mpmc(nbma), inc1, inc2, aux, jlino
    integer :: jlmat, ntrou, nbnoma, dima, macou, nocou 
    integer :: jlico3, nlico1, lico1, nlico2, lico2, inc3, var(1), jlico4
    character(len=8) :: main 
    character(len=24) :: lmat, conneo, lico3, lico4
! ----------------------------------------------------------------------
!        CREATION DE LA CONNECTIVITÉ MAILLE DE PEAU MAILLE DE CORPS 
!                  (POUR LE GROUPE DE MAILLE LIMA)
! ----------------------------------------------------------------------
! IN        MAIN   K8  NOM DU MAILLAGE 
! IN        NBMA    I  NOMBRE DE MAILLE DE PEAU
! IN        LIMA    I  NUMERO DES MAILLES A TRAITER
! OUT       MPMC    I  CONNECTIVITÉ
! ----------------------------------------------------------------------
    
    
! ----------------------------------------------------------------------
!        CONSTRUCTION DE LA CONNECTIVITÉ INVERSE NOEUD MAILLE
! ----------------------------------------------------------------------
    call jeveuo(main//'.DIME', 'L', dima)
    aux = zi(dima-1+3)
    lmat='&&CNMPMC.LISTE_MA_TOT'
    conneo='&&CNMPMC.CONNEX_INV'
    call wkvect(lmat, 'V V I', zi(dima-1+3), jlmat)
    do inc1=1,aux
        zi(jlmat+inc1-1)=inc1
    end do 
    call cncinv(main, [0], 0,'V', conneo)
    
    
! ----------------------------------------------------------------------
!        CONSTRUCTION DE MPMC
! ----------------------------------------------------------------------
    do inc1 = 1,nbma
        macou=lima(inc1)
        call jelira(jexnum(main//'.CONNEX',macou),'LONMAX',nbnoma)
        call jeveuo(jexnum(main//'.CONNEX',macou),'L',jlino)
        nocou=zi(jlino+1-1)   
        call jelira(jexnum(conneo,nocou),'LONMAX',nlico1)
        ASSERT(nlico1.gt.1)
        call jeveuo(jexnum(conneo,nocou),'L',lico1)
        lico3='&&CNMPMC.LICO3'
        lico4='&&CNMPMC.LICO4'
        call wkvect(lico3, 'V V I', nlico1, jlico3)
        call wkvect(lico4, 'V V I', nlico1, jlico4)
        do inc3 = 1,nlico1
            zi(jlico3+inc3-1)=zi(lico1+inc3-1)
        end do
!
        do  inc2 = 2, nbnoma
                nocou=zi(jlino+inc2-1)
                call jelira(jexnum(conneo,nocou),'LONMAX',nlico2)

                call jeveuo(jexnum(conneo,nocou),'L',lico2)
                call utlisi('INTER',zi(jlico3),nlico1,zi(lico2),nlico2,&
                            zi(jlico4),nlico1,ntrou)
!
                do inc3 = 1,ntrou
                        zi(jlico3+inc3-1)=zi(jlico4+inc3-1)
                end do
                nlico1 = ntrou  
!
        end do
        if (ntrou .gt. 0) then
        call utlisi('DIFFE',zi(jlico3),nlico1,lima, nbma,&
                            var,1,ntrou)
        mpmc(inc1)=var(1)
        else
        call utmess('F', 'ALGELINE2_93')!à changer avec un message aproprié
        end if
        call jedetr(lico3)
        call jedetr(lico4)
    end do

    
! ---------------------------------------------------------------------
!     DESTRUCTION DES VECTEUR AUXILIAIRES
! ---------------------------------------------------------------------
   call jedetr(lmat)
   call jedetr(conneo)
   
end subroutine

