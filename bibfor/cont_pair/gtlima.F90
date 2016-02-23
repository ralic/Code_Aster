subroutine gtlima(defico, izone, nbmma, nbmes, limama, limaes)
   
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
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
#include "asterfort/cfzone.h"
#include "asterfort/cfnbsf.h"
#include "asterfort/cfnumm.h"
!
    integer :: nbmes, nbmma
    integer :: izone
    character(len=24) :: limama, limaes, defico
! ----------------------------------------------------------------------
!     RECUPERATION DES LISTES DE MAILLE ASSOCI A UNE ZONE DE CONTACT
! ----------------------------------------------------------------------
!   IN        MAIL       MAILLAGE
!   IN        DEFICO     SD DEFINITION CONTACT
!   IN        IZONE      NUMERO ZONE DE CONTACT
!   OUT       NBMES      NOMBRE DE MAILLES ESCLAVES
!   OUT       NBMMA      NOMBRE DE MAILLES MAITRES   
!   OUT       LIMAMA     LISTE DES MAILLES MAITRES
!   OUT       LIMAES     LISTE DES MAILLES ESCLAVES      
! ----------------------------------------------------------------------
!
    integer :: nsurfe, nsurfm, jcmmal, jcmesl
    integer :: jcmama, jcmaes, jmaco 
    integer :: ind
    character(len=24) :: contma

! ----------------------------------------------------------------------
!
    call jemarq()
! --- Recuperation des numeros de surface locaux (DEFICO) --------------    
!
    call cfzone(defico, izone, 'MAIT', nsurfm)
    call cfzone(defico, izone, 'ESCL', nsurfe)
! --- liste maille locale ----------------------------------------------
!
    call cfnbsf(defico, nsurfm, 'MAIL', nbmma, jcmmal)
    call cfnbsf(defico, nsurfe, 'MAIL', nbmes, jcmesl)
! --- Liste maille numerotation globale --------------------------------
!
    call wkvect(limama, 'V V I', nbmma, jcmama)
    call wkvect(limaes, 'V V I', nbmes, jcmaes)
    contma = defico(1:16)//'.MAILCO'
    call jeveuo(contma, 'L', jmaco)
    do ind=1, nbmma
        zi(jcmama+ind-1) = zi(jmaco+jcmmal+ind-1)
    end do
    do ind=1, nbmes    
        zi(jcmaes+ind-1) = zi(jmaco+jcmesl+ind-1)
    end do
!
    call jedema()
end subroutine    
    