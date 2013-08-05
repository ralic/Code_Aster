function cudisd(resocu, questz)
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
    implicit none
    integer :: cudisd
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resocu
    character(len=*) :: questz
!
! ----------------------------------------------------------------------
!
! ROUTINE LIAISON_UNILATER (UTILITAIRE)
!
! LECTURE INFORMATIONS SUR L'ETAT DES LIAISONS
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCU  : SD DE RESOLUTION DU CONTACT
! IN  QUESTI  : VALEUR A LIRE/ECRIRE
!   NBLIAC : NOMBRE DE LIAISON DE CONTACT ACTIVES
!
!
!
!
    character(len=24) :: questi
    character(len=24) :: coco
    integer :: jcoco
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    questi = questz
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    coco = resocu(1:14)//'.COCO'
    call jeveuo(coco, 'L', jcoco)
!
    if (questi .eq. 'NBLIAC') then
        cudisd = zi(jcoco+3-1)
    else
        ASSERT(.false.)
    endif
!
    call jedema()
end function
