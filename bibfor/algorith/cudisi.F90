function cudisi(deficz, questz)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: cudisi
    character(len=*) :: deficz
    character(len=*) :: questz
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES)
!
! RETOURNE DES INFOS DIVERSES POUR LIAISON_UNIL (ENTIER)
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICU  : SD DE DEFINITION DE LIAISON_UNILATER (DEFI_CONTACT)
! IN  QUESTI  : QUESTION (PARAMETRE INTERROGE)
!
!
!
!
    character(len=24) :: ndimcu
    integer :: jdim
    character(len=24) :: deficu, questi
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    deficu = deficz
    questi = questz
!
! --- LECTURE DES STRUCTURES DE DONNEES
!
    ndimcu = deficu(1:16)//'.NDIMCU'
    if (questi .eq. 'NNOCU') then
        call jeveuo(ndimcu, 'L', jdim)
        cudisi = zi(jdim)
    else if (questi.eq.'NCMPG') then
        call jeveuo(ndimcu, 'L', jdim)
        cudisi = zi(jdim+1)
    else if (questi.eq.'NB_RESOL') then
        cudisi = 10
!
    else
        write(6,*) 'QUESTION: ',questi
        ASSERT(.false.)
    endif
!
    call jedema()
!
end function
