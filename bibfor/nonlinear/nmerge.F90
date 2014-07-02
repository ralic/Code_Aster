subroutine nmerge(sderro, nomevt, lactiv)
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: sderro
    character(len=9) :: nomevt
    aster_logical :: lactiv
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (SD ERREUR)
!
! DIT SI UN EVENEMENT EST DECLENCHE
!
! ----------------------------------------------------------------------
!
!
! IN  SDERRO : SD GESTION DES ERREURS
! IN  NOMEVT : NOM DE L'EVENEMENT (VOIR LA LISTE DANS NMCRER)
! OUT LACTIV : .TRUE. SI EVENEMENT ACTIVE
!
!
!
!
    integer :: zeven
    integer :: ieven, icode
    character(len=24) :: errinf
    integer :: jeinf
    character(len=24) :: erreno, erraac
    integer :: jeenom, jeeact
    character(len=16) :: neven
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    lactiv = .false.
!
! --- ACCES SD
!
    errinf = sderro(1:19)//'.INFO'
    call jeveuo(errinf, 'L', jeinf)
    zeven = zi(jeinf-1+1)
!
    erreno = sderro(1:19)//'.ENOM'
    erraac = sderro(1:19)//'.EACT'
    call jeveuo(erreno, 'L', jeenom)
    call jeveuo(erraac, 'L', jeeact)
!
    do 15 ieven = 1, zeven
        neven = zk16(jeenom-1+ieven)
        if (neven .eq. nomevt) then
            icode = zi(jeeact-1+ieven)
            if (icode .eq. 1) lactiv = .true.
        endif
 15 end do
!
    call jedema()
end subroutine
