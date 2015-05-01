subroutine nmeraz(sderro, typevt)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: sderro
    character(len=4) :: typevt
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (SD ERREUR)
!
! REMISE A ZERO DES EVENEMENTS
!
! ----------------------------------------------------------------------
!
!
! IN  SDERRO : SD GESTION DES ERREURS
! IN  TYPEVT : TYPE DE L'EVENEMENT
!              'TOUS' - TOUS LES EVENEMENTS
!              'EVEN' - EVENEMENT SIMPLE
!
! ----------------------------------------------------------------------
!
    integer :: ieven, zeven
    integer :: iret
    character(len=24) :: errinf
    integer :: jeinfo
    character(len=24) :: erraac, erreni
    integer :: jeeact, jeeniv
    character(len=16) :: teven
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD
!
    errinf = sderro(1:19)//'.INFO'
    call jeexin(errinf, iret)
    if (iret .eq. 0) goto 99
    call jeveuo(errinf, 'L', jeinfo)
    zeven = zi(jeinfo-1+1)
    erraac = sderro(1:19)//'.EACT'
    erreni = sderro(1:19)//'.ENIV'
    call jeveuo(erraac, 'E', jeeact)
    call jeveuo(erreni, 'L', jeeniv)
!
! --- EVENEMENTS DESACTIVES
!
    do 15 ieven = 1, zeven
        teven = zk16(jeeniv-1+ieven)(1:9)
        if (typevt .eq. 'TOUS') then
            zi(jeeact-1+ieven) = 0
        else if (typevt.eq.'EVEN') then
            if (teven .eq. 'EVEN') zi(jeeact-1+ieven) = 0
        else
            ASSERT(.false.)
        endif
15  end do
!
99  continue
!
    call jedema()
end subroutine
