function xxconi(defico, nomfis, typmai)
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
    implicit none
    integer :: xxconi
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=8) :: nomfis
    character(len=4) :: typmai
    character(len=24) :: defico
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (METHODE XFEM - UTILITAIRE)
!
! RETOURNE LA ZONE DE CONTACT CORRESPONDANT A UNE FISSURE
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DU CONTACT
! IN  NOMFIS : NOM DE LA SD FISS_XFEM
! IN  TYPMAI : TYPE DE LA FISSURE: POUR L'INSTANT 'MAIT'
!
!
!
!
!
!
    character(len=24) :: xfimai
    integer :: jfimai
    integer :: nzoco, izone, iret
    character(len=8) :: fiscou
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    iret = 0
!
! --- ACCES OBJETS JEVEUX
!
    xfimai = defico(1:16)//'.XFIMAI'
    call jeveuo(xfimai, 'L', jfimai)
    call jelira(xfimai, 'LONMAX', nzoco)
!
! --- RECHERCHE FISSURE DANS MAITRE
!
    do 10 izone = 1, nzoco
        if (typmai .eq. 'MAIT') then
            fiscou = zk8(jfimai-1+izone)
        else
            ASSERT(.false.)
        endif
        if (fiscou .eq. nomfis) then
            if (iret .eq. 0) then
                iret = izone
                goto 11
            else
                ASSERT(.false.)
            endif
        endif
10  end do
11  continue
!
    if (iret .le. 0) then
        call utmess('F', 'XFEM_4')
    else
        xxconi = iret
    endif
!
    call jedema()
!
end function
