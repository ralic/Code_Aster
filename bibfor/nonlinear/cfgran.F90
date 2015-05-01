subroutine cfgran(resoco, nbliac, kkliai, kkliac)
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
!
    implicit none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resoco
    integer :: nbliac, kkliai, kkliac
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! VERIFICATION SI L'ENSEMBLE DES LIAISONS SUPPOSEES TROP GRAND
!
! ----------------------------------------------------------------------
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! OUT KKLIAI : NUMERO DE LA LIAISON AYANT LE MU LE PLUS NEGATIF
!              0 SI TOUS LES MU SONT POSITIF (ON A CONVERGE)
! OUT KKLIAC : NUMERO DE LA LIAISON _ACTIVE_ AYANT LE MU LE PLUS NEGATIF
!
!
!
!
    real(kind=8) :: rminmu, valmu
    character(len=19) :: liac, mu
    integer :: jliac, jmu
    integer :: iliac, index
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    kkliai = 0
    kkliac = 0
    index = 0
    rminmu = r8maem()
!
! --- ACCES STRUCTURES DE DONNEES DE CONTACT
!
    mu = resoco(1:14)//'.MU'
    liac = resoco(1:14)//'.LIAC'
    call jeveuo(mu, 'L', jmu)
    call jeveuo(liac, 'L', jliac)
!
! --- MU MINIMUM
!
    do 10 iliac = 1, nbliac
        valmu = zr(jmu+iliac-1)
        if (rminmu .gt. valmu) then
            rminmu = valmu
            index = iliac
        endif
10  end do
!
! --- ON REPERE LA LIAISON KKMIN AYANT LE MU LE PLUS NEGATIF
!
    kkliac = index
    if (kkliac .ne. 0) then
        kkliai = zi(jliac-1+kkliac)
    endif
!
! --- SI TOUS LES MU SONT > 0 -> ON A CONVERGE (IL Y A CONTACT)
!
    if (rminmu .ge. 0.d0) then
        kkliai = 0
        kkliac = 0
    endif
!
    call jedema()
!
end subroutine
