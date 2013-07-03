subroutine cflema(defico, nsuco, nmaco0, listma, poinsm,&
                  nmaco)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfnbsf.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    integer :: nsuco
    integer :: nmaco0, nmaco
    character(len=24) :: defico
    character(len=24) :: listma
    character(len=24) :: poinsm
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES - ELIMINATION)
!
! CREATION D'UNE LISTE DES MAILLES EN DOUBLES AU SEIN D'UNE SURFACE
! DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : NOM SD CONTACT DEFINITION
! IN  NSUCO  : NOMBRE TOTAL DE SURFACES DE CONTACT
! IN  NMACO0 : NOMBRE TOTAL DE MAILLES DES SURFACES
! OUT POINSM : POINTEUR MISE A JOUR POUR PSURMA
! OUT LISTMA : LISTE DES MAILLES RESTANTES (LONGUEUR NMACO
! OUT NMACO  : NOMBRE DE MAILLES AU FINAL
!
!
!
!
    character(len=24) :: contma
    integer :: jmaco
    integer :: jindma, jelima
    integer :: jdecma, jma
    integer :: isuco, i, ii, ima1, ima2, k
    integer :: elimma, nbma
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- CREATION DES VECTEURS DE TRAVAIL TEMPORAIRES
!
    call wkvect('&&CFLEMA.INDIMA', 'V V I', nmaco, jindma)
    call wkvect(poinsm, 'V V I', nsuco+1, jelima)
!
! --- ACCES AUX STRUCTURES DE DONNEES DE CONTACT
!
    contma = defico(1:16)//'.MAILCO'
    call jeveuo(contma, 'L', jmaco)
!
! --- INITIALISATIONS
!
    elimma = 0
!
! --- REPERAGE DES MAILLES REDONDANTES POUR CHAQUE SURFACE
!
    do 110 isuco = 1, nsuco
        zi(jelima+isuco) = zi(jelima+isuco-1)
        call cfnbsf(defico, isuco, 'MAIL', nbma, jdecma)
        do 20 i = 1, nbma
            ima1 = zi(jmaco+jdecma+i-1)
            do 10 ii = 1, i - 1
                ima2 = zi(jmaco+jdecma+ii-1)
                if (ima1 .eq. ima2) then
                    zi(jindma+jdecma+i-1) = 1
                    zi(jelima+isuco) = zi(jelima+isuco) + 1
                    elimma = elimma + 1
                    goto 20
                endif
10          continue
20      continue
110  end do
!
! --- RECOPIE DES MAILLES NON ELIMINES DANS TABLEAU DE TRAVAIL
!
    nmaco = nmaco0 - elimma
    call wkvect(listma, 'V V I', nmaco, jma)
!
! --- TRAITEMENT DES MAILLES
!
    k = 0
    do 120 i = 1, nmaco0
        if (zi(jindma+i-1) .eq. 0) then
            k = k + 1
            zi(jma+k-1) = zi(jmaco+i-1)
        endif
120  end do
    call assert(k.eq.nmaco)
!
! --- DESTRUCTION DES VECTEURS DE TRAVAIL TEMPORAIRES
!
    call jedetr('&&CFLEMA.INDIMA')
!
    call jedema()
end subroutine
