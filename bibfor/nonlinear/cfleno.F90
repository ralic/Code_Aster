subroutine cfleno(defico, nsuco, nnoco0, listno, poinsn,&
                  nnoco)
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
    integer :: nnoco0, nnoco
    character(len=24) :: defico
    character(len=24) :: listno
    character(len=24) :: poinsn
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES - ELIMINATION)
!
! CREATION D'UNE LISTE DES NOEUDS EN DOUBLE AU SEIN D'UNE SURFACE
! DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : NOM SD CONTACT DEFINITION
! IN  NSUCO  : NOMBRE TOTAL DE SURFACES DE CONTACT
! IN  NNOCO0 : NOMBRE TOTAL DE NOEUDS DES SURFACES
! OUT POINSN : POINTEUR MISE A JOUR POUR PSURNO
!               ZI(JELINO+ISUCO) - ZI(JELINO+ISUCO-1)
!                NOMBRE DE NOEUDS SUPPRIMES POUR LA SURFACE ISUCO
! OUT LISTNO : LISTE DES NOEUDS RESTANTES (LONGUEUR NNOCO
! OUT NNOCO  : NOMBRE DE NOEUDS AU FINAL
!
!
!
!
    character(len=24) :: contno
    integer :: jnoco
    integer :: jindno, jelino
    integer :: jdecno, jno
    integer :: isuco, i, ii, ino1, ino2, k
    integer :: elimno, nbno
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- CREATION DES VECTEURS DE TRAVAIL TEMPORAIRES
!
    call wkvect('&&CFLENO.INDINO', 'V V I', nnoco, jindno)
    call wkvect(poinsn, 'V V I', nsuco+1, jelino)
!
! --- ACCES AUX STRUCTURES DE DONNEES DE CONTACT
!
    contno = defico(1:16)//'.NOEUCO'
    call jeveuo(contno, 'L', jnoco)
!
! --- INITIALISATIONS
!
    elimno = 0
!
! --- REPERAGE DES NOEUDS REDONDANTS POUR CHAQUE SURFACE
!
    do 110 isuco = 1, nsuco
        zi(jelino+isuco) = zi(jelino+isuco-1)
        call cfnbsf(defico, isuco, 'NOEU', nbno, jdecno)
        do 20 i = 1, nbno
            ino1 = zi(jnoco+jdecno+i-1)
            do 10 ii = 1, i - 1
                ino2 = zi(jnoco+jdecno+ii-1)
                if (ino1 .eq. ino2) then
                    zi(jindno+jdecno+i-1) = 1
                    zi(jelino+isuco) = zi(jelino+isuco) + 1
                    elimno = elimno + 1
                    goto 20
                endif
10          continue
20      continue
110  end do
!
! --- RECOPIE DES NOEUDS NON ELIMINES DANS TABLEAU DE TRAVAIL
!
    nnoco = nnoco0 - elimno
    call wkvect(listno, 'V V I', nnoco, jno)
!
! --- TRAITEMENT DES NOEUDS
!
    k = 0
    do 120 i = 1, nnoco0
        if (zi(jindno+i-1) .eq. 0) then
            k = k + 1
            zi(jno+k-1) = zi(jnoco+i-1)
        endif
120  end do
    ASSERT(k.eq.nnoco)
!
! --- DESTRUCTION DES VECTEURS DE TRAVAIL TEMPORAIRES
!
    call jedetr('&&CFLENO.INDINO')
!
    call jedema()
end subroutine
