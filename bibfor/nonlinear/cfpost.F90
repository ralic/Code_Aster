subroutine cfpost(noma, defico, resoco, ddepla, ctccvg)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfjefi.h"
#include "asterfort/copisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    character(len=19) :: ddepla
    integer :: ctccvg
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - ALGORITHME)
!
! POST-TRAITEMENT ALGORITHMES
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  DDEPLA : INCREMENT DE DEPLACEMENT
! OUT CTCCVG : CODE RETOUR CONTACT DISCRET
!                -1 : PAS DE CALCUL DU CONTACT DISCRET
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : NOMBRE MAXI D'ITERATIONS
!                 2 : MATRICE SINGULIERE
!
!
!
!
    integer :: ifm, niv
    character(len=19) :: ddeplc
    integer :: jddepc
    aster_logical :: lpenac, lgcp
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... POST-TRAITEMENT DU CALCUL'
    endif
!
! --- PARAMETRES
!
    lpenac = cfdisl(defico,'CONT_PENA')
    lgcp = cfdisl(defico,'CONT_GCP' )
!
! --- SORTIE EN ERREUR
!
    if (ctccvg .ne. 0) then
        if (.not.lgcp) then
            if (niv .ge. 2) then
                write (ifm,*) '<CONTACT> ...... SORTIE DIRECTE CAR ERREUR'
            endif
            goto 999
        endif
    endif
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DDEPLC: INCREMENT DE SOLUTION APRES CORRECTION DU CONTACT
!
    ddeplc = resoco(1:14)//'.DELC'
    call jeveuo(ddeplc(1:19)//'.VALE', 'L', jddepc)
!
! --- RECOPIE CHAMP DE DEPLACEMENT SOLUTION
!
    if ((.not.lpenac) .and. (ctccvg.eq.0)) then
        call copisd('CHAMP_GD', 'V', ddeplc, ddepla)
    endif
!
! --- CALCUL DES JEUX FINAUX
!
    call cfjefi(noma, defico, resoco, ddepla)
!
999 continue
!
    call jedema()
!
end subroutine
