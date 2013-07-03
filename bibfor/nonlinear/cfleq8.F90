subroutine cfleq8(noma, defico, nzoco, nsuco, nnoco,&
                  nnoco0, listno, poinsn)
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
#include "asterfort/cfleqa.h"
#include "asterfort/cfleqb.h"
#include "asterfort/cfleqc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: noma
    integer :: nnoco0, nsuco, nnoco, nzoco
    character(len=24) :: defico
    character(len=24) :: listno
    character(len=24) :: poinsn
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES )
!
! CREATION D'UNE LISTE DES NOEUDS MILIEUX DES ARETES POUR LES MAILLES
! QUADRATIQUES
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : NOM SD CONTACT DEFINITION
! IN  NZOCO  : NOMBRE TOTAL DE ZONES DE CONTACT
! IN  NSUCO  : NOMBRE TOTAL DE SURFACES DE CONTACT
! IN  NNOCO0 : NOMBRE DE NOEUDS INITIAL
! OUT POINSN : POINTEUR MISE A JOUR POUR PSURNO
! OUT LISTNO : LISTE DES NOEUDS RESTANTES (LONGUEUR NNOCO
! OUT NNOCO  : NOMBRE DE NOEUDS FINAL
!
!
!
!
    character(len=24) :: contno
    integer :: jnoco
    character(len=24) :: indino
    integer :: jindno
    integer :: jno
    integer :: ino, k
    integer :: elimno, nnoqua
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES AUX STRUCTURES DE DONNEES DE CONTACT
!
    contno = defico(1:16)//'.NOEUCO'
    call jeveuo(contno, 'L', jnoco)
!
! --- INITIALISATIONS
!
    nnoqua = 0
    elimno = 0
    indino = '&&CFLEQ8.INDINO'
!
! --- NOMBRE TOTAL DE NOEUDS QUADRATIQUES
!
    call cfleqa(noma, defico, nzoco, nnoqua)
!
! --- PAS DE QUAD8
!
    if (nnoqua .eq. 0) then
        goto 999
    else
        call u2mess('A', 'CONTACT_8')
    endif
!
! --- ECRITURE LISTE DES NOEUDS QUADRATIQUES
!
    call cfleqb(noma, defico, nzoco, nnoqua)
!
! --- ELIMINATION DES NOEUDS MILIEUX DES ARETES DES QUAD8
!
    call cfleqc(noma, defico, nzoco, nnoco, nsuco,&
                poinsn, indino, elimno)
!
! --- RECOPIE DES NOEUDS NON ELIMINES DANS TABLEAU DE TRAVAIL
!
    nnoco = nnoco0 - elimno
    call wkvect(listno, 'V V I', nnoco, jno)
!
! --- TRAITEMENT DES NOEUDS MILIEUX DES ARETES DES QUAD8
!
    k = 0
    call jeveuo(indino, 'L', jindno)
    do 120 ino = 1, nnoco0
        if (zi(jindno+ino-1) .eq. 0) then
            k = k + 1
            zi(jno+k-1) = zi(jnoco+ino-1)
        endif
120  end do
    call assert(k.eq.nnoco)
!
999  continue
!
! --- DESTRUCTION DU VECTEUR DE TRAVAIL TEMPORAIRE
!
    call jedetr(indino)
!
    call jedema()
end subroutine
