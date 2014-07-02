subroutine quadco(char, indqua)
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
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    character(len=8) :: char
    integer :: indqua
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - LECTURE DONNEES)
!
! TRAITEMENT DU CAS DES MAILLES QUADRATIQUES
!
! ----------------------------------------------------------------------
!
!
! INDQUA VAUT 0 SI L'ON DOIT CONSIDERER LES NOEUDS MILIEUX A PART
! DANS CE CAS, PERMETTRA DE FAIRE LA LIAISON LINEAIRE DANS CACOEQ
! MASI UNIQUEMENT POUR LES QUAD8 EN SOLIDE.
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE)
! OUT INDQUA : VAUT 0 LORSQUE L'ON DOIT TRAITER LES NOEUDS MILIEUX
!                     A PART
!              VAUT 1 LORSQUE L'ON DOIT TRAITER LES NOEUDS MILIEUX
!                     NORMALEMENT
!
!
!
!
    integer :: iform
    character(len=24) :: defico
    aster_logical :: lallv, lpenac, lgliss
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    indqua = 0
    defico = char(1:8)//'.CONTACT'
!
! --- TYPES DE CONTACT
!
    lallv = cfdisl(defico,'ALL_VERIF')
    iform = cfdisi(defico,'FORMULATION')
    lpenac = cfdisl(defico,'CONT_PENA')
    lgliss = cfdisl(defico,'CONT_DISC_GLIS')
!
! --- TRAITEMENT DU CAS DES MAILLES QUADRATIQUES
!
    indqua = 0
    if (iform .eq. 1) then
        if (lallv .or. lpenac .or. lgliss) then
            indqua = 1
        else
            indqua = 0
        endif
    else if (iform.eq.2) then
        indqua = 1
    else if (iform.eq.3) then
        indqua = 1
    else
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine
