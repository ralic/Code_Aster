subroutine cfecrd(resoco, questz, ival)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resoco
    character(len=*) :: questz
    integer :: ival
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - UTILITAIRE)
!
! ECRIRE INFORMATIONS SUR L'ETAT DU CONTACT ACTUEL
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO  : SD DE RESOLUTION DU CONTACT
! IN  QUESTI  : VALEUR A LIRE/ECRIRE
!   NDIM   : DIMENSION DE L'ESPACE
!   NBLIAC : NOMBRE DE LIAISON DE CONTACT ACTIVES
!   LLF    : NOMBRE DE LIAISON DE FROTTEMENT (DEUX DIRECTIONS)
!   LLF1   : NOMBRE DE LIAISON DE FROTTEMENT (1ERE DIRECTION )
!   LLF2   : NOMBRE DE LIAISON DE FROTTEMENT (2EME DIRECTION )
!   NBLIAI : NOMBRE DE LIAISONS (NOEUDS ESCLAVES APPARIES)
!   NEQ    : NOMBRE D'EQUATIONS DU SYSTEME
!   NESMAX : NOMBRE MAXI DE NOEUDS ESCLAVES (POUR DECALAGES)
! IN  IVAL    : VALEUR
!
!
!
!
    character(len=24) :: questi
    character(len=19) :: coco
    integer :: jcoco
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    questi = questz
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    coco = resoco(1:14)//'.COCO'
    call jeveuo(coco, 'E', jcoco)
!
    if (questi .eq. 'NDIM') then
        zi(jcoco+1-1) = ival
    else if (questi.eq.'NEQ') then
        zi(jcoco+2-1) = ival
    else if (questi.eq.'NBLIAC') then
        zi(jcoco+3-1) = ival
    else if (questi.eq.'LLF') then
        zi(jcoco+4-1) = ival
    else if (questi.eq.'LLF1') then
        zi(jcoco+5-1) = ival
    else if (questi.eq.'LLF2') then
        zi(jcoco+6-1) = ival
    else if (questi.eq.'NESMAX') then
        zi(jcoco+7-1) = ival
    else if (questi.eq.'NBLIAI') then
        zi(jcoco+8-1) = ival
    else
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine
