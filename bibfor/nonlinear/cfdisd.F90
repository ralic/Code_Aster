function cfdisd(resoco, questz)
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
    integer :: cfdisd
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resoco
    character(len=*) :: questz
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - UTILITAIRE)
!
! LECTURE INFORMATIONS SUR L'ETAT DU CONTACT ACTUEL
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
!
!
!
!
    character(len=24) :: questi
    character(len=24) :: coco
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
    call jeveuo(coco, 'L', jcoco)
!
    if (questi .eq. 'NDIM') then
        cfdisd = zi(jcoco+1-1)
    else if (questi.eq.'NEQ') then
        cfdisd = zi(jcoco+2-1)
    else if (questi.eq.'NBLIAC') then
        cfdisd = zi(jcoco+3-1)
    else if (questi.eq.'LLF') then
        cfdisd = zi(jcoco+4-1)
    else if (questi.eq.'LLF1') then
        cfdisd = zi(jcoco+5-1)
    else if (questi.eq.'LLF2') then
        cfdisd = zi(jcoco+6-1)
    else if (questi.eq.'NESMAX') then
        cfdisd = zi(jcoco+7-1)
    else if (questi.eq.'NBLIAI') then
        cfdisd = zi(jcoco+8-1)
    else
        call assert(.false.)
    endif
!
    call jedema()
end function
