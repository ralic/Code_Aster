subroutine dismff(questi, nomobz, repi, repkz, ierd)
    implicit none
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
! person_in_charge: samuel.geniaut at edf.fr
!
!     --     DISMOI(FOND_FISS)
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: repi, ierd
    character(len=*) :: questi, nomobz, repkz
    character(len=32) :: repk
    character(len=8) :: nomob
! ----------------------------------------------------------------------
!     IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOB  : NOM D'UN OBJET DE TYPE SD_FOND_FISS
!     OUT:
!       REPI   : REPONSE ( SI ENTIER )
!       REPK   : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD    : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    integer :: jinfo
!
!
    call jemarq()
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob=nomobz
!
    if (questi .eq. 'SYME') then
!
        call jeveuo(nomob//'.INFO', 'L', jinfo)
        repk = zk8(jinfo-1+1)
!
    else if (questi.eq.'CONFIG_INIT') then
!
        call jeveuo(nomob//'.INFO', 'L', jinfo)
        repk = zk8(jinfo-1+2)
!
    else if (questi.eq.'TYPE_FOND') then
!
        call jeveuo(nomob//'.INFO', 'L', jinfo)
        repk = zk8(jinfo-1+3)
!
    else
!
        ierd=1
!
    endif
!
    repkz = repk
    call jedema()
end subroutine
