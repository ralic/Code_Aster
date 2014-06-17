subroutine dismch(questi, nomobz, repi, repkz, ierd)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!     --     DISMOI(CHARGE)
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/dismmo.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=*) :: nomobz, repkz
    character(len=32) :: repk
    character(len=8) :: nomob, modele
! ----------------------------------------------------------------------
!     IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOB  : NOM D'UN OBJET DE TYPE  CHARGE
!     OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPK   : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=4) :: suf
    character(len=8) :: ktyp
    character(len=19) :: nom19
!
!
!
!-----------------------------------------------------------------------
    integer ::   ier1, ier2, iphen
    character(len=8), pointer :: type(:) => null()
    character(len=8), pointer :: nomo(:) => null()
    character(len=8), pointer :: afck(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob = nomobz
    nom19 = nomobz
!
!     -- CHARGE OU CHAR_CINE ?
    call jeexin(nomob//'.TYPE', ier1)
    call jeexin(nom19//'.AFCK', ier2)
    if (ier1 .eq. 0 .and. ier2 .eq. 0 .or. ier1 .ne. 0 .and. ier2 .ne. 0) then
        ierd=1
        goto 9999
    endif
!
    if (ier1 .gt. 0) then
        call jeveuo(nomob//'.TYPE', 'L', vk8=type)
        ktyp=type(1)
    else if (ier2.gt.0) then
        call jeveuo(nom19//'.AFCK', 'L', vk8=afck)
        ktyp=afck(1)
        modele=afck(2)
    endif
!
    if (ktyp(1:5) .eq. 'MECA_') then
        iphen = 1
        suf = 'CHME'
    else if (ktyp(1:5).eq.'THER_') then
        iphen = 2
        suf = 'CHTH'
    else if (ktyp(1:5).eq.'ACOU_') then
        iphen = 3
        suf = 'CHAC'
    else if (ktyp(1:5).eq.'CIME_') then
        iphen = 1
        suf = 'CIME'
    else if (ktyp(1:5).eq.'CITH_') then
        iphen = 2
        suf = 'CITH'
    else if (ktyp(1:5).eq.'CIAC_') then
        iphen = 3
        suf = 'CIAC'
! --- LIGREL ELEMENTS DE CONTACT
    else if (ktyp(1:3).eq.'ME ') then
        iphen = 1
        suf = 'CHME'
    else
        call utmess('F', 'UTILITAI_52')
    endif
!
    if ((suf(1:2).eq.'CH') .and. (ktyp(1:3).ne.'ME ')) then
        call jeveuo(nomob//'.'//suf//'.MODEL.NOMO', 'L', vk8=nomo)
        modele = nomo(1)
    endif
!
!
    if (questi .eq. 'PHENOMENE') then
        if (iphen .eq. 1) then
            repk = 'MECANIQUE'
        else if (iphen.eq.2) then
            repk = 'THERMIQUE'
        else if (iphen.eq.3) then
            repk = 'ACOUSTIQUE'
        else
            repk = ' '
        endif
!
!
    else if (questi.eq.'NOM_MODELE') then
        repk = modele
!
    else if (questi.eq.'TYPE_CHARGE') then
        repk = ktyp
!
    else if (questi.eq.'NOM_MAILLA') then
        call dismmo(questi, modele, repi, repk, ierd)
!
    else if (questi.eq.'NOM_LIGREL') then
        repk = nomob//'.'//suf//'.LIGRE'
!
    else
        ierd=1
    endif
!
9999  continue
    repkz = repk
    call jedema()
end subroutine
