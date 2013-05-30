subroutine dismct(questi, nomobz, repi, repkz, ierd)
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
!     --     DISMOI(CATALOGUE)
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=32) :: repk
    character(len=*) :: nomobz, repkz
! ----------------------------------------------------------------------
!    IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM BIDON (1 CARACTERE)
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=1) :: k1bid
!
!
!
!-----------------------------------------------------------------------
    integer :: ianbno, ityma, nbtyma
!-----------------------------------------------------------------------
    call jemarq()
    call assert(nomobz.eq.'&')
    repk = ' '
    repi = 0
    ierd = 0
!
    call jelira('&CATA.TM.NBNO', 'NUTIOC', nbtyma, k1bid)
!
    if (questi .eq. 'NB_TYPE_MA') then
        repi=nbtyma
!
    else if (questi.eq.'NB_NO_MAX') then
        repi=0
        do 1, ityma=1,nbtyma
        call jeveuo(jexnum('&CATA.TM.NBNO', ityma), 'L', ianbno)
        repi=max(repi,zi(ianbno))
 1      continue
!
    else
        ierd=1
    endif
!
    repkz = repk
    call jedema()
end subroutine
