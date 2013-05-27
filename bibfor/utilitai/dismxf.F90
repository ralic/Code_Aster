subroutine dismxf(questi, nomobz, repi, repkz, ierd)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     --     DISMOI(XFEM)
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: repi, ierd
    character(len=*) :: questi, nomobz, repkz
    character(len=32) :: repk
    character(len=8) :: nomob
! ----------------------------------------------------------------------
!     IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOB  : NOM D'UN OBJET DE TYPE SD_FISS_XFEM
!     OUT:
!       REPI   : REPONSE ( SI ENTIER )
!       REPK   : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD    : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    integer :: jinfo, jmod, long
    character(len=8) :: k8bid
!
!
    call jemarq()
    nomob=nomobz
    repk=' '
    ierd = 0
!
    if (questi .eq. 'TYPE_DISCONTINUITE') then
!
        call jeveuo(nomob//'.INFO', 'L', jinfo)
        repk = zk16(jinfo-1+1)
!
    else if (questi.eq.'CHAM_DISCONTINUITE') then
!
        call jeveuo(nomob//'.INFO', 'L', jinfo)
        repk = zk16(jinfo-1+2)
!
    else if (questi.eq.'TYPE_FOND') then
!
        call jeveuo(nomob//'.INFO', 'L', jinfo)
        repk = zk16(jinfo-1+3)
!
    else if (questi.eq.'NOM_MODELE') then
!
        call jeveuo(nomob//'.MODELE', 'L', jmod)
        repk = zk8(jmod-1+1)
!
    else if (questi.eq.'NB_FOND') then
!
        call jelira(nomob//'.FONDMULT', 'LONMAX', long, k8bid)
        repi = long/2
!
    else if (questi.eq.'NB_POINT_FOND') then
!
        call jelira(nomob//'.FONDFISS', 'LONMAX', long, k8bid)
        repi = long/4
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
