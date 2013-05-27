subroutine dismpm(questi, nomobz, repi, repkz, ierd)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit none
!     --     DISMOI(PHEN_MODE)
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=24) :: repk
    character(len=32) :: nomob
    character(len=*) :: nomobz, repkz
! ----------------------------------------------------------------------
!    IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN PHENMODE : (1:16)  : PHENOMENE
!                                    (17:32) : MODELISATION
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    integer :: nbtm, jphen, imode
    character(len=8) :: kbid
    character(len=16) :: phen, mode
! DEB-------------------------------------------------------------------
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob = nomobz
    phen=nomob(1:16)
    mode=nomob(17:32)
!
    call jelira('&CATA.TM.NOMTM', 'NOMMAX', nbtm, kbid)
    call jenonu(jexnom('&CATA.'//phen(1:13)//'.MODL', mode), imode)
    call assert(imode.gt.0)
    call jeveuo(jexnum('&CATA.'//phen, imode), 'L', jphen)
!
    if (questi .eq. 'DIM_GEOM') then
        repi=zi(jphen-1+nbtm+2)
    else if (questi.eq.'DIM_TOPO') then
        repi=zi(jphen-1+nbtm+1)
    else
        ierd = 1
    endif
!
    repkz = repk
end subroutine
