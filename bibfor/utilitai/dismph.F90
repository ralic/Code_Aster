subroutine dismph(questi, nomobz, repi, repkz, ierd)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!     --     DISMOI(PHENOMENE)
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismgd.h'
    include 'asterfort/u2mesk.h'
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=32) :: repk
    character(len=16) :: nomob
    character(len=*) :: nomobz, repkz
! ----------------------------------------------------------------------
!    IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN PHENOMENE
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=8) :: k8bid
!
! DEB-------------------------------------------------------------------
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob = nomobz
    if (nomob(1:9) .eq. 'THERMIQUE') then
        repk = 'TEMP_R'
    else if (nomob(1:9).eq.'MECANIQUE') then
        repk = 'DEPL_R'
    else if (nomob(1:9).eq.'ACOUSTIQU') then
        repk = 'PRES_C'
    else if (nomob(1:9).eq.'NON_LOCAL') then
        repk = 'VANL_R'
    else
        call u2mesk('F', 'UTILITAI_66', 1, nomob)
        ierd = 1
        goto 10
    endif
!
!
    if (questi .eq. 'NOM_GD') then
!        C'EST DEJA FAIT !
    else if (questi.eq.'NUM_GD') then
        call dismgd('NUM_GD', repk(1:8), repi, k8bid, ierd)
    else if (questi.eq.'NOM_MOLOC') then
        if (nomob(1:9) .eq. 'THERMIQUE') then
            repk = 'DDL_THER'
        else if (nomob(1:9).eq.'MECANIQUE') then
            repk = 'DDL_MECA'
        else if (nomob(1:9).eq.'ACOUSTIQU') then
            repk = 'DDL_ACOU'
        else
            call assert(.false.)
        endif
    else
        ierd = 1
    endif
!
10  continue
    repkz = repk
end subroutine
