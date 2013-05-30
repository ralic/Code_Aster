subroutine dismml(questi, nomobz, repi, repkz, ierd)
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
!     --     DISMOI(MACR_ELEM_STAT)
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterfort/dismmo.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=32) :: repk
    character(len=8) :: nomob
    character(len=*) :: nomobz, repkz
! ----------------------------------------------------------------------
!    IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN OBJET DE TYPE LIGREL
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    integer :: jrefm, iexi
    character(len=8) :: modele
!
!
!
    call jemarq()
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob = nomobz
!
    call jeveuo(nomob//'.REFM', 'L', jrefm)
    if (questi .eq. 'NOM_MAILLA') then
        repk= zk8(jrefm-1+2)
    else if (questi.eq.'NOM_MODELE') then
        repk= zk8(jrefm-1+1)
    else if (questi.eq.'NOM_NUME_DDL') then
        repk= zk8(jrefm-1+5)
    else if (questi.eq.'NOM_PROJ_MESU') then
        repk= zk8(jrefm-1+9)
!
    else if (questi.eq.'DIM_GEOM') then
        modele=zk8(jrefm)
        call dismmo(questi, modele, repi, repk, ierd)
!
    else if (questi.eq.'EXI_AMOR') then
        call jeexin(nomob//'.MAEL_AMOR_VALE', iexi)
        if (iexi .ne. 0) then
            repk='OUI'
        else
            repk='NON'
        endif
    else
        ierd=1
    endif
!
    repkz = repk
    call jedema()
end subroutine
