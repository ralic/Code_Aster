subroutine dismrc(questi, nomobz, repi, repk, ierd)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jelira.h'
    include 'asterfort/jexnum.h'
    integer :: repi, ierd
    character(len=*) :: questi, repk
    character(len=19) :: nomob
    character(len=*) :: nomobz
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
! IN  : QUESTI : TEXTE PRECISANT LA QUESTION POSEE
! IN  : NOMOBZ : NOM D'UN OBJET DE TYPE RESU_COMPO (K19)
! OUT : REPI   : REPONSE ( SI ENTIERE )
! OUT : REPK   : REPONSE ( SI CHAINE DE CARACTERES )
! OUT : IERD   : CODE RETOUR (0--> OK, 1 --> PB)
! ----------------------------------------------------------------------
    character(len=1) :: kbid
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    nomob = nomobz
! ----------------------------------------------------------------------
    ierd = 0
!
    if (questi .eq. 'NB_CHAMP_MAX') then
        call jelira(jexnum(nomob//'.TACH', 1), 'LONMAX', repi, kbid)
    else if (questi.eq.'NB_CHAMP_UTI') then
        call jelira(nomob//'.ORDR', 'LONUTI', repi, kbid)
    else
        ierd = 1
    endif
!
    repk=' '
end subroutine
