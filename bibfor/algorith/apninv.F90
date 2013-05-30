subroutine apninv(sdappa, defico, posnom, questz, vali)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfnben.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    character(len=19) :: sdappa
    character(len=24) :: defico
    character(len=*) :: questz
    integer :: vali, posnom
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! COORDONNEES DU POINT DU NOEUD
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  DEFICO : SD DEFINITION DU CONTACT
! IN  POSNOM : POSITION DU NOEUD MAITRE
! IN  QUESTI : QUESTION
!               NMANOM  NOMBRE DE MAILLES ATTACHEES A CE NOEUD
!               JDECIV  DECALAGE POUR TABLEAU DES CONNECTIVITES INVERSES
! OUT VALI   : REPONSE A LA QUESTION
!
!
!
!
    integer :: nmanom, ibid, jdeciv
    character(len=24) :: questi
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    vali = 0
    questi = questz
!
    if (questi .eq. 'NMANOM') then
        call cfnben(defico, posnom, 'CONINV', nmanom, ibid)
        vali = nmanom
    else if (questi.eq.'JDECIV') then
        call cfnben(defico, posnom, 'CONINV', ibid, jdeciv)
        vali = jdeciv
    else
        call assert(.false.)
    endif
!
    call jedema()
!
end subroutine
