subroutine mmpnoe(defico, posmae, alias, typint, iptm,&
                  posnoe)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfconn.h'
    include 'asterfort/cfnben.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    character(len=8) :: alias
    character(len=24) :: defico
    integer :: iptm
    integer :: posmae, posnoe
    integer :: typint
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! DONNE LE NUMERO DU NOEUD DANS SD CONTACT SI INTEGRATION AUX NOEUDS
!
! ----------------------------------------------------------------------
!
!
! IN  TYPINT : TYPE D'INTEGRATION
! IN  IPTM   : NUMERO DU POINT D'INTEGRATION DANS LA MAILLE
! IN  ALIAS  : NOM D'ALIAS DE L'ELEMENT
! IN  POSMAE : POSITION DE LA MAILLE ESCLAVE DANS LES SD CONTACT
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  TYPINT : TYPE D'INTEGRATION
! OUT POSNOE : POSITION DU NOEUD ESCLAVE SI INTEG. AUX NOEUDS
!
!
!
!
    integer :: nbnoe, jdecne, inoe
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    posnoe = 0
!
! --- NOEUD ESCLAVE SI INTEGRATION AUX NOEUDS
!
    if (typint .eq. 1) then
        inoe = iptm
!       GLUTE POUR QUAD8 POUR LEQUEL NBNOE=8<NBPC=9
        if (alias .eq. 'QU8') then
            posnoe = -1
        else
            call cfnben(defico, posmae, 'CONNEX', nbnoe, jdecne)
            if (inoe .le. nbnoe) then
                call cfconn(defico, jdecne, inoe, posnoe)
            else
                call assert(.false.)
            endif
        endif
    else
        posnoe = 0
    endif
!
    call jedema()
!
end subroutine
