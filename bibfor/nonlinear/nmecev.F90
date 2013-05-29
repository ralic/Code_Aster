subroutine nmecev(sderro, acces, nomevd, action)
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
    implicit     none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: sderro
    character(len=1) :: acces
    character(len=16) :: action, nomevd
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! ECHEC DU TRAITEMENT D'UNE ACTION - SAUVEGARDE/LECTURE POUR INFO
!
! ----------------------------------------------------------------------
!
!
! IN  SDERRO : SD ERREUR
! IN  ACCES  : TYPE ACCES 'E' OU 'L'
! I/O NOMEVD : NOM DE L'EVENEMENT
! I/O ACTION : NOM DE L'ACTION
!
!
!
!
    character(len=24) :: errevt
    integer :: jeeevt
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD
!
    errevt = sderro(1:19)//'.EEVT'
    call jeveuo(errevt, 'E', jeeevt)
!
    if (acces .eq. 'E') then
        zk16(jeeevt-1+1) = nomevd
        zk16(jeeevt-1+2) = action
    else if (acces.eq.'L') then
        nomevd = zk16(jeeevt-1+1)
        action = zk16(jeeevt-1+2)
    else
        call assert(.false.)
    endif
!
    call jedema()
end subroutine
