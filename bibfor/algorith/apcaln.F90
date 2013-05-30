subroutine apcaln(sdappa)
!
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
    include 'asterfort/aptgen.h'
    include 'asterfort/aptgno.h'
    include 'asterfort/apverl.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    character(len=19) :: sdappa
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT - NORMALES
!
! PRE-CALCUL DES NORMALES
!
! ----------------------------------------------------------------------
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
!
!
!
!
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<APPARIEMENT> ... CALCUL DES TANGENTES SUR' //&
        ' TOUS LES NOEUDS'
    endif
!
! --- CALCUL DES VECTEURS TANGENTS EN CHAQUE NOEUD POUR CHAQUE ELEMENT
!
    call aptgen(sdappa)
!
! --- CALCUL DES VECTEURS TANGENTS EN CHAQUE NOEUD (MOYENNE)
!
    call aptgno(sdappa)
!
! --- VERIFICATION FACETTISATION SURFACE MAITRE
!
    call apverl(sdappa)
!
    call jedema()
!
end subroutine
