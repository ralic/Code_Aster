subroutine apcalc(sdappa)
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
    include 'asterfort/apcaln.h'
    include 'asterfort/apforc.h'
    include 'asterfort/apimpr.h'
    include 'asterfort/apvepa.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    character(len=19) :: sdappa
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT
!
! REALISATION DE L'APPARIEMENT ENTRE DEUX SURFACES MAILLEES
!
! ----------------------------------------------------------------------
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
!
! ----------------------------------------------------------------------
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
        write (ifm,*) '<APPARIEMENT> REALISATION DE L''APPARIEMENT'
    endif
!
! --- CALCUL DES TANGENTES
!
    call apcaln(sdappa)
!
! --- APPARIEMENT METHODE FORCE BRUTE
!
    call apforc(sdappa)
!
! --- VERIFICATIONS APPARIEMENT
!
    call apvepa(sdappa)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        call apimpr(sdappa, ifm)
    endif
!
    call jedema()
!
end subroutine
