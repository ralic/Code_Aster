subroutine nmpro2(fonact, numedd, numfix)
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
    include 'asterfort/copisd.h'
    include 'asterfort/isfonc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    character(len=24) :: numedd, numfix
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! DUPLICATION DU NUME_DDL - CREATION DU NUME_DDL 'FIXE'
!
! ----------------------------------------------------------------------
!
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! OUT NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
!
!
!
!
    logical :: lmacr, leltc
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- FONCTIONNALITES ACTIVEES
!
    lmacr = isfonc(fonact,'MACR_ELEM_STAT')
    leltc = isfonc(fonact,'ELT_CONTACT')
!
! --- DUPLICATION (OU PAS !)
!
    if (leltc .and. lmacr) then
        numfix = '&&NMPRO2.NUMFIX'
        call copisd('NUME_DDL', 'V', numedd, numfix)
    else
        numfix = numedd
    endif
!
    call jedema()
!
end subroutine
