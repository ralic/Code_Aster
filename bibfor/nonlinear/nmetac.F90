subroutine nmetac(fonact, sddyna, defico, nbmax, chaact)
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndynlo.h"
#include "asterfort/wkvect.h"
    integer :: nbmax
    aster_logical :: chaact(nbmax)
    integer :: fonact(*)
    character(len=19) :: sddyna
    character(len=24) :: defico
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (GESTION IN ET OUT)
!
! ACTIVATION DES CHAMPS A TRAITER SUIVANT FONCTIONNALITES ACTIVEES
!
! ----------------------------------------------------------------------
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  DEFICO : SD POUR LA DEFINITION DU CONTACT
! IN  NBMAX  : NOMBRE DE CHAMPS A CONSIDERER
! OUT CHAACT : CHAMPS A ACTIVER/DESACTIVER
!
! ----------------------------------------------------------------------
!
    aster_logical :: lxfcm, ldyna, lxffm, lxczm, lcont, lnoeu, lmuap, lstrx
    aster_logical :: lvibr, lflam, lstab, lener
    character(len=24) :: trav
    integer :: jtrav
    integer :: icham, istop
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- FONCTIONNALITES ACTIVEES
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE' )
    lxfcm = isfonc(fonact,'CONT_XFEM' )
    lcont = isfonc(fonact,'CONTACT' )
    lmuap = ndynlo(sddyna,'MULTI_APPUI')
    lflam = isfonc(fonact,'CRIT_STAB' )
    lstab = isfonc(fonact,'DDL_STAB' )
    lvibr = isfonc(fonact,'MODE_VIBR' )
    lener = isfonc(fonact,'ENERGIE' )
    if (lxfcm) then
        lxffm = isfonc(fonact,'FROT_XFEM')
        lxczm = cfdisl(defico,'EXIS_XFEM_CZM')
    endif
    lstrx = isfonc(fonact,'EXI_STRX')
!
! --- VECTEUR ACTIVATION
!
    trav = '&&NMETAC.TRAV'
    call wkvect(trav, 'V V I', nbmax, jtrav)
!
! --- CHAMPS STANDARDS: DEPL/SIEF_ELGA/VARI_ELGA/FORC_NODA
!
    chaact(1) = .true.
    chaact(2) = .true.
    chaact(3) = .true.
    chaact(16) = .true.
    zi(jtrav-1+1) = 1
    zi(jtrav-1+2) = 1
    zi(jtrav-1+3) = 1
    zi(jtrav-1+16) = 1
!
! --- CARTE COMPORTEMENT
!
    chaact(4) = .true.
    zi(jtrav-1+4) = 1
!
! --- CHAMPS DYNAMIQUE: VITE/ACCE
!
    if (ldyna) then
        chaact(5) = .true.
        chaact(6) = .true.
    endif
    zi(jtrav-1+5) = 1
    zi(jtrav-1+6) = 1
!
! --- CHAMPS XFEM
!
    if (lxfcm) then
        chaact(7) = .true.
        if (lxffm) then
            chaact(8) = .true.
        endif
        if (lxczm) then
            chaact(9) = .true.
        endif
    endif
    zi(jtrav-1+7) = 1
    zi(jtrav-1+8) = 1
    zi(jtrav-1+9) = 1
!
! --- CONTACT
!
    if (lcont) then
        lnoeu = cfdisl(defico,'ALL_INTEG_NOEUD')
        if (lnoeu) then
            chaact(10) = .true.
        endif
    endif
    zi(jtrav-1+10) = 1
!
! --- FLAMBEMENT
!
    if (lflam) then
        chaact(11) = .true.
    endif
    zi(jtrav-1+11) = 1
!
! --- STABILITE
!
    if (lstab) then
        chaact(18) = .true.
    endif
    zi(jtrav-1+18) = 1
!
! --- MODES VIBRATOIRES
!
    if (lvibr) then
        chaact(12) = .true.
    endif
    zi(jtrav-1+12) = 1
!
! --- DEPL/VITE/ACCE D'ENTRAINEMENT EN MULTI-APPUIS
!
    if (lmuap) then
        chaact(13) = .true.
        chaact(14) = .true.
        chaact(15) = .true.
    endif
    zi(jtrav-1+13) = 1
    zi(jtrav-1+14) = 1
    zi(jtrav-1+15) = 1
!
! --- POUTRE MULTI_FIBRE
!
    if (lstrx) then
        chaact(17) = .true.
    endif
    zi(jtrav-1+17) = 1
!
! --- FORCES POUR CALCUL DES ENERGIES
!
    if (lener) then
        chaact(19) = .true.
        chaact(20) = .true.
    endif
    zi(jtrav-1+19) = 1
    zi(jtrav-1+20) = 1
!
! --- VERIFICATION
! --- SI LE ASSERT SE DECLENCHE, C'EST QUE VOUS AVEZ OUBLIE DE DIRE
! --- DANS QUEL CAS ON DOIT S'OCCUPER DU CHAMP
!
    do 10 icham = 1, nbmax
        istop = zi(jtrav-1+icham)
        ASSERT(istop.eq.1)
 10 end do
!
    call jedetr(trav)
    call jedema()
end subroutine
