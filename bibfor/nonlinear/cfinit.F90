subroutine cfinit(noma, fonact, defico, resoco, numins,&
                  sddyna, valinc, sdnume)
!
implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/cfdisl.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mmbouc.h"
#include "asterfort/mminit.h"
#include "asterfort/vtzero.h"
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
    character(len=8), intent(in) :: noma
    character(len=24), intent(in) :: defico
    character(len=24), intent(in) :: resoco
    integer, intent(in) :: numins
    integer, intent(in) :: fonact(*)
    character(len=19), intent(in) :: valinc(*)
    character(len=19), intent(in) :: sddyna
    character(len=19), intent(in) :: sdnume
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION CONTACT)
!
! INITIALISATION DES PARAMETRES DE CONTACT POUR LE NOUVEAU PAS DE
! TEMPS
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  DEFICO : SD DEFINITION DU CONTACT
! IN  RESOCO : SD RESOLUTION DU CONTACT
! IN  SDDYNA : SD DYNAMIQUE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  NUMINS : NUMERO INSTANT COURANT
!
!
!
!
    aster_logical :: lreac(3)
    character(len=24) :: clreac
    integer :: jclrea
    character(len=24) :: autoc1, autoc2
    aster_logical :: leltc, lctcd, lallv
    integer :: mmitgo
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- FONCTIONNALITES ACTIVEES
!
    lctcd = isfonc(fonact,'CONT_DISCRET')
    leltc = isfonc(fonact,'ELT_CONTACT')
    lallv = cfdisl(defico,'ALL_VERIF')
    if (lallv) goto 99
!
! --- INITIALISATIONS POUR CONTACT DISCRET
!
    if (lctcd) then
!
! ----- ACCES OBJETS
!
        autoc1 = resoco(1:14)//'.REA1'
        autoc2 = resoco(1:14)//'.REA2'
        clreac = resoco(1:14)//'.REAL'
        call jeveuo(clreac, 'E', jclrea)
!
! ----- PARAMETRES DE REACTUALISATION GEOMETRIQUE
!
        lreac(1) = .true.
        lreac(2) = .false.
        lreac(3) = .true.
        if (cfdisl(defico,'REAC_GEOM_SANS')) then
            if (numins .ne. 1) then
                lreac(1) = .false.
                lreac(3) = .false.
            endif
        endif
!
        call mmbouc(resoco, 'GEOM', 'INIT', mmitgo)
        call mmbouc(resoco, 'GEOM', 'INCR', mmitgo)
!
! ----- INITIALISATION DES VECTEURS POUR REAC_GEOM
!
        call vtzero(autoc1)
        call vtzero(autoc2)
!
! ----- SAUVEGARDE
!
        zl(jclrea-1+1) = lreac(1)
        zl(jclrea-1+2) = lreac(2)
        zl(jclrea-1+3) = lreac(3)
    endif
!
! --- INITIALISATIONS POUR CONTACT CONTINU ET XFEM
!
    if (leltc) then
        call mminit(noma, defico, resoco, sddyna, valinc,&
                    sdnume)
    endif
!
 99 continue
!
    call jedema()
!
end subroutine
