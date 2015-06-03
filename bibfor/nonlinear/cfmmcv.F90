subroutine cfmmcv(noma, modele, numedd, fonact, sddyna,&
                  sdimpr, sdstat, sddisc, sdtime, sderro,&
                  numins, iterat, defico, resoco, valinc,&
                  solalg, instan)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfconv.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mm_cycl_print.h"
#include "asterfort/mmbclc.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmimck.h"
#include "asterfort/nmimcr.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: fonact(*)
    integer :: iterat, numins
    character(len=19) :: sddisc, sddyna
    character(len=8) :: noma
    character(len=24) :: numedd, modele
    character(len=24) :: defico, resoco
    character(len=24) :: sdimpr, sderro, sdstat, sdtime
    character(len=19) :: solalg(*), valinc(*)
    real(kind=8) :: instan
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CRITERES DE CONVERGENCE POUR LE CONTACT
!
! --------------------------------------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  MODELE : NOM DU MODELE
! IN  NUMEDD : NUMEROTATION NUME_DDL
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  SDDYNA : SD DYNAMIQUE
! IN  SDIMPR : SD AFFICHAGE
! IN  SDSTAT : SD STATISTIQUES
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDTIME : SD TIMER
! IN  SDERRO : GESTION DES ERREURS
! IN  ITERAT : NUMERO D'ITERATION
! IN  NUMINS : NUMERO D'INSTANT
! IN  DEFICO : SD POUR LA DEFINITION DU CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DU CONTACT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: lctcd=.false._1, lctcc=.false._1, lnewtc=.false._1
    aster_logical :: mmcvca=.false._1
    character(len=8) :: nomo=' '
    character(len=16) :: k16bla=' '
    real(kind=8) :: r8bid=0.d0
    integer :: ntpc=0
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    mmcvca = .false.
    nomo = modele(1:8)
    k16bla = ' '
    ntpc = cfdisi(defico,'NTPC' )
!
! --- FONCTIONNALITES ACTIVEES
!
    lctcd = isfonc(fonact,'CONT_DISCRET')
    lctcc = isfonc(fonact,'CONT_CONTINU')
    lnewtc = isfonc(fonact,'CONT_NEWTON')
!
! --- VALEURS NON AFFECTEES DANS LE TABLEAU
!
    call nmimck(sdimpr, 'BOUC_NOEU', k16bla, .false._1)
    call nmimcr(sdimpr, 'BOUC_VALE', r8bid, .false._1)
!
! --- CONVERGENCE ADAPTEE AU CONTACT DISCRET
!
    if (lctcd) then
        call cfconv(noma, sdstat, sdimpr, sderro, defico,&
                    resoco, solalg)
    endif
!
! --- CONVERGENCE ADAPTEE AU CONTACT CONTINU
!
    if (lnewtc) then
        call mmbclc(noma, nomo, numedd, iterat, numins,&
                    sddisc, sddyna, sdimpr, defico, resoco,&
                    valinc, solalg, sdtime, sdstat, mmcvca,&
                    instan)
        if (mmcvca) then
            call nmcrel(sderro, 'DIVE_CTCC', .false._1)
        else
            call nmcrel(sderro, 'DIVE_CTCC', .true._1)
        endif
    endif
!
! - Cycling informations printing in convergence table
!
    if (lctcc) then
        call mm_cycl_print(sdimpr, sdstat)
    endif
!
    call jedema()
end subroutine
