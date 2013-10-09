subroutine cfmxme(numedd, sddyna, defico, resoco)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mm_cycl_crsd.h"
#include "asterfort/mm_cycl_init.h"
#include "asterfort/ndynlo.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
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
    character(len=24), intent(in) :: numedd
    character(len=19), intent(in) :: sddyna
    character(len=24), intent(in) :: defico
    character(len=24), intent(in) :: resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES CONTINUE)
!
! CREATION SD DE RESOLUTION RESOCO
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NUME_DDL DE LA MATRICE TANGENTE GLOBALE
! IN  SDDYNA : SD DYNAMIQUE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    integer ::  neq, ntpc
    logical :: ldyna, lnoeu
    character(len=24) :: mdecol, etatct
    integer :: jmdeco, jetat
    character(len=24) :: tabfin, apjeu
    integer :: jtabf, japjeu
    character(len=24) :: vitini, accini
    integer :: ztabf, zetat
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... CREATION DES SD POUR LA '//&
        ' FORMULATION CONTINUE'
    endif
!
! --- INITIALISATIONS
!
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
    ntpc = cfdisi(defico,'NTPC' )
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
!
! --- TABLEAU CONTENANT LES INFORMATIONS DIVERSES
!
    tabfin = resoco(1:14)//'.TABFIN'
    ztabf = cfmmvd('ZTABF')
    call wkvect(tabfin, 'V V R', ztabf*ntpc+1, jtabf)
    zr(jtabf) = ntpc
!
! --- CREATION INDICATEUR DE DECOLLEMENT DANS COMPLIANCE
!
    mdecol = resoco(1:14)//'.MDECOL'
    call wkvect(mdecol, 'V V L', 1, jmdeco)
    zl(jmdeco+1-1) = .false.
!
! --- VECTEUR POUR LA DYNAMIQUE A L INSTANT MOINS
! --- UTILE UNIQUEMENT AFIN D ARCHIVER LE DERNIER INSTANT CALCULE
! --- SI PLANTE POUR LE NOUVEAU PAS DE TEMPS DANS
! --- LES ITERATIONS DE NEWTON
!
    if (ldyna) then
        vitini = resoco(1:14)//'.VITI'
        accini = resoco(1:14)//'.ACCI'
        call vtcreb(vitini, numedd, 'V', 'R', neq)
        call vtcreb(accini, numedd, 'V', 'R', neq)
    endif
!
! --- OBJET DE SAUVEGARDE DE L ETAT DE CONTACT
!
    zetat = cfmmvd('ZETAT')
    etatct = resoco(1:14)//'.ETATCT'
    call wkvect(etatct, 'V V R', zetat*ntpc, jetat)
!
! - Creating data structures for cycling detection and treatment
!
    call mm_cycl_crsd(defico, resoco)
    call mm_cycl_init(defico, resoco)
!
! --- JEU TOTAL
!
    apjeu = resoco(1:14)//'.APJEU'
    call wkvect(apjeu, 'V V R', ntpc, japjeu)
!
! --- TOUTES LES ZONES EN INTEGRATION AUX NOEUDS ?
!
    lnoeu = cfdisl(defico,'ALL_INTEG_NOEUD')
    if (.not.lnoeu) then
        call utmess('A', 'CONTACT3_16')
    endif
!
    call jedema()
end subroutine
