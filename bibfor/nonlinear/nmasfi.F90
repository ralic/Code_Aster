subroutine nmasfi(fonact, sddyna, veasse, cnffdo, cnffpi)
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
    implicit none
#include "jeveux.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndynkk.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdebg.h"
#include "asterfort/vtaxpy.h"
#include "asterfort/vtzero.h"
    character(len=19) :: cnffdo, cnffpi
    character(len=19) :: veasse(*)
    integer :: fonact(*)
    character(len=19) :: sddyna
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL DES COMPOSANTES DU VECTEUR SECOND MEMBRE
!  - CHARGEMENT DE TYPE NEUMANN
!  - CHARGEMENT FIXE AU COURS DU PAS DE TEMPS
!  - CHARGEMENT DONNE ET PILOTE
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  SDDYNA : SD DYNAMIQUE
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! OUT CNFFDO : VECT_ASSE DE TOUTES LES FORCES FIXES DONNES
! OUT CNFFPI : VECT_ASSE DE TOUTES LES FORCES FIXES PILOTES
!
!
!
!
!
    integer :: ifm, niv
    integer :: ifdo
    integer :: n
    character(len=19) :: cndonn(20)
    real(kind=8) :: codonn(20)
    real(kind=8) :: coeext, coeex2
    character(len=19) :: cnfedo, cnlame, cnondp, cnfepi
    character(len=19) :: cnsstf, cnviss
    logical(kind=1) :: llapl
    logical(kind=1) :: londe, lpilo, lsstf, lmpas, ldyna, lviss
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ...... CALCUL NEUMANN CONSTANT'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    londe = ndynlo(sddyna,'ONDE_PLANE')
    llapl = isfonc(fonact,'LAPLACE')
    lpilo = isfonc(fonact,'PILOTAGE')
    lsstf = isfonc(fonact,'SOUS_STRUC')
    lmpas = ndynlo(sddyna,'MULTI_PAS')
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lviss = ndynlo(sddyna,'VECT_ISS')
!
! --- INITIALISATIONS
!
    ifdo = 0
    call vtzero(cnffdo)
    call vtzero(cnffpi)
!
! --- COEFFICIENTS POUR MULTI-PAS
!
    if (ldyna) then
        coeext = ndynre(sddyna,'COEF_MPAS_FEXT_PREC')
        coeex2 = ndynre(sddyna,'COEF_MPAS_FEXT_COUR')
    else
        coeext = 1.d0
        coeex2 = 1.d0
    endif
!
! --- FORCES DONNEES
!
    call nmchex(veasse, 'VEASSE', 'CNFEDO', cnfedo)
    ifdo = ifdo+1
    cndonn(ifdo) = cnfedo
    codonn(ifdo) = coeex2
!
! --- CHARGEMENTS FORCES DE LAPLACE
!
    if (llapl) then
        call nmchex(veasse, 'VEASSE', 'CNLAPL', cnlame)
        ifdo = ifdo+1
        cndonn(ifdo) = cnlame
        codonn(ifdo) = coeex2
    endif
!
! --- CHARGEMENTS ONDE_PLANE
!
    if (londe) then
        call nmchex(veasse, 'VEASSE', 'CNONDP', cnondp)
        ifdo = ifdo+1
        cndonn(ifdo) = cnondp
        codonn(ifdo) = -1.d0*coeex2
    endif
!
! --- FORCES ISSUES DU CALCUL PAR SOUS-STRUCTURATION
!
    if (lsstf) then
        call nmchex(veasse, 'VEASSE', 'CNSSTF', cnsstf)
        ifdo = ifdo+1
        cndonn(ifdo) = cnsstf
        codonn(ifdo) = 1.d0
    endif
!
! --- FORCES VEC_ISS
!
    if (lviss) then
        call nmchex(veasse, 'VEASSE', 'CNVISS', cnviss)
        ifdo = ifdo+1
        cndonn(ifdo) = cnviss
        codonn(ifdo) = 1.d0
    endif
!
! --- AJOUT FORCES EXTERNES PAS PRECEDENT
!
    if (lmpas) then
        call ndynkk(sddyna, 'OLDP_CNFEDO', cnfedo)
        ifdo = ifdo+1
        cndonn(ifdo) = cnfedo
        codonn(ifdo) = coeext
        if (llapl) then
            call ndynkk(sddyna, 'OLDP_CNLAPL', cnlame)
            ifdo = ifdo+1
            cndonn(ifdo) = cnlame
            codonn(ifdo) = coeext
        endif
        if (londe) then
            call ndynkk(sddyna, 'OLDP_CNONDP', cnondp)
            ifdo = ifdo+1
            cndonn(ifdo) = cnondp
            codonn(ifdo) = coeext
        endif
    endif
!
! --- VECTEUR RESULTANT FORCES DONNEES
!
    do 10 n = 1, ifdo
        call vtaxpy(codonn(n), cndonn(n), cnffdo)
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ......... FORC. DONNEES'
            write (ifm,*) '<MECANONLINE> .........  ',n,' - COEF: ',&
     &                 codonn(n)
            call nmdebg('VECT', cndonn(n), ifm)
        endif
10  end do
!
! --- VECTEUR RESULTANT FORCES PILOTEES
!
    if (lpilo) then
        call nmchex(veasse, 'VEASSE', 'CNFEPI', cnfepi)
        cnffpi = cnfepi
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ......... FORC. PILOTEES'
            write (ifm,*) '<MECANONLINE> .........  ',1,' - COEF: ',&
     &                 1.d0
            call nmdebg('VECT', cnffpi, ifm)
        endif
    endif
!
    call jedema()
end subroutine
