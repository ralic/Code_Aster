subroutine nmasva(sddyna, veasse, cnvado)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndynkk.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdebg.h"
#include "asterfort/vtaxpy.h"
#include "asterfort/vtzero.h"
    character(len=19) :: cnvado
    character(len=19) :: veasse(*)
    character(len=19) :: sddyna
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL DES COMPOSANTES DU VECTEUR SECOND MEMBRE
!  - CHARGEMENT DE TYPE NEUMANN
!  - CHARGEMENT VARIABLE AU COURS DU PAS DE TEMPS
!  - CHARGEMENT DONNE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! OUT CNVADO : VECT_ASSE DE TOUS LES CHARGEMENTS VARIABLES DONNES
!
!
!
!
    integer :: ifm, niv
    integer :: ifdo, n
    character(len=19) :: cnvari(20)
    real(kind=8) :: covari(20)
    real(kind=8) :: coeext, coeex2, coeint
    character(len=19) :: cnfsdo, cnfint
    aster_logical :: ldyna, lmpas
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ...... CALCUL NEUMANN VARIABLE'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lmpas = ndynlo(sddyna,'MULTI_PAS')
!
! --- INITIALISATIONS
!
    ifdo = 0
    call vtzero(cnvado)
!
! --- COEFFICIENTS POUR MULTI-PAS
!
    if (ldyna) then
        coeext = ndynre(sddyna,'COEF_MPAS_FEXT_PREC')
        coeex2 = ndynre(sddyna,'COEF_MPAS_FEXT_COUR')
        coeint = ndynre(sddyna,'COEF_MPAS_FINT_PREC')
    else
        coeext = 1.d0
        coeex2 = 1.d0
        coeint = 1.d0
    endif
!
! --- CALCUL DES FORCES EXTERIEURES VARIABLES
!
    call nmchex(veasse, 'VEASSE', 'CNFSDO', cnfsdo)
    ifdo = ifdo+1
    cnvari(ifdo) = cnfsdo
    covari(ifdo) = coeex2
!
! --- AJOUT FORCES EXTERIEURES VARIABLES PAS PRECEDENT
!
    if (lmpas) then
        call ndynkk(sddyna, 'OLDP_CNFSDO', cnfsdo)
        ifdo = ifdo+1
        cnvari(ifdo) = cnfsdo
        covari(ifdo) = coeext
    endif
!
! --- AJOUT FORCES INTERNES PAS PRECEDENT
!
    if (lmpas) then
        call ndynkk(sddyna, 'OLDP_CNFINT', cnfint)
        ifdo = ifdo+1
        cnvari(ifdo) = cnfint
        covari(ifdo) = -1.d0*coeint
    endif
!
! --- VECTEUR RESULTANT CHARGEMENT DONNE
!
    do 10 n = 1, ifdo
        call vtaxpy(covari(n), cnvari(n), cnvado)
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ......... FORC. DONNEES'
            write (ifm,*) '<MECANONLINE> .........  ',n,' - COEF: ',&
     &                   covari(n)
            call nmdebg('VECT', cnvari(n), ifm)
        endif
 10 end do
!
    call jedema()
end subroutine
