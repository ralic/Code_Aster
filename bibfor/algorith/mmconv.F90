subroutine mmconv(noma, defico, resoco, valinc, solalg,&
                  vfrot, nfrot, vgeom, ngeom)
!
    implicit     none
!
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/cfdisl.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mmmcrf.h"
#include "asterfort/mmmcrg.h"
#include "asterfort/mmreas.h"
#include "asterfort/mreacg.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmimck.h"
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
    character(len=19), intent(in) :: valinc(*)
    character(len=19), intent(in) :: solalg(*)
    real(kind=8), intent(out) :: vfrot
    character(len=16), intent(out) :: nfrot
    real(kind=8), intent(out) :: vgeom
    character(len=16), intent(out) :: ngeom
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE)
!
! RESIDUS SPECIFIQUES POUR NEWTON GENERALISE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! OUT VFROT  : VALEUR NORME_MAX POUR RESI_FROT
! OUT NFROT  : LIEU OU VALEUR NORME_MAX POUR RESI_FROT
! OUT VGEOM  : VALEUR NORME_MAX POUR RESI_GEOM
! OUT NGEOM  : LIEU OU VALEUR NORME_MAX POUR RESI_GEOM
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=19) :: depplu, depmoi, ddepla
    logical :: lnewtf, lnewtg
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- INITIALISATIONS
!
    nfrot = ' '
    vfrot = r8vide()
    ngeom = ' '
    vgeom = r8vide()
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(solalg, 'SOLALG', 'DDEPLA', ddepla)
!
! --- FONCTIONNALITES ACTIVEES
!
    lnewtf = cfdisl(defico,'FROT_NEWTON')
    lnewtg = cfdisl(defico,'GEOM_NEWTON')
!
! --- AFFICHAGE
!
    if ((niv.ge.2) .and. (lnewtg.or.lnewtf)) then
        write (ifm,*) '<CONTACT> ... CALCUL DES RESIDUS POUR NEWTON '//&
     &                'GENERALISE'
    endif
!
! --- EVALUATION RESIDU SEUIL FROTTEMENT
!
    if (lnewtf) then
!
! ----- MISE A JOUR DES SEUILS
!
        call mmreas(noma, defico, resoco, valinc)
!
! ----- CALCUL RESIDU DE FROTTEMENT
!
        call mmmcrf(noma, ddepla, depplu, nfrot, vfrot)
    endif
!
! --- EVALUATION RESIDU SEUIL GEOMETRIE
!
    if (lnewtg) then
!
! ----- MISE A JOUR DE LA GEOMETRIE
!
        call mreacg(noma, resoco)
!
! ----- CALCUL RESIDU DE GEOMETRIE
!
        call mmmcrg(noma, ddepla, depplu, ngeom, vgeom)
    endif
!
    call jedema()
end subroutine
