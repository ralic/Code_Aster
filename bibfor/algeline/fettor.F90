subroutine fettor(option, infofe, rang, nbi, irg1,&
                  irg, irp, irz, ifm)
! ----------------------------------------------------------------------
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
! person_in_charge: olivier.boiteau at edf.fr
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  TEST ORTHOGONALITE DU GCPPC DE ALGO FETI
!                          POUR SOULAGER ALFETI.F
!     ------------------------------------------------------------------
!     IN  OPTION : IN   : OPTION DE LA ROUTINE
!     IN  INFOFE : CH19 : CHAINE DE CHARACTERES POUR MONITORING FETI
!     IN  RANG   : IN   : RANG DU PROCESSEUR
!     IN  NBI    : IN   : TAILLE DE L'INTERFACE
!     IN  IRG1   : IN   : ADRESSE DE PRI-1 AVANT MAJ
!     IN  IRG    : IN   : IDEM APRES MAJ
!     IN  IRP    : IN   : ADRESSE DE DI-1
!     IN  IRZ    : IN   : ADRESSE DE FI*DI-1
!     IN  IFM    : IN   : UNITE LOGIQUE D'AFFICHAGE
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "blas/ddot.h"
    integer :: option, rang, nbi, irg1, irg, irp, irz, ifm
    character(len=24) :: infofe
!
! DECLARATION VARIABLES LOCALES
    real(kind=8) :: raux
!
! CORPS DU PROGRAMME
    call jemarq()
!
    if ((infofe(8:8).eq.'T') .and. (rang.eq.0)) then
        if (option .eq. 1) then
! TEST 1 ORTHOGONALITE DU GCPPC
            raux=ddot(nbi,zr(irg),1,zr(irg1),1)
            write(ifm,*)'RANG ',rang,' TEST <PRI,PRI-1>',raux
            raux=ddot(nbi,zr(irg),1,zr(irp),1)
            write(ifm,*)'RANG ',rang,' TEST <PRI,DI-1>',raux
        else if (option.eq.2) then
! TEST 2 ORTHOGONALITE DU GCPPC
            raux=ddot(nbi,zr(irp),1,zr(irz),1)
            write(ifm,*)'RANG ',rang,' TEST <DI,FI*DI-1>',raux
        else
            ASSERT(.false.)
        endif
    endif
!
    call jedema()
end subroutine
