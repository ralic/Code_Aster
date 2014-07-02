function arlelt(nomte, mod, cin)
!
!
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
    implicit none
#include "asterf_types.h"
    aster_logical :: arlelt
    character(len=16) :: nomte
    character(len=16) :: mod
    character(len=16) :: cin
!
! ----------------------------------------------------------------------
!
! ROUTINE ARLEQUIN
!
! RETOURNE .TRUE. SI LE TYPE D'ELEMENT EST RECONNU COMME ETANT VALIDE
! POUR ARLEQUIN ET DONNE LE TYPE DE MODELISATION ET DE CINEMATIQUE
!
! ----------------------------------------------------------------------
!
!
! NB: EXCLU LES BORDS/ARETES
!
! IN  NOMTE  : NOM DU TE
! OUT MOD    : TYPE DE MODELISATION
!              'DPLAN'  ELEMENT DE DEFORMATIONS PLANES
!              'CPLAN'  ELEMENT DE CONTRAINTES PLANES
!              'AXIS'   ELEMENT AXISYMETRIQUE
!              '3D'     ELEMENT 3D POUR SOLIDE
!                       DKT/DST/COQUE_3D/Q4G POUR COQUE
! OUT CIN    : TYPE DE CINEMATIQUE
!              'SOLIDE'
!              'POUTRE'
!
! ----------------------------------------------------------------------
    arlelt = .false.
!
    if (nomte(1:5) == 'MECA_') then
        mod = '3D'
        if ((nomte(6:10) == 'PENTA') .or. (nomte(6:9) == 'HEXA') .or.&
            (nomte(6:10) == 'TETRA')) then
            cin = 'SOLIDE'
            arlelt = .true.
        else if (nomte(6:8) == 'POU') then
            cin = 'POUTRE'
            arlelt = .true.
        else
            arlelt = .false.
        endif
    else
        arlelt = .false.
    endif
!
end function
