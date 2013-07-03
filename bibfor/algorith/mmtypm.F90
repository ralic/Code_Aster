subroutine mmtypm(noma, numma, nnosd, alias, ndim)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mmelty.h"
    integer :: numma
    integer :: nnosd, ndim
    character(len=8) :: noma
    character(len=8) :: alias
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - UTILITAIRE)
!
! TYPE DE LA MAILLE
!
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NUMMA  : NUMERO ABSOLU DE LA MAILLE
! IN  NNOSD  : NOMBRE DE NOEUDS DE LA MAILLE SUIVANT LA SD
! OUT NDIM   : DIMENSION DE LA MAILLE
! OUT ALIAS  : TYPE GEOMETRIQUE DE LA MAILLE
!
! /!\  NNO EST DIFFERENT DE NNOSD
!        NNOSD : NOMBRE DE NOEUDS SUPPORTANT SUIVANT LA SD
!                REPREND UNIQUEMENT LES NOEUDS SUPPORTANT
!                DES DDLS DE DEPLACEMENTS
!        NNO   : NOMBRE DE NOEUDS TOTAL DE LA MAILLE
!
!
!
!
    integer :: nno
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- TYPE GEOMETRIQUE DE LA MAILLE
!
    call mmelty(noma, numma, alias, nno, ndim)
!
! --- CAS DES COQUES_3D: PAS DE DX/DY/DZ SUR NOEUD MILIEU
!
    if ((nno .eq.9) .and. (nnosd .eq.8)) then
        alias = 'QU8'
    endif
    if ((nno .eq.7) .and. (nnosd .eq.6)) then
        alias = 'TR6'
    endif
!
! --- CAS DES QUAD8 EN 3D: 4 NOEUDS ET RELATIONS LINEAIRES
!
    if ((nno .eq.8) .and. (nnosd .eq.4)) then
        alias = 'QU4'
    endif
!
    call jedema()
!
end subroutine
