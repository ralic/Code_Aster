subroutine listun(noma, motfac, nzocu, nopono, nnocu,&
                  nolino)
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
!
    implicit     none
#include "jeveux.h"
#include "asterfort/exnode.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nbnode.h"
    character(len=8) :: noma
    character(len=16) :: motfac
    integer :: nzocu
    character(len=24) :: nopono, nolino
    integer :: nnocu
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (LIAISON_UNILATERALE - LECTURE)
!
! STOCKAGE DES NOEUDS
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  MOTFAC : MOT_CLEF FACTEUR POUR LIAISON UNILATERALE
! IN  NZOCU  : NOMBRE DE ZONES DE LIAISON_UNILATERALE
! OUT NOPONO : NOM DE L'OBJET JEVEUX CONTENANT LE VECTEUR D'INDIRECTION
! OUT NOLINO : NOM DE L'OBJET JEVEUX CONTENANT LA LISTE DES NOEUDS
! OUT NNOCU  : NOMBRE DE TOTAL DE NOEUDS POUR TOUTES LES OCCURRENCES
!
!
!
!
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- COMPTE DES NOEUDS
!
    call nbnode(noma, motfac, nzocu, nopono, nnocu)
!
! --- LECTURE DES NOEUDS
!
    call exnode(noma, motfac, nzocu, nnocu, nolino)
!
    call jedema()
end subroutine
