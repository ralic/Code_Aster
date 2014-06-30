subroutine rc3200(pmpb, sn, snet, fatigu, lrocht,&
                  mater, symax)
    implicit   none
#include "asterfort/jedetc.h"
#include "asterfort/rc32ac.h"
#include "asterfort/rc32cm.h"
#include "asterfort/rc32ma.h"
#include "asterfort/rc32mu.h"
#include "asterfort/rc32rs.h"
#include "asterfort/rc32si.h"
#include "asterfort/rc32th.h"
    real(kind=8) :: symax
    logical(kind=1) :: pmpb, sn, snet, fatigu, lrocht
    character(len=8) :: mater
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!
! DEB ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
!                           LES SITUATIONS
!     ------------------------------------------------------------------
!
    call rc32si()
!
!     ------------------------------------------------------------------
!              RECUPERATION DES CARACTERISTIQUES MATERIAU
!     ------------------------------------------------------------------
!
    call rc32ma(mater)
!
!     ------------------------------------------------------------------
!              RECUPERATION DU CALCUL MECANIQUE UNITAIRE
!     ------------------------------------------------------------------
!
    call rc32mu()
!
!     ------------------------------------------------------------------
!              RECUPERATION DES CHARGES MECANIQUES
!     ------------------------------------------------------------------
!
    call rc32cm()
!
!     ------------------------------------------------------------------
!              RECUPERATION DES RESULTATS THERMIQUES
!     ------------------------------------------------------------------
!
    call rc32th()
!
!     ------------------------------------------------------------------
!              CALCULS DES AMPLITUDES DE CONTRAINTES
!     ------------------------------------------------------------------
!
!     CALCUL DES AMPLITUDES DE CONTRAINTES QUI CORRESPONDENT AUX
!     COMBINAISONS DE TOUS LES ETATS STABILISES APPARTENANT AUX
!     SITUATIONS D'UN GROUPE DONNE
!
! --- CALCUL DES AMPLITUDES DE CONTRAINTES
!     CALCUL DU FACTEUR D'USAGE
!     -------------------------
!
    call rc32ac(pmpb, sn, snet, fatigu, lrocht,&
                mater)
!
!
! --- STOCKAGE DES RESULTATS
!     ----------------------
!
    call rc32rs(pmpb, sn, snet, fatigu, lrocht,&
                mater, symax)
!
    call jedetc('V', '&&RC3200', 1)
!
end subroutine
