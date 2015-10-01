subroutine rcZ200(sn, snet, fatigu, lrocht,&
                  mater, symax)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedetc.h"
#include "asterfort/rc32si.h"
#include "asterfort/rc32ma.h"
#include "asterfort/rcZ2in.h"
#include "asterfort/rcZ2cm.h"
#include "asterfort/rcZ2t.h"
#include "asterfort/rcZ2ac.h"
#include "asterfort/rcZ2rs.h"

    real(kind=8) :: symax
    aster_logical :: sn, snet, fatigu, lrocht
    character(len=8) :: mater
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_ZE200
!
! DEB ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
!              TRAITEMENT DES SITUATIONS (GROUPES, PASSAGE...)
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
!              RECUPERATION DES INDICES DE CONTRAINTES 
!              ET DES CARACTERISTIQUES DE LA TUYAUTERIE
!     ------------------------------------------------------------------
!
    call rcZ2in()
! 
!     ------------------------------------------------------------------
!              RECUPERATION DES CHARGES MECANIQUES
!     ------------------------------------------------------------------
!
    call rcZ2cm()
!
!     ------------------------------------------------------------------
!                  RECUPERATION DES TRANSITOIRES :
!                    - THERMIQUES(si RESU_THER)
!     ------------------------------------------------------------------
!
    call rcZ2t()
!
!     ------------------------------------------------------------------
!              CALCULS DES AMPLITUDES DE CONTRAINTES
!                  ET DU FACTEUR D'USAGE
!     ------------------------------------------------------------------
!
    call rcZ2ac(sn, snet, fatigu, lrocht,&
                mater)
!
!     ------------------------------------------------------------------
!                       STOCKAGE DES RESULTATS
!     ------------------------------------------------------------------
!
    call rcZ2rs(sn, snet, fatigu, lrocht,&
                mater, symax)
!
    call jedetc('V', '&&RC3200', 1)
!
end subroutine
