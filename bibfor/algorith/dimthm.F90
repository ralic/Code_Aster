subroutine dimthm(ndlno, ndlnm, ndim)
    implicit none
#include "asterf_types.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
#include "asterfort/lteatt.h"
    integer :: ndlno, ndlnm, ndim
    aster_logical :: elsufm
! ======================================================================
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
! ======================================================================
! --- BUT: CALCUL DU NOMBRE DE DDL SUR CHAQUE TYPE DE NOEUDS POUR LES --
! --- ELEMENTS DE BORD EN THM ------------------------------------------
! ======================================================================
!     ELSUFM VRAI SI ELEMENT SUSHI OU EFMH
!     NDDLNO NOMBRE DE DDL DES NOEUDS EXTREMITE DE SEGMENTS
!     NDDLM  NOMBRE DE DDL DES NOEUDS MILIEU DE SEGMENTS OU FACE
    elsufm= lteatt('MODTHM','SUSHI')
!
    if (elsufm) then
! ======================================================================
! --- SI MODELISATION = SUSHI   ----------------------------------------
! ======================================================================
        ndlno = 0
        ndlnm = 2
    else if (lteatt('MODTHM','THHM')) then
! ======================================================================
! --- SI MODELISATION = THHM -------------------------------------------
! ======================================================================
        ndlno = ndim+3
        ndlnm = ndim
    else if (lteatt('MODTHM','THH2M')) then
! ======================================================================
! --- SI MODELISATION = THH2M ------------------------------------------
! ======================================================================
        ndlno = ndim +3
        ndlnm = ndim
    else if (lteatt('MODTHM','HM')) then
! ======================================================================
! --- SI MODELISATION = HM ---------------------------------------------
! ======================================================================
        ndlno = ndim+1
        ndlnm = ndim
    else if (lteatt('MODTHM','HHM')) then
! ======================================================================
! --- SI MODELISATION = HHM --------------------------------------------
! ======================================================================
        ndlno = ndim+2
        ndlnm = ndim
    else if (lteatt('MODTHM','HH2M')) then
! ======================================================================
! --- SI MODELISATION = HH2M -------------------------------------------
! ======================================================================
        ndlno = ndim+2
        ndlnm = ndim
    else if (lteatt('MODTHM','THH')) then
! ======================================================================
! --- SI MODELISATION = THH --------------------------------------------
! ======================================================================
        ndlno = 3
        ndlnm = 0
    else if (lteatt('MODTHM','THH2')) then
! ======================================================================
! --- SI MODELISATION = THH2 -------------------------------------------
! ======================================================================
        ndlno = 3
        ndlnm = 0
    else if (lteatt('MODTHM','HH')) then
! ======================================================================
! --- SI MODELISATION = HH_ -------------------------------------------
! ======================================================================
        ndlno = 2
        ndlnm = 0
    else if (lteatt('MODTHM','H')) then
! ======================================================================
! --- SI MODELISATION = H_ --------------------------------------------
! ======================================================================
        ndlno = 1
        ndlnm = 0
    else if (lteatt('MODTHM','HH2')) then
! ======================================================================
! --- SI MODELISATION = HH2 -------------------------------------------
! ======================================================================
        ndlno = 2
        ndlnm = 0
    else if (lteatt('MODTHM','THV')) then
! ======================================================================
! --- SI MODELISATION = THV --------------------------------------------
! ======================================================================
        ndlno = 2
        ndlnm = 0
    else if (lteatt('MODTHM','THM')) then
! ======================================================================
! --- SI MODELISATION = THM --------------------------------------------
! ======================================================================
        ndlno = ndim+2
        ndlnm = ndim
    else
        call utmess('F', 'ALGORITH3_8')
    endif
! =====================================================================
end subroutine
