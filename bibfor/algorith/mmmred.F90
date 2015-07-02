subroutine mmmred(ndimg, lctfc, champ, champr, ndd1)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/mmfield_prep.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    integer :: ndimg
    character(len=19) :: champ, champr
    aster_logical :: lctfc
    integer :: ndd1
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - POST-TRAITEMENT)
!
! REDUCTION DU CHAMP SUR LES DDL
!
! ----------------------------------------------------------------------
!
!
! IN  NDIMG  : DIMENSION DE L'ESPACE
! IN  LCTFC  : .TRUE. SI FROTTEMENT
! IN  CHAMP  : CHAM_NO A REDUIRE
! OUT CHAMPR : CHAM_NO_S REDUIT DE L'INCREMENT DE DEPLACEMENT CUMULE
! OUT NDD1   : NOMBRE DE DDL/NOEUD
!
!
!
!
    if (ndimg .eq. 3) then
        if (lctfc) then
            ndd1 = 6
            call mmfield_prep(champ, champr,&
                              l_sort_ = .true._1, nb_cmp_ = ndd1,&
                              list_cmp_ = ['DX      ','DY      ','DZ      ',&
                                           'LAGS_C  ','LAGS_F1 ','LAGS_F2 '])
        else
            ndd1 = 4
            call mmfield_prep(champ, champr,&
                              l_sort_ = .true._1, nb_cmp_ = ndd1,&
                              list_cmp_ = ['DX      ','DY      ','DZ      ',&
                                           'LAGS_C  '])
        endif
    else if (ndimg.eq.2) then
        if (lctfc) then
            ndd1 = 4
            call mmfield_prep(champ, champr,&
                              l_sort_ = .true._1, nb_cmp_ = ndd1,&
                              list_cmp_ = ['DX      ','DY      ',&
                                           'LAGS_C  ','LAGS_F1 '])
        else
            ndd1 = 3
            call mmfield_prep(champ, champr,&
                              l_sort_ = .true._1, nb_cmp_ = ndd1,&
                              list_cmp_ = ['DX      ','DY      ',&
                                           'LAGS_C  '])
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
