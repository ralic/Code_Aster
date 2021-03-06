subroutine i2fspl(tvois2, tplace, n, existe, adrdbt)
    implicit none
#include "asterf_types.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!*****************************************************************
!
!        RECHERCHE DE LA PRESENCE D' UN CHEMIN SIMPLE
!        NON ENCORE EXPLOITE DANS UN GROUPE DE MAILLES.
!
!        TVOIS2 (IN)  : TABLE DES SECONDS VOISINS
!
!        N      (IN)  : NBR DE MAILLES DU GROUPE
!
!        TPLACE (IN)  : TABLE DES MAILLES DU GROUPE DEJA PLACEES
!
!        EXISTE (OUT) : INDICATEUR DE PRESENCE
!
!        ADRDBT (I-O) : PREMIERE MAILLE DU CHEMIN TROUVE
!
!*****************************************************************
!
    aster_logical :: existe, tplace(*)
    integer :: tvois2(*), n, adrdbt
!
    integer :: i
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    existe = .false.
!
    i = adrdbt + 1
!
 10 continue
    if ((.not. existe) .and. (i .le. n)) then
!
        if (.not. tplace(i)) then
!
            if (tvois2(i) .eq. 0) then
!
                existe = .true.
                adrdbt = i
!
            else
!
                i = i + 1
!
            endif
!
        else
!
            i = i + 1
!
        endif
!
        goto 10
!
    endif
!
end subroutine
