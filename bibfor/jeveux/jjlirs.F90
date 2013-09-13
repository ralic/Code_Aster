subroutine jjlirs(jadm, iclas, idos, ius, ist)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: j-pierre.lefebvre at edf.fr
    implicit none
#include "jeveux_private.h"
#include "asterfort/utmess.h"
    integer :: jadm, iclas, ius, ist
! ----------------------------------------------------------------------
!     RELIT LES ENTIERS ENCADRANT UN SEGMENT DE VALEURS
!
! IN  JADM   : ADRESSE DU PREMIER MOT DU SEGMENT DE VALEUR
! IN  ICLAS  : CLASSE DE L'OBJET JEVEUX
! IN  IDOS   : IDENTIFICATEUR D'OBJET SIMPLE OU D'OBJET DE COLLECTION
! OUT IUS    : USAGE DU SEGMENT DE VALEUR
! OUT IST    : STATUT DU SEGMENT DE VALEUR
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ----------------------------------------------------------------------
    integer :: istat
    common /istaje/  istat(4)
! DEB ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: icla2, idatoc, idos, is, ista1, ista2
!
!-----------------------------------------------------------------------
    ista1 = iszon(jiszon+jadm-1)
    idatoc = iszon(jiszon+jadm-2)
    if (idatoc .ne. idos) then
        call utmess('F', 'JEVEUX1_54', si=jadm)
    endif
!
    if (ista1 .ne. istat(1) .and. ista1 .ne. istat(2)) then
        call utmess('F', 'JEVEUX1_54', si=jadm)
    endif
!
    is = jiszon+iszon(jiszon+jadm-4)
    ista2 = iszon(is-4)
    icla2 = iszon(is-2)
    if (icla2 .ne. iclas) then
        call utmess('F', 'JEVEUX1_55', si=jadm)
    endif
!
    if (ista2 .ne. istat(3) .and. ista2 .ne. istat(4)) then
        call utmess('F', 'JEVEUX1_55', si=jadm)
    endif
!
    ius = ista1
    ist = ista2
! FIN ------------------------------------------------------------------
end subroutine
