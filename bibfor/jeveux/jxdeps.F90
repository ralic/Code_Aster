subroutine jxdeps(iadini, iadfin, lso)
! person_in_charge: j-pierre.lefebvre at edf.fr
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
!     DEPLACEMENT D'UNE ZONE MEMOIRE
!     ------------------------------------------------------------------
! IN  IADINI : ADRESSE INITIALE
! IN  IADFIN : ADRESSE CIBLE
! IN  LSO : LONGUEUR DE LA ZONE A DEPLACER
!     ------------------------------------------------------------------
    implicit none
!             ROUTINE AVEC ADHERENCE SYSTEME    CRAY
!             FONCTION(S) UTILISEE(S) : IAND
!
#include "jeveux_private.h"
    integer :: iadini, iadfin, lso
!     ------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!     ------------------------------------------------------------------
    integer :: mslois
    common /jenvje/  mslois
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
! DEB ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, jfin, jini
!-----------------------------------------------------------------------
    if (iand ( jk1zon + iadini - 1 , mslois ) .eq. 0 .and.&
        iand ( jk1zon + iadfin - 1 , mslois ) .eq. 0 .and. iand ( lso , mslois ) .eq. 0) then
        jini = ( jk1zon + iadini - 1 ) / lois + 1
        jfin = ( jk1zon + iadfin - 1 ) / lois + 1
        if (jini .gt. jfin) then
            do 20 i = 0, (lso / lois) - 1
                iszon( jfin + i ) = iszon( jini + i )
20          continue
        else if (jini .lt. jfin) then
            do 21 i = (lso / lois) - 1, 0, -1
                iszon( jfin + i ) = iszon( jini + i )
21          continue
        endif
    else
        if (iadini .gt. iadfin) then
            do 30 i = 0, lso - 1
                k1zon(jk1zon + iadfin + i) = k1zon(jk1zon + iadini + i)
30          continue
        else if (iadini .lt. iadfin) then
            do 31 i = lso-1, 0, - 1
                k1zon(jk1zon + iadfin + i) = k1zon(jk1zon + iadini + i)
31          continue
        endif
    endif
! FIN ------------------------------------------------------------------
end subroutine
