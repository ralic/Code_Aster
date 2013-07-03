function nufloc(ndim, nsc, isc)
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    integer :: nufloc
! DONNE LE NUMERO  LOCAL DE LA FACE DONT LES NUMEROS LOCAUX DE SOMMETS
! SONT ISC
!
!
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/u2mesk.h"
    integer :: ndim, nsc, isc(1:nsc)
!
    character(len=8) :: elrefe
    integer :: fa, cle
!
    call elref1(elrefe)
!
    if (ndim .eq. 2) then
        call assert(nsc.eq.2)
        call assert(isc(1).ne.isc(2))
!
!  ON NE SAIT PAS DANS QUEL ORDRE SONT DONNES LES SOMETS VOISINS
!  SI ABS(ISC1-ISC2)=1 ON N'EST PAS SUR LE DERNIER SOMMET ET IFA =
!     LE PLUS PETIT
!  SI ABS(ISC1-ISC2)>1 ON EST SUR LE DERNIER SOMMET ET IFA =
!     LE PLUS GRAND
!
        if (abs(isc(1)-isc(2)) .eq. 1) then
            fa= min(isc(1),isc(2))
        else
            fa= max(isc(1),isc(2))
        endif
    else
        if (elrefe .eq. 'H27') then
            call assert(nsc.eq.4)
            cle = isc(1)**2+isc(2)**2+isc(3)**2+isc(4)**2
            if (cle .eq. 30) then
                fa = 1
            else if (cle.eq.66) then
                fa = 2
            else if (cle.eq.98) then
                fa = 3
            else if (cle.eq.90) then
                fa = 4
            else if (cle.eq.106) then
                fa = 5
            else if (cle.eq.174) then
                fa = 6
            else
                call assert(.false.)
            endif
        else if (elrefe.eq.'T9') then
            call assert(nsc.eq.3)
            fa=10-isc(1)-isc(2)-isc(3)
            call assert(fa.ge.1.and.fa.le.4)
        else
            call u2mesk('F', 'VOLUFINI_12', 1, elrefe)
        endif
    endif
    nufloc=fa
end function
