subroutine xmodfc(lact, nlact, nno, dfdic, dffc, ndim)
    implicit none
    
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"

! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: daniele.colombo at ifpen.fr

    integer :: i, j, k
    integer :: lact(16), nlact(2), nno, ndim
    real(kind=8) :: dfdic(nno,3), dffc(16,3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT AVEC XFEM
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
!
! POUR LA FORMULATION AUX NOEUDS SOMMET,  SI UN NOEUD N'EST PAS ACTIF,
! ON REPARTI SES DFF EQUITABLEMENT SUR LES NOEUDS ACTIF
!
! ----------------------------------------------------------------------
!
! IN  NNO    : NOMBRE DE NOEUD DE L'ELEMENT PARENT
! IN LACT    : LITE DES LAGRANGES ACTIFS
! IN NLACT   : NOMBRE TOTAL DE LAGRANGES ACTIFS
! IN DFDIC   : GRADIENT DES FONCTION DE FORMES DE L'ELEMENT PARENT
! OUT DFFC   : GRADIENT DES FONCTION DE FORMES DU POINT DE CONTACT
!
! ----------------------------------------------------------------------

    call jemarq()
!
    do i = 1, nno
       do k = 1, ndim
          dffc(i,k)=dfdic(i,k)
       end do
    end do
    if (nlact(1) .lt. nno) then
       do i = 1, nno
          if (lact(i) .eq. 0) then
             do j = 1, nno
                if (i .ne. j .and. lact(j) .ne. 0) then
                   do k = 1, ndim
                      dffc(j,k)=dffc(j,k)+dffc(i,k)/nlact(1)
                   end do
                endif
             end do
             do k = 1, ndim
                dffc(i,k)= 0.d0
             end do
          endif
       end do
    endif
!
    if (nno .gt. 8) then
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine
