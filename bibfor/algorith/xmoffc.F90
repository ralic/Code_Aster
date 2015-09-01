subroutine xmoffc(lact, nlact, nno, ffe, ffc)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "asterfort/assert.h"
    integer :: lact(*), nlact, nno
    real(kind=8) :: ffe(*), ffc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT AVEC XFEM
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
!
! POUR LA FORMULATION AUX NOEUDS SOMMET,  SI UN NOEUD N'EST PAS ACTIF,
! ON REPARTI SES FF EQUITABLEMENT SUR LES NOEUDS ACTIF
!
! ----------------------------------------------------------------------
!
! IN  NNO    : NOMBRE DE NOEUD DE L'ELEMENT PARENT
! IN LACT    : LITE DES LAGRANGES ACTIFS
! IN NLACT   : NOMBRE TOTAL DE LAGRANGES ACTIFS
! IN FFE     : FONCTION DE FORMES DE L'ELEMENT ESCLAVE PARENT
! OUT FFC    : FONCTION DE FORMES DU POINT DE CONTACT
!
! ----------------------------------------------------------------------
!
    integer :: i, j
!
! ----------------------------------------------------------------------
!
!
    do 10 i = 1, nno
        ffc(i)=ffe(i)
10  end do
    if (nlact .lt. nno) then
        do 20 i = 1, nno
            if (lact(i) .eq. 0) then
                do 30 j = 1, nno
                    if (i .ne. j .and. lact(j) .ne. 0) then
                        ffc(j)=ffc(j)+ffc(i)/nlact
                    endif
30              continue
                ffc(i)= 0.d0
            endif
20      continue
    endif
!
    if (nno .gt. 8) then
        ASSERT(.false.)
    endif
!
end subroutine
