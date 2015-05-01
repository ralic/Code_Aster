subroutine te0009(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/infdis.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/upletr.h"
#include "asterfort/utmess.h"
#include "asterfort/utpalg.h"
    character(len=16) :: option, nomte
! ------------------------------------------------------------------
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
!     CALCUL DES MATRICES D'AMORTISSEMENT GYROSCOPIQUE
!     DES ELEMENTS DISCRETS :
!                             MECA_DIS_TR_N
!     ------------------------------------------------------------------
! IN  : OPTION : NOM DE L'OPTION A CALCULER
! IN  : NOMTE  : NOM DU TYPE_ELEMENT
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!
    integer :: nddlm, nl1, ipoint, lorien
    parameter     (nddlm=6,nl1=(nddlm+1)*nddlm/2)
    integer :: i, nc, nno, jdm, jdc, j, infodi, ibid
    real(kind=8) :: vxx, r8bid, pgl(3, 3), klv(nl1), klw(nl1)
    character(len=8) :: k8bid
!
!     ------------------------------------------------------------------
    call jemarq()
!
    if (nomte .eq. 'MECA_DIS_TR_N') then
!        ON VERIFIE QUE LES CARACTERISTIQUES ONT ETE AFFECTEES
!        LE CODE DU DISCRET
        call infdis('CODE', ibid, r8bid, nomte)
!        LE CODE STOKE DANS LA CARTE
        call infdis('TYDI', infodi, r8bid, k8bid)
        if (infodi .ne. ibid) then
            call utmess('F+', 'DISCRETS_25', sk=nomte)
            call infdis('DUMP', ibid, r8bid, 'F+')
        endif
!        DISCRET DE TYPE MASSE
        call infdis('DISM', infodi, r8bid, k8bid)
        if (infodi .eq. 0) then
            call utmess('A+', 'DISCRETS_26', sk=nomte)
            call infdis('DUMP', ibid, r8bid, 'A+')
        endif
        nc = 6
    else
        call utmess('F', 'CALCULEL_17')
    endif
!     OPTION DE CALCUL INVALIDE
    if (option .ne. 'MECA_GYRO') then
        ASSERT(.false.)
    endif
!
    call infdis('SYMM', infodi, r8bid, k8bid)
    call jevech('PCADISM', 'L', jdc)
    if (infodi .eq. 1) then
        vxx = zr(jdc+10-1)
    else if (infodi.eq.2) then
        vxx = zr(jdc+22-1)
    endif
    call jevech('PMATUNS', 'E', jdm)
!
    do 60 i = 1, nl1
        klv(i)=0.d0
 60 end do
!
!     I : LIGNE ; J : COLONNE
    i = 5
    j = 6
    ipoint = int(j*(j-1)/2)+i
    klv(ipoint) = -vxx
!
    call jevech('PCAORIE', 'L', lorien)
    call matrot(zr(lorien), pgl)
    nno = 1
    call utpalg(nno, nc, pgl, klv, klw)
    call upletr(nddlm, zr(jdm), klw)
!
    call jedema()
end subroutine
