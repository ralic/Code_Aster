subroutine mnlbra(xups, xfpnla, ninc, ordman, nbpt,&
                  epsman, amax, xus)
    implicit none
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!
!     MODE_NON_LINE CALCUL D'UNE BRANCHE
!     -    -   -                 ---
! ----------------------------------------------------------------------
!
! CALCUL UNE BRANCHE A L'AIDE DES COEFFICIENTS DE LA SERIE ENTIERE
! ----------------------------------------------------------------------
! IN   XUPS   : K14  : NOM DU VECTEUR QUI CONTIENT LES COEFFICIENTS
!                       DE LA SERIE ENTIERE DE LA VARIABLE
! IN   XFPNLA : K14  : NOM DU VECTEUR QUI CONTIENT LE SECOND MEMBRE
!                       SUPPLEMENTAIRE
! IN   NINC   : I    : NOMBRE D INCONNUES DU SYSTEME
! IN   ORDMAN : I    : ORDRE DE LA MAN
! IN   NBPT   : I    : DISCRETISATION DE LA BRANCHE
! IN   EPSMAN : R8   : PRECISION POUR L'ALGORITHME MAN
! OUT  AMAX   : R8   : LONGUEUR D'ARC DE LA BRANCHE
! OUT  XUS    : K14  : BRANCHE SOLUTION
! ----------------------------------------------------------------------
!
!
#include "jeveux.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dnrm2.h"
#include "blas/dscal.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"

    character(len=14) :: xups, xfpnla, xus
    integer :: ninc, ordman, nbpt
    real(kind=8) :: epsman, amax
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    integer :: ius, iups, ifpnla, i, k
    real(kind=8) :: norme, a
!
    call jemarq()
! ----------------------------------------------------------------------
! --- RECUPERATION DU POINTEUR DE LA BRANCHE
! ----------------------------------------------------------------------
    call jeveuo(xus, 'E', ius)
    call dscal(ninc*nbpt, 0.d0, zr(ius), 1)
! ----------------------------------------------------------------------
! --- RECUPERATION DES COEFFICIENTS DE LA SERIE ENTIERE
! ----------------------------------------------------------------------
    call jeveuo(xups, 'L', iups)
! ----------------------------------------------------------------------
! --- RECUPERATION DU SECOND MEMBRE SUPPLEMENTAIRE
! ----------------------------------------------------------------------
    call jeveuo(xfpnla, 'L', ifpnla)
    norme = dnrm2(ninc-1,zr(ifpnla),1)
!
    if (norme .eq. 0.d0) then
        call utmess('F', 'MECANONLINE9_61')
    else
        amax = (epsman/norme)**dble(1.d0/(ordman+1))
    endif
! ----------------------------------------------------------------------
! --- ON RECOPIE LE POINT INITIAL
! ----------------------------------------------------------------------
    call dcopy(ninc, zr(iups), 1, zr(ius), 1)
! ----------------------------------------------------------------------
! --- ON CALCUL LA BRANCHE
! ----------------------------------------------------------------------
    do i = 2, nbpt
        a=amax*dble(i-1)/dble(nbpt-1)
        do k = 0, ordman
            call daxpy(ninc, a**dble(k), zr(iups+k*ninc), 1, zr(ius+(i-1)*ninc),&
                       1)
        end do
    end do
!
    call jedema()
!
end subroutine
