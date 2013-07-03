subroutine pccoef(n, in, ip, ac, icpl,&
                  icpc, acpc, cx)
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
!
!   ENTREE
!   N          : TAILLE DE A
!   IN,IP,AC   : MATRICE D'ENTREE FORMAT SYMETRIQUE
!   CX      : TRAVAIL
!   ICPL       : IDEM IN POUR LA MATRICE DE PRECOND.
!   ICPC       : IDEM IP POUR LA MATRICE DE PRECOND.
!
!   SORTIE
!   ACPC       : COEFS DE LA MATRICE DE PRECOND.
!--------------------------------------------------------
! aslint: disable=W1304
    implicit none
#include "jeveux.h"
#include "asterfort/jedetr.h"
#include "asterfort/wkvect.h"
    real(kind=8) :: ac(*)
    integer :: in(n)
    integer(kind=4) :: ip(*), icpc(*)
    real(kind=8) :: acpc(*), cx(n)
    integer :: icpl(0:n)
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
!
!
! AC ---> ACPC
! ==========================
!   MISE A 0 DE ACPC
!-----------------------------------------------------------------------
    integer :: i, imp, j, jind, k, k1, k2
    integer :: kk, kk1, kk2, n
!-----------------------------------------------------------------------
    kk2 = icpl(n-1)
    do 10 kk = 1, kk2
        acpc(kk) = 0.d0
10  end do
    call wkvect('&&PCCOEF.IND', 'V V I', n, jind)
!
    acpc(1) = ac(1)
    do 40 i = 2, n
!  LIGNE CREUSE I DE AC --> LIGNE PLEINE IND-CX
!                          (ICPL(I-1)=FIN LIGNE I)
        k1 = in(i-1) + 1
        k2 = in(i)
        do 20 k = k1, k2 - 1
            j = ip(k)
            zi(jind-1+j) = i
            cx(j) = ac(k)
20      continue
        kk1 = icpl(i-2) + 1
        kk2 = icpl(i-1)
        do 30 kk = kk1, kk2 - 1
            j = icpc(kk)
            if (zi(jind-1+j) .eq. i) acpc(kk) = cx(j)
30      continue
        acpc(kk2) = ac(k2)
40  end do
!
    imp = 0
    if (imp .eq. 1) then
!       WRITE (6,*) ' FIN DU S-P PCCOEF'
    endif
    call jedetr('&&PCCOEF.IND')
!
end subroutine
