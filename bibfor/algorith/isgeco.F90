subroutine isgeco(icod1, icod2, ndim, iopt, icod)
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
!***********************************************************************
!    P. RICHARD     DATE 19/02/91
!-----------------------------------------------------------------------
!  BUT:  GERER L'ADDITION OU LA SOUSTRACTION DES DEUX ENTIER CODES SUR
!   LES 7 PREMIERES PUISSANCE ENTIERES DE DEUX
    implicit none
!
!   SI IOPT=1     ADDITION ICOD1+ICOD2
!   SI IOPT=-1     ICOD1-ICOD2
!-----------------------------------------------------------------------
!
! ICOD1    /I/: PREMIER ENTIER CODE
! ICOD2    /I/: DEUXIEME  ENTIER CODE
! NDIM     /I/: NOMBRE DE PUISSANCE A DECODER
! IOPT     /I/: OPTION DE CALCUL
! ICOD     /O/: RESULTAT
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
#include "asterfort/iscode.h"
#include "asterfort/isdeco.h"
    integer :: i, ik, iopt, nbcpmx, ndim
!-----------------------------------------------------------------------
    parameter (nbcpmx=300)
    integer :: icod1(1), icod2(1), icod(1)
    integer :: idec1(nbcpmx), idec2(nbcpmx), idec(nbcpmx)
!
!-----------------------------------------------------------------------
!
    call isdeco(icod1, idec1, ndim)
    call isdeco(icod2, idec2, ndim)
!
    if (iopt .eq. 1) then
        do 10 i = 1, ndim
            ik=idec1(i)+idec2(i)
            if (ik .gt. 0) then
                idec(i)=1
            else
                idec(i)=0
            endif
10      continue
    endif
!
    if (iopt .eq. -1) then
        do 20 i = 1, ndim
            ik=idec1(i)-idec2(i)
            if (ik .gt. 0) then
                idec(i)=1
            else
                idec(i)=0
            endif
20      continue
    endif
!
    call iscode(idec, icod, ndim)
!
end subroutine
