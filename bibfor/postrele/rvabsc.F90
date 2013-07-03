subroutine rvabsc(mailla, tnd, nbn, tabsc, tcoor)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=8) :: mailla
    integer :: tnd(*), nbn
    real(kind=8) :: tabsc(*), tcoor(*)
!
!***********************************************************************
!
!  OPERATION REALISEE
!  ------------------
!
!     CALCUL DES ABSCISSES CURVILIGNES LE LONG D' UNE LISTE DE NOEUDS
!
!  ARGUMENTS EN ENTREE
!  -------------------
!
!     MAILLA : NOM DU MAILLAGE
!     TND    : TABLE DES NUMEROS DE NOEUDS
!     NBN    : NOMBRE DE NOEUDS
!
!  ARGUMENTS EN SORTIE
!  -------------------
!
!     TABSC : LE TABLEAU DES ABSCISSES
!     TCOOR : LE TABLEAU DES COORDONNEES (ORDRE X,Y,Z)
!
!***********************************************************************
!
!  -----------------------------------------
!
!
!  ---------------------------------
!
!  VARIABLES LOCALES
!  -----------------
!
    integer :: acoord, i
    real(kind=8) :: xc, xp, yc, yp, l, zzc, zzp
!
!==================== CORPS DE LA ROUTINE =============================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    call jeveuo(mailla//'.COORDO    .VALE', 'L', acoord)
!
    tabsc(1) = 0.0d0
!
    xp = zr(acoord + (tnd(1)-1)*3 + 1-1)
    yp = zr(acoord + (tnd(1)-1)*3 + 2-1)
    zzp = zr(acoord + (tnd(1)-1)*3 + 3-1)
!
    tcoor(1) = xp
    tcoor(2) = yp
    tcoor(3) = zzp
!
    do 10, i = 2, nbn, 1
!
    xc = zr(acoord + (tnd(i)-1)*3 + 1-1)
    yc = zr(acoord + (tnd(i)-1)*3 + 2-1)
    zzc = zr(acoord + (tnd(i)-1)*3 + 3-1)
!
    l = sqrt((xc-xp)*(xc-xp)+(yc-yp)*(yc-yp)+(zzc-zzp)*(zzc-zzp))
!
    tabsc(i) = tabsc(i-1) + l
!
    tcoor(3*(i-1) + 1) = xc
    tcoor(3*(i-1) + 2) = yc
    tcoor(3*(i-1) + 3) = zzc
!
    xp = xc
    yp = yc
    zzp = zzc
!
    10 end do
!
    call jedema()
end subroutine
