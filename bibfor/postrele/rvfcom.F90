subroutine rvfcom(nmaila, m1, f1, m2, f2)
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
#include "asterfort/i2extf.h"
#include "asterfort/u2mess.h"
    character(len=8) :: nmaila
    integer :: m1, m2, f1, f2
!
!***********************************************************************
!
!  OPERATION REALISEE
!  ------------------
!
!     ETANT DONNEES DEUX MAILLES M1 ET M2, ET UNE FACE F1 DE M1,TROUVER
!     LA FACE F2 DE M2 QUI COINCIDE AVEC F1
!
!  ARGUMENTS EN ENTREE
!  -------------------
!
!     NMAILA : NOM DU MAILLAGE
!
!     M1, M2 : NUMERO DES MAILLES (2D)
!
!     F1     : NUMERO LOCALE DE LA FACE (1D) DE M1
!
!  ARGUMENTS EN ENTREE
!  -------------------
!
!     F2 : NUMERO LOCAL DE LA FACE DE M2 CHERCHEE AVEC LA CONVENTION
!             F2 > 0 : F2 ET F1 DE MEME ORIENTATION
!             F2 < 0 : F2 ET F1 D' ORIENTATION OPPOSEE
!
!***********************************************************************
!
!  FONCTIONS EXTERNES
!  ------------------
!
!
!  -----------------------------------------
!
!
!  ---------------------------------
!
!  VARIABLES LOCALES
!  -----------------
!
    character(len=16) :: ntype
    character(len=15) :: nconec
!
    integer :: n1g, n1d, n2g, n2d
!
    logical :: trouve
!
!==================== CORPS DE LA ROUTINE =============================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    nconec = nmaila//'.CONNEX'
    ntype = nmaila//'.TYPMAIL'
!
    call i2extf(m1, f1, nconec, ntype, n1g,&
                n1d)
!
    f2 = 0
!
    trouve = .false.
!
10  continue
    if (.not. trouve) then
!
        f2 = f2 + 1
!
        if (f2 .gt. 12) call u2mess('F', 'POSTRELE_19')
!
        call i2extf(m2, f2, nconec, ntype, n2g,&
                    n2d)
!
        if ((n1g .eq. n2g) .and. ( n1d .eq. n2d)) then
!
            trouve = .true.
!
        else if ((n1d .eq. n2g) .and. ( n1g .eq. n2d)) then
!
            trouve = .true.
!
            f2 = -f2
!
        else
!
!
        endif
!
        goto 10
!
    endif
!
end subroutine
