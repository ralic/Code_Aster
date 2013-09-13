subroutine nodoub(nbl, nbb, nol, nob, typl,&
                  typb, mailla, double)
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
!***********************************************************************
!    P. RICHARD     DATE 16/07/90
!-----------------------------------------------------------------------
!  BUT: COMPARER DEUX LISTES DE NUMEROS DE NOEUDS PAR ORDRE CROISSANT ET
    implicit none
!         DETECTER LES ELEMENTS COMMUNS
!         ARRET EN CAS D'INTERSECTION NON VIDE
!-----------------------------------------------------------------------
!
! NBL      /I/: NOMBRE DE NOEUDS INTERFACE LIBRE
! NBB      /I/: NOMBRE DE NOEUDS INTERFACE BLOQUEE
! NOL      /I/: VECTEUR DES NUMEROS DES NOEUDS LIBRES
! NOB      /I/: VECTEUR DES NUMEROS DES NOEUDS BLOQUES
! MAILLAGE /I/: NOM DU UTILISATEUR DU MAILLAGE
!
!
!
!
!
#include "jeveux.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    character(len=24) :: valk(3)
    character(len=8) :: nomnoe, mailla, typl, typb
    integer :: nol(nbl), nob(nbb)
    logical :: double
    integer :: i, jf, lcou, lp, nbb, nbl
!-----------------------------------------------------------------------
!
!
    if (nbl .eq. 0 .or. nbb .eq. 0) goto 9999
!
    double = .false.
    jf = 1
    do 10 i = 1, nbl
        jf = jf - 1
        lcou = nol(i)
        lp = 0
20      continue
        if (lp .lt. lcou .and. jf .lt. nbb) then
            jf = jf + 1
            lp = nob(jf)
            if (lp .eq. lcou) then
                double = .true.
                call jenuno(jexnum(mailla//'.NOMNOE', lp), nomnoe)
                valk(1) = nomnoe
                valk(2) = typl
                valk(3) = typb
                call utmess('E', 'ALGORITH13_69', nk=3, valk=valk)
            endif
!
            goto 20
!
        else
            goto 10
!
        endif
!
10  end do
!
    goto 9999
!
9999  continue
end subroutine
