subroutine xintar(elp, ndim, ia, tabco, tabls,&
                  intar)
! aslint: disable=W1306
    implicit none
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/reerel.h"
#include "asterfort/u2mess.h"
#include "asterfort/vecini.h"
    integer :: ia, ndim
    character(len=8) :: elp
    real(kind=8) :: intar(ndim), tabco(*), tabls(*)
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
!                      TROUVER LE PT D'INTERSECTION ENTRE L'ARETE
!                      ET LA FISSURE
!
!     ENTREE
!       ELP     : TYPE DE L'ELEMENT
!       IA      : NUMERO DE L'ARETE REPEREE SUR L'ELEMENT
!       TABCO   : COORDONNEES DES NOEUDS DE L'ELEMENT
!       TABLS   : VALEUR DES LSN DES NOEUDS DE L'ELEMENT
!
!     SORTIE
!       INTAR   : COORDONNÉES DES POINTS D'INTERSECTION
!     ----------------------------------------------------------------
!
    real(kind=8) :: c1, c2, c3, a, b, c, d, sol, sol1, sol2, min, max
    real(kind=8) :: xe(ndim), crival
    integer :: nno
    parameter       (crival=1.d-8)
!
!---------------------------------------------------------------------
!     DEBUT
!---------------------------------------------------------------------
    call jemarq()
!
    sol=0.d0
    sol1=0.d0
    sol2=0.d0
    call vecini(ndim, 0.d0, xe)
!
    if (elp .eq. 'TR6') then
        max=1.d0
        min=0.d0
        if (ia .eq. 1) then
! ARETE 1-2-4
            c1=tabls(2)
            c2=tabls(4)
            c3=tabls(1)
        else if (ia.eq.2) then
! ARETE 2-3-5
            c1=tabls(2)
            c2=tabls(5)
            c3=tabls(3)
        else if (ia.eq.3) then
! ARETE 3-1-6
            c1=tabls(3)
            c2=tabls(6)
            c3=tabls(1)
        endif
        a = 2*c1-4*c2+2*c3
        b = -c1+4*c2-3*c3
        c = c3
!
    else if (elp.eq.'SE3') then
        max=1.d0
        min=-1.d0
        c1=tabls(1)
        c2=tabls(2)
        c3=tabls(3)
        a = c1/2+c2/2-c3
        b = (c2-c1)/2
        c = c3
!
    else if (elp.eq.'QU8') then
        max=1.d0
        min=-1.d0
        if (ia .eq. 1) then
! ARETE 1-2-5
            c1=tabls(1)
            c2=tabls(2)
            c3=tabls(5)
        else if (ia.eq.2) then
! ARETE 2-3-6
            c1=tabls(2)
            c2=tabls(3)
            c3=tabls(6)
        else if (ia.eq.3) then
! ARETE 3-4-7
            c1=tabls(4)
            c2=tabls(3)
            c3=tabls(7)
        else if (ia.eq.4) then
! ARETE 4-1-8
            c1=tabls(1)
            c2=tabls(4)
            c3=tabls(8)
        endif
        a = c1/2+c2/2-c3
        b = -c1/2+c2/2
        c = c3
!
    else
        ASSERT(1.eq.2)
    endif
!
    d = b*b-4*a*c
!
    if (abs(a) .le. crival .and. abs(b) .gt. crival) then
        sol=-c/b
    else if (abs(a).gt.crival) then
        if (abs(d) .le. r8prem()) then
            sol=-b/(2*a)
        else if (d.gt.r8prem()) then
            sol1=(-b-sqrt(d))/(2*a)
            sol2=(-b+sqrt(d))/(2*a)
            if (sol1 .gt. min .and. sol1 .lt. max) then
                sol=sol1
            else
                if (sol2 .gt. min .and. sol2 .lt. max) then
                    sol=sol2
                else
                    ASSERT(1.eq.2)
                endif
            endif
        else if (d.lt.-r8prem()) then
!       LE POLYNOME N'A PAS DE SOLUTION
            call u2mess('F', 'XFEM_65')
        endif
    endif
!
    if (elp .eq. 'TR6') then
        nno=6
        if (ia .eq. 1) then
! ARETE 1-2-4
            xe(1)=sol
        else if (ia.eq.2) then
! ARETE 2-3-5
            xe(1)=sol
            xe(2)=1-xe(1)
        else if (ia.eq.3) then
! ARETE 3-1-6
            xe(2)=sol
        endif
!
    else if (elp.eq.'SE3') then
        nno=3
        xe(1)=sol
!
    else if (elp.eq.'QU8') then
        nno=8
        if (ia .eq. 1) then
! ARETE 1-2-5
            xe(1)=sol
            xe(2)=-1
        else if (ia.eq.2) then
! ARETE 2-3-6
            xe(1)=1
            xe(2)=sol
        else if (ia.eq.3) then
! ARETE 3-4-7
            xe(1)=sol
            xe(2)=1
        else if (ia.eq.4) then
! ARETE 4-1-8
            xe(1)=-1
            xe(2)=sol
        endif
    else
        ASSERT(1.eq.2)
    endif
!
    call reerel(elp, nno, ndim, tabco, xe,&
                intar)
!
!---------------------------------------------------------------------
!     FIN
!---------------------------------------------------------------------
    call jedema()
end subroutine
