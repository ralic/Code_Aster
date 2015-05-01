subroutine te0546(option, nomte)
    implicit none
! aslint: disable=W0104
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/tecach.h"
!
    character(len=16) :: nomte, option
!.......................................................................
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
!
!     BUT: CALCUL DES OPTIONS SIGM_ELGA ET EFGE_ELGA
!          POUR TOUS LES ELEMENTS
!.......................................................................
!
    integer :: itab1(8), itab2(8), iret, nbpg, nbcmp, nbsp
    integer :: kpg, ksp, kcmp, jin, jout, ico, n1
!.......................................................................
!
!
    call tecach('OOO', 'PSIEFR', 'L', iret, nval=8,&
                itab=itab1)
    ASSERT(iret.eq.0)
!
    if (option .eq. 'SIGM_ELGA') then
        call tecach('OOO', 'PSIGMR', 'E', iret, nval=8,&
                    itab=itab2)
    else if (option.eq.'EFGE_ELGA') then
        call tecach('OOO', 'PEFGER', 'E', iret, nval=8,&
                    itab=itab2)
    else
        ASSERT(.false.)
    endif
!
!     -- VERIFICATIONS DE COHERENCE :
!     --------------------------------
    nbpg=itab1(3)
    ASSERT(nbpg.ge.1)
    ASSERT(nbpg.eq.itab2(3))
!
    nbsp=itab1(7)
    ASSERT(nbsp.ge.1)
    ASSERT(nbsp.eq.itab2(7))
!
    n1=itab1(2)
    nbcmp=n1/nbpg
    ASSERT(nbcmp*nbpg.eq.n1)
    ASSERT(nbcmp*nbpg.eq.itab2(2))
!
    ASSERT(itab1(6).le.1)
    ASSERT(itab2(6).le.1)
!
!     -- RECOPIE DES VALEURS :
!     --------------------------
    jin=itab1(1)
    jout=itab2(1)
    ico=0
    do kpg = 1, nbpg
        do ksp = 1, nbsp
            do kcmp = 1, nbcmp
                ico=ico+1
                zr(jout-1+ico)=zr(jin-1+ico)
            end do
        end do
    end do
!
end subroutine
