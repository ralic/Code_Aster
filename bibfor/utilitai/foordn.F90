subroutine foordn(vecpar, vecnom, ne, ns, ier)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesg.h"
    integer :: ne, ns, ier
    real(kind=8) :: vecpar(ne)
    character(len=*) :: vecnom(ne)
! ----------------------------------------------------------------------
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
!     REORDONNE UNE NAPPE PAR ORDRE CROISSANT DU PARAMETRE
! ----------------------------------------------------------------------
! IN  : VECPAR : VECTEUR DES VALEURS DU PARAMETRE
! IN  : VECNOM : VECTEUR DES NOMS DES FONCTIONS
! IN  : NE     : NOMBRE DE FONCTIONS ET PARAMETRES
! OUT : NS     : NE - EVENTUELLEMENT LE NOMBRE DE DOUBLONS
! OUT : IER    : = 0 , SI TOUT VA BIEN
!                = 1 , SINON
! ----------------------------------------------------------------------
    integer :: vali
!     ------------------------------------------------------------------
    real(kind=8) :: x
    character(len=24) :: c
!     ------------------------------------------------------------------
!
!     --- RANGEMENT EN ORDRE CROISSANT ---
!-----------------------------------------------------------------------
    integer :: i, ival, j, k, lval1, lval2, nbval1
    integer :: nbval2
!-----------------------------------------------------------------------
    call jemarq()
    ier = 0
    do 10 i = 1, ne-1
        do 10 j = i+1, ne
            if (vecpar(i) .gt. vecpar(j)) then
                x = vecpar(i)
                c = vecnom(i)
                vecpar(i) = vecpar(j)
                vecnom(i) = vecnom(j)
                vecpar(j) = x
                vecnom(j) = c
            endif
10      continue
!
!     --- SUPPRESSION DES DOUBLONS ---
    ns = ne
    do 20 i = 1, ne-1
        if (vecpar(i) .eq. vecpar(i+1)) then
            if (vecnom(i) .ne. vecnom(i+1)) then
                if (vecnom(i) (1:1) .eq. '&' .and. vecnom(i+1)(1:1) .eq. '&') then
                    call jelira(vecnom(i), 'LONUTI', nbval1)
                    call jelira(vecnom(i+1), 'LONUTI', nbval2)
                    if (nbval1 .eq. nbval2) then
                        call jeveuo(vecnom(i), 'L', lval1)
                        call jeveuo(vecnom(i+1), 'L', lval2)
                        do 22 ival = 0, nbval1-1
                            if (zr(lval1+ival) .ne. zr(lval2+ival)) ier = 1
22                      continue
                    else
                        ier = 1
                    endif
                else
                    ier = 1
                endif
            else
                do 24 j = i, ne-1
                    vecpar(j) = vecpar(j+1)
                    vecnom(j) = vecnom(j+1)
24              continue
                ns = ns - 1
            endif
        endif
20  end do
    if (ne .ne. ns) then
        k = ne - ns
        vali = k
        call u2mesg('F', 'UTILITAI6_38', 0, ' ', 1,&
                    vali, 0, 0.d0)
    endif
!
    call jedema()
end subroutine
