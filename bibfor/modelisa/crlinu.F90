subroutine crlinu(nonu, mlgnno, nbnoe, numnoe, nomnoe,&
                  nbmtrd, jdnw, nummai, kk)
    implicit  none
#include "jeveux.h"
!
#include "asterfort/jenonu.h"
#include "asterfort/jexnom.h"
    integer :: nbnoe, numnoe(*), nummai(*), nbmtrd, jdnw(*), kk
    character(len=*) :: mlgnno, nonu, nomnoe(*)
!     -----------------------------------------------------------------
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
!     -----------------------------------------------------------------
!     TRANSFORME UNE LISTE DE NOMS DE NOEUDS EN UNE LISTE DE
!     NUMEROS DE MAILLES TARDIVES POUR NOCART
!     -----------------------------------------------------------------
!
    integer :: j, k, inoe
    character(len=8) :: nnoe
!     -----------------------------------------------------------------
!
    kk = 0
    do 10 j = 1, nbnoe
        if (nonu(1:3) .eq. 'NUM') then
            inoe = numnoe(j)
        else
            nnoe = nomnoe(j)
            call jenonu(jexnom(mlgnno, nnoe), inoe)
        endif
        do 20 k = 1, nbmtrd
            if (jdnw(k*2-1) .eq. inoe) then
                kk = kk + 1
                nummai(kk) = -k
                goto 10
            endif
20      continue
10  end do
!
end subroutine
