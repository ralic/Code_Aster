subroutine utnuav(noma, k, iocc, lno)
    implicit none
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/reliem.h"
    integer :: k, iocc
    character(len=*) :: noma, lno
! ----------------------------------------------------------------------
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
!
!     COMMANDE:  PROJ_CHAMP / NUAG_DEG_XX
!
! BUT : CREER LA LISTE DES NUMEROS DE NOEUDS D'UNE OCCURENCE DE
!       VIS_A_VIS
! ----------------------------------------------------------------------
!
    character(len=16) :: limocl(5), tymocl(5)
    character(len=8) :: ma8
    integer :: n1
!
    call jemarq()
    ma8=noma
!
    if (k .eq. 1) then
        limocl(1)='GROUP_MA_1'
        limocl(2)='GROUP_NO_1'
        limocl(3)='MAILLE_1'
        limocl(4)='NOEUD_1'
        limocl(5)='TOUT_1'
    else
        limocl(1)='GROUP_MA_2'
        limocl(2)='GROUP_NO_2'
        limocl(3)='MAILLE_2'
        limocl(4)='NOEUD_2'
        limocl(5)='TOUT_2'
    endif
!
    tymocl(1)='GROUP_MA'
    tymocl(2)='GROUP_NO'
    tymocl(3)='MAILLE'
    tymocl(4)='NOEUD'
    tymocl(5)='TOUT'
!
    call reliem(' ', ma8, 'NU_NOEUD', 'VIS_A_VIS', iocc,&
                5, limocl, tymocl, lno, n1)
    ASSERT(n1.gt.0)
!
    call jedema()
end subroutine
