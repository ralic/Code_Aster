subroutine utreno(mcf, mcs, iocc, ma, noeud)
    implicit none
#include "asterfort/getvem.h"
#include "asterfort/getvtx.h"
#include "asterfort/u2mesk.h"
#include "asterfort/utnono.h"
    integer :: iocc
    character(len=8) :: ma, noeud
    character(len=*) :: mcf, mcs
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!     BUT: RECUPERER UN NOEUD "ORIGINE" OU "EXTREMITE"
!
! IN  : MCF    : MOT CLE FACTEUR
! IN  : MCS    : MOT CLE SIMPLE, ORIGINE OU EXTREMITE
! IN  : IOCC   : NUMERO D'OCCURENCE
! IN  : MA     : NOM DU MAILLAGE
! OUT : NOEUD  : NOM DU NOEUD RECUPERE
!     ------------------------------------------------------------------
    integer :: n1, iret
    character(len=8) :: k8b
    character(len=16) :: mcnoeu, mcgrno
    character(len=24) :: valk(2), nogno
    integer :: iarg
!     ------------------------------------------------------------------
!
    noeud = '        '
    if (mcs(1:4) .eq. 'ORIG') then
        mcnoeu = 'NOEUD_ORIG'
        mcgrno = 'GROUP_NO_ORIG'
    else if (mcs(1:4) .eq. 'EXTR') then
        mcnoeu = 'NOEUD_EXTR'
        mcgrno = 'GROUP_NO_EXTR'
    endif
!
    call getvtx(mcf, mcnoeu, iocc=iocc, nbval=0, nbret=n1)
    if (n1 .ne. 0) then
        call getvem(ma, 'NOEUD', mcf, mcnoeu, iocc,&
                    iarg, 1, noeud, n1)
    endif
!
    call getvtx(mcf, mcgrno, iocc=iocc, nbval=0, nbret=n1)
    if (n1 .ne. 0) then
        call getvtx(mcf, mcgrno, iocc=iocc, scal=nogno, nbret=n1)
        call utnono(' ', ma, 'NOEUD', nogno, noeud,&
                    iret)
        if (iret .eq. 10) then
            call u2mesk('F', 'ELEMENTS_67', 1, nogno)
        else if (iret .eq. 1) then
            valk(1) = nogno
            valk(2) = noeud
            call u2mesk('A', 'SOUSTRUC_87', 2, valk)
        endif
    endif
!
end subroutine
