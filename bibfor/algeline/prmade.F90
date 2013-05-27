subroutine prmade(deblis, liste, adjncy, xadjd, ndi)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: olivier.boiteau at edf.fr
    implicit none
    integer :: deblis, liste(*), adjncy(*), xadjd(*), ndi
    integer :: nd, nnzero
    nd = deblis
    nnzero = 0
 1  continue
    if (nd .ne. 0) then
!      DO WHILE(ND.NE.0)
        adjncy(xadjd(ndi) + nnzero ) = nd
        nnzero = nnzero +1
        nd = liste(nd)
        goto 1
    endif
    xadjd(ndi+1) = xadjd(ndi) +nnzero
    deblis=0
end subroutine
