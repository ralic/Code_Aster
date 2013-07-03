subroutine sstriv(rdiak, rdiam, lprod, ipos, neq)
    implicit none
#include "jeveux.h"
    real(kind=8) :: rdiak(neq), rdiam(neq)
    integer :: lprod(neq), ipos(neq), neq
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
!        TRI DES PLUS PETITS K/M
!
!     ------------------------------------------------------------------
!     IN RDIAK : TABLEAU CONTENANT LA DIAGONALE DE LA RAIDEUR
!     IN RDIAM : TABLEAU CONTENANT LA DIAGONALE DE LA MASSE
!     IN LPROD : ADRESSE DANS ZI DES DDL SUR LESQUELS ON TRIE
!     IN NEQ   : NOMBRE D EQUATIONS
!     OUT IPOS : TABLEAU DES INDIRECTIONS APRES LE TRI
!
!
!-----------------------------------------------------------------------
    integer :: i, icont, indic, itemp, ll
!-----------------------------------------------------------------------
    icont = 0
    do 1 i = 1, neq
        ipos(i) = i
        icont = icont + lprod(i)
 1  end do
10  continue
    indic = 0
    do 2 ll = 1, neq-1
        if (lprod(ipos(ll)) .eq. 0 .and. lprod(ipos(ll+1)) .eq. 1) then
            itemp = ipos(ll)
            ipos(ll) = ipos(ll+1)
            ipos(ll+1) = itemp
            indic =1
        endif
 2  end do
    if (indic .eq. 1) goto 10
!
31  continue
    indic = 0
    do 32 ll = 1, icont-1
        if (rdiam(ipos(ll)) .gt. 0.0d0 .and. rdiam(ipos(ll+1)) .gt. 0.0d0) then
            if (rdiak(ipos(ll)) .gt. 0.0d0 .and. rdiak(ipos(ll+1)) .gt. 0.0d0) then
                if (rdiak(ipos(ll))/rdiam(ipos(ll)) .gt.&
                    rdiak(ipos(ll+ 1))/rdiam(ipos(ll+1))) then
                    itemp = ipos(ll)
                    ipos(ll) = ipos(ll+1)
                    ipos(ll+1) = itemp
                    indic =1
                endif
            else if (rdiak(ipos(ll+1)).gt.0.0d0) then
                itemp = ipos(ll)
                ipos(ll) = ipos(ll+1)
                ipos(ll+1) = itemp
                indic =1
            endif
            else if(rdiak(ipos(ll+1)).gt.0.0d0 .and. rdiam(ipos(ll+1))&
        .gt.0.0d0) then
            itemp = ipos(ll)
            ipos(ll) = ipos(ll+1)
            ipos(ll+1) = itemp
            indic =1
        else if (rdiam(ipos(ll)).eq.0.and.rdiam(ipos(ll+1)).ne.0) then
            itemp = ipos(ll)
            ipos(ll) = ipos(ll+1)
            ipos(ll+1) = itemp
            indic =1
        endif
32  end do
    if (indic .eq. 1) goto 31
end subroutine
