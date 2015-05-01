function nddl(ili, nunoel, nec, idprn1, idprn2)
! aslint: disable=
    implicit none
    integer :: nddl
#include "jeveux.h"
    integer :: ili, nunoel
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
!----------------------------------------------------------------------
! IN  ILI    I : NUMERO DU GROUPE DANS LE LIGREL
! IN  NUNOEL I : NUMERO DU NOEUD
! OUT NDDL   I : NOMBRE DE DDLS DE CE NOEUD
!----------------------------------------------------------------------
!     FONCTION D ACCES A PRNO
!----------------------------------------------------------------------
    integer ::  idprn1, idprn2, iec, j, k, nec, lshift
!
!-----------------------------------------------------------------------
#define zzprno(ili,nunoel,l)   zi( idprn1-1+zi(idprn2+ili-1)+ (nunoel-1)* (nec+2)+l-1)
!---- DEBUT
    nddl = 0
    do 100 iec = 1, nec
        do 10 j = 1, 30
            k = iand(zzprno(ili,nunoel,iec+2),lshift(1,j))
            if (k .gt. 0) then
                nddl = nddl + 1
!
            endif
10      continue
100  end do
end function
