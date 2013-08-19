function jxhcod(chain, lrep)
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
! aslint: disable=W1304,C1002
    implicit none
#include "asterc/strmov.h"
    integer :: jxhcod, lrep
    character(len=*) :: chain
    integer :: i1, j(4)
    integer(kind=4) :: i(8)
    equivalence (i(1), j(1))
!
!   ATTENTION, IL FAUT IMPERATIVEMENT UTILISER UN TABLEAU I AYANT
!   POUR LONGUEUR TOTALE 32 OCTETS (ICI 4*8) POUR S'ALIGNER SUR LA
!   CHAINE PASSEE EN ARGUMENT (32 CARACTERES MAXIMUM)
!
!-----------------------------------------------------------------------
    integer :: k
!-----------------------------------------------------------------------
    call strmov(chain, 1, 32, j, 1)
    do 1 k = 2, 8
        i(1) = ieor( i(1) , i(k) )
 1  end do
    i1=i(1)
    jxhcod = 1 + mod(i1,lrep)
end function
