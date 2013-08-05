subroutine cmtrf2(codcm1, codtrf, ncm1, lcm1, ntrf,&
                  ltrf, nbma, codint, lint, nint)
    implicit   none
#include "asterfort/assert.h"
    integer :: codcm1, codtrf, codint, ncm1, ntrf, nint, nbma
    integer :: lint(nbma), lcm1(ncm1), ltrf(ntrf)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
!  BUT :
!  -----
!  ETABLIR LA LISTE DES NUMEROS DE MAILLES (LINT) APPARTENANT
!  AUX 2 LISTES LCM1 ET LTRF
! ----------------------------------------------------------------------
!
    integer :: k
! ----------------------------------------------------------------------
    ASSERT(codcm1.eq.1 .or. codcm1.eq.3)
    ASSERT(codtrf.eq.1 .or. codtrf.eq.3)
    ASSERT(nbma.gt.0)
    codint = 3
!
    if (codcm1 .eq. 1) then
        if (codtrf .eq. 1) then
            codint = 1
            nint = nbma
!
        else
            do 10,k = 1,ntrf
            lint(k) = ltrf(k)
            nint = ntrf
10          continue
        endif
!
    else
        if (codtrf .eq. 1) then
            do 20,k = 1,ncm1
            lint(k) = lcm1(k)
            nint = ncm1
20          continue
!
        else
!            -- ON NE PEUT PLUS RECULER, IL FAUT CALCULER
!               L'INTERSECTION :
            do 30,k = 1,nbma
            lint(k) = 0
30          continue
            do 40,k = 1,ncm1
            lint(lcm1(k)) = 1
40          continue
            do 50,k = 1,ntrf
            lint(ltrf(k)) = lint(ltrf(k)) + 1
50          continue
!          -- LES MAILLES COMMUNES CONTIENNENT 2 (1+1) :
            nint = 0
            do 60,k = 1,nbma
            if (lint(k) .eq. 2) then
                nint = nint + 1
                lint(nint) = k
            endif
60          continue
        endif
    endif
!
!
    ASSERT(codint.eq.1 .or. codint.eq.3)
    ASSERT(nint.ge.0)
end subroutine
