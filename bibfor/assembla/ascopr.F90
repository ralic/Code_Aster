subroutine ascopr(lmasym, lmesym, tt, jtmp2, nrmax,&
                  jresl, rcoef, jvalm)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
    character(len=2) :: tt
    integer :: jtmp2, nrmax, jresl
    integer :: jvalm(2)
    logical :: lmasym, lmesym
    real(kind=8) :: rcoef
!-----------------------------------------------------------------------
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
!     ROUTINE QUI ACCUMULE LES TERMES ELEMENTAIRES DANS LES BLOCS DE LA
!     MATRICE ASSEMBLEE POUR UNE MATRICE REELLE
!-----------------------------------------------------------------------
! IN  K2  TT    : /'RR' : COPIE R->R
!                 /'CC' : COPIE C->C
!                 /'RC' : COPIE R->C
! IN  I   JTMP2 : ADRESSE JEVEUX DE L'OBJET ".TMP2"
! IN  I   NRMAX  : NOMBRE DE REELS A CHARGER.
! IN  I   JRESL : ADRESSE JEVEUX DE L'OBJET ".RESL(IEL)".
! IN  L   LMASYM   : .TRUE. => LA MATR_ASSE EST SYMETRIQUE (2 BLOCS)
! IN  L   LMESYM   : .TRUE. => LA MATRICE ELEMANTAIRE EST SYMETRIQUE
! IN  R   RCOEF   : COEFFICIENT REEL MULTIPLICATEUR.
! IN  I   JVALM  : LISTE DES ADRESSES DES BLOCS DE .VALM
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ibloc, iadloc, j, jvalb, kfois, nbfois, permbl(2)
!-----------------------------------------------------------------------
!
!     -- SI ON ASSEMBLE UNE MATRICE ELEMENTAIRE SYMETRIQUE DANS
!        UNE MATRICE ASSEMBLEE NON SYMETRIQUE, IL FAUT COPIER 2 FOIS
    nbfois=1
    if (lmasym) then
        ASSERT(lmesym)
    else
        if (lmesym) nbfois=2
    endif
!
!
    do 1, kfois=1,nbfois
!
    if (kfois .eq. 1) then
        permbl(1)=1
        permbl(2)=2
    else
        permbl(1)=2
        permbl(2)=1
    endif
!
!
    if (tt .eq. 'RR') then
!       ---------------------------
!CDIR$   IVDEP
        do 10,j=1,nrmax
        ibloc=zi(jtmp2-1+2*(j-1)+1)
        jvalb=jvalm(permbl(ibloc))
        iadloc=zi(jtmp2-1+2*(j-1)+2)
        zr(jvalb+iadloc-1)=zr(jvalb+iadloc-1)+rcoef*zr(jresl-&
                1+j)
10      continue
!
!
    else if (tt.eq.'CC') then
!       ---------------------------
!CDIR$   IVDEP
        do 11,j=1,nrmax
        ibloc=zi(jtmp2-1+2*(j-1)+1)
        jvalb=jvalm(permbl(ibloc))
        iadloc=zi(jtmp2-1+2*(j-1)+2)
        zc(jvalb+iadloc-1)=zc(jvalb+iadloc-1)+rcoef*zc(jresl-&
                1+j)
11      continue
!
!
    else if (tt.eq.'RC') then
!       ---------------------------
!CDIR$   IVDEP
        do 12,j=1,nrmax
        ibloc=zi(jtmp2-1+2*(j-1)+1)
        jvalb=jvalm(permbl(ibloc))
        iadloc=zi(jtmp2-1+2*(j-1)+2)
        zc(jvalb+iadloc-1)=zc(jvalb+iadloc-1)+ dcmplx(rcoef*&
                zr(jresl-1+j),0.d0)
12      continue
!
!
    else
        ASSERT(.false.)
    endif
!
    1 end do
!
end subroutine
