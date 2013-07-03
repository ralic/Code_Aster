subroutine te0001(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/u2mesk.h"
    character(len=16) :: option, nomte
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
!     CALCUL DES TERMES DE FORC_NOD_6DDL, 3DDL, 2DDL
!     -----------------------------------------------------------------
!     EN ENTREE :
!        OPTION : NOM DE L'OPTION A CALCULER
!        NOMTE  : NOM DU TYPE_ELEMENT
!     -----------------------------------------------------------------
!     -----------------------------------------------------------------
    real(kind=8) :: dgrd
    real(kind=8) :: valpar(4), angl(3), mat(3, 3), vect(6)
    character(len=8) :: nompar(4), nomfon
    logical :: langl
!     -----------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ier, j, jdimp, jgeom, jtime, jvec
    integer :: nbpar, nddl, nddl1
!-----------------------------------------------------------------------
    if (nomte .eq. 'FORCE_NOD_6DDL') nddl1 = 6
    if (nomte .eq. 'FORCE_NOD_3DDL') nddl1 = 3
    if (nomte .eq. 'FORCE_NOD_2DDL') nddl1 = 2
    if (nomte .eq. 'FORCE_NOD_COQ2D') nddl1 = 3
    nddl = nddl1
    if (nomte .eq. 'FORCE_NOD_COQ2D') nddl = 2
!
    if (option .eq. 'CHAR_MECA_FORC_R') then
        call jevech('PGEOMER', 'L', jgeom)
        call jevech('PFORNOR', 'L', jdimp)
        call jevech('PVECTUR', 'E', jvec)
!CDIR$ IVDEP
        do 10 i = 1, nddl1
            zr(jvec-1+i) = zr(jdimp-1+i)
10      continue
        langl = zr(jdimp+nddl1) .lt. 0.d0
        do 11 i = 1, 3
            angl(i) = zr(jdimp+nddl1+i)
11      continue
    else if (option.eq. 'CHAR_MECA_FORC_F') then
        nbpar = 4
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        nompar(4) = 'INST'
        call jevech('PGEOMER', 'L', jgeom)
        call jevech('PTEMPSR', 'L', jtime)
        call jevech('PVECTUR', 'E', jvec)
        valpar(1) = zr(jgeom-1+1)
        valpar(2) = zr(jgeom-1+2)
        valpar(3) = zr(jgeom-1+3)
        valpar(4) = zr(jtime-1+1)
        call jevech('PFORNOF', 'L', jdimp)
        do 20 i = 1, nddl1
            nomfon = zk8(jdimp-1+i)
            ier=0
            call fointe('FM', nomfon, nbpar, nompar, valpar,&
                        zr(jvec-1+i), ier)
20      continue
        langl = zk8(jdimp+nddl1) .eq. 'UTILISAT'
        if (langl) then
            dgrd = r8dgrd()
            do 21 i = 1, 3
                nomfon = zk8(jdimp+nddl1+i)
                ier=0
                call fointe('FM', nomfon, nbpar, nompar, valpar,&
                            angl(i), ier)
                angl(i) = angl(i) * dgrd
21          continue
        endif
    else
        call u2mesk('F', 'ELEMENTS2_61', 1, option)
    endif
!
!     --- PROJECTION DANS LE REPERE ABSOLU ---
    if (langl) then
        call matrot(angl, mat)
        do 101 i = 1, min(nddl, 3)
            vect(i) = 0.d0
            do 101 j = 1, min(nddl, 3)
                vect(i) = vect(i) + mat(j,i)*zr(jvec-1+j)
101          continue
        do 102 i = 4, min(nddl, 6)
            vect(i) = 0.d0
            do 102 j = 4, min(nddl, 6)
                vect(i) = vect(i) + mat(j-3,i-3)*zr(jvec-1+j)
102          continue
        do 103 i = 1, nddl
            zr(jvec-1+i) = vect(i)
103      continue
    endif
end subroutine
