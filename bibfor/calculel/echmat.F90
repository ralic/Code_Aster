subroutine echmat(matz, ldist, rmin, rmax)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "jeveux.h"
!
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mpicm1.h"
    character(len=*) :: matz
    real(kind=8) :: rmin, rmax
    logical :: ldist
! ---------------------------------------------------------------------
! BUT: DONNER LES VALEURS EXTREMES DES VALEURS ABSOLUES
!      DES TERMES NON NULS DE LA DIAGONALE D'UNE MATR_ASSE
! ---------------------------------------------------------------------
!
!     ARGUMENTS:
!
! IN   MATZ  (K19)     : MATR_ASSE A ANALYSER
! IN   LDIST (LOGICAL) : INDIQUE SI LE CALCUL EST DISTRIBUE AU SENS
!                        DONNEE INCOMPLETE PAR PROC
!
! OUT  RMIN  (R8)      : PLUS PETIT TERME NON NUL (EN VALEUR ABSOLUE)
!                        SUR LA DIAGONALE DE MATZ
! OUT  RMAX  (R8)      : PLUS GRAND TERME (EN VALEUR ABSOLUE)
!                        SUR LA DIAGONALE DE MATZ
! ATTENTION : SI LA MATRICE EST IDENTIQUEMENT NULLE, LA ROUTINE
!             RETOURNE :
!               RMAX=0.D0
!               RMIN=R8MAEM ~1.8E308   (RMIN > RMAX !)
! ---------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: jsmdi, nsmhc, jdelgg, jdelgl, jsmhc, ng, ibid, nz, n, imatd
    integer :: jcol, nlong, jrefa, jvalm1
    character(len=1) :: kbid, ktyp, base1
    character(len=14) :: nonu
    character(len=19) :: mat19
    real(kind=8) :: rdiag
    complex(kind=8) :: cbid
!=================================================================
    call jemarq()
!
    mat19=matz
    call jeveuo(mat19//'.REFA', 'L', jrefa)
    nonu=zk24(jrefa-1+2)
    call jelira(nonu//'.SMOS.SMDI', 'LONMAX', n, kbid)
    call jeveuo(nonu//'.SMOS.SMDI', 'L', jsmdi)
    nz=zi(jsmdi-1+n)
    call jeveuo(nonu//'.SMOS.SMHC', 'L', jsmhc)
    call jelira(nonu//'.SMOS.SMHC', 'LONMAX', nsmhc, kbid)
    call assert(nz.le.nsmhc)
!
    call jeveuo(nonu//'.NUME.DELG', 'L', jdelgg)
    call jelira(nonu//'.NUME.DELG', 'LONMAX', ng, kbid)
    call jeexin(nonu//'.NUML.DELG', imatd)
    if (imatd .ne. 0) then
        call jeveuo(nonu//'.NUML.DELG', 'L', jdelgl)
    else
        jdelgl=jdelgg
        call assert(ng.eq.n)
    endif
!
    call jelira(mat19//'.VALM', 'TYPE', ibid, ktyp)
    call jelira(mat19//'.VALM', 'CLAS', ibid, base1)
    call jeveuo(jexnum(mat19//'.VALM', 1), 'L', jvalm1)
    call jelira(jexnum(mat19//'.VALM', 1), 'LONMAX', nlong, kbid)
    call assert(nlong.eq.nz)
!
!
!     --CALCUL DE RMIN ET RMAX :
!     -----------------------------
    rmin=r8maem()
    rmax=-1.d0
!     CALCUL DE RMIN : PLUS PETIT TERME NON NUL DE LA DIAGONALE
!     CALCUL DE RMAX : PLUS GRAND TERME DE LA DIAGONALE
    do 10,jcol=1,n
    if (zi(jdelgl-1+jcol) .lt. 0) then
        goto 10
    endif
!
    if (ktyp .eq. 'R') then
        rdiag=abs(zr(jvalm1-1+zi(jsmdi+jcol-1)))
    else
        rdiag=abs(zc(jvalm1-1+zi(jsmdi+jcol-1)))
    endif
    if (rdiag .gt. rmax) rmax=rdiag
    if (rdiag .eq. 0.d0) goto 10
    if (rdiag .lt. rmin) rmin=rdiag
    10 end do
!
!     -- SI EXECUTION PARALLELE, IL FAUT COMMUNIQUER :
    if (ldist) call mpicm1('MPI_MAX', 'R', 1, ibid, ibid,&
                           rmax, cbid)
    if (ldist) call mpicm1('MPI_MIN', 'R', 1, ibid, ibid,&
                           rmin, cbid)
!
    call jedema()
end subroutine
