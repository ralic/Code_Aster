subroutine calsvd(nm, m, n, a, w,&
                  matu, u, matv, v, ierr)
! aslint: disable=W1304
    implicit none
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
!-----------------------------------------------------------------------
!
! DESCRIPTION :   CALCUL DE LA DECOMPOSITION AUX VALEURS SINGULIERES
! -----------                          T
!                             A = U S V
!
!                 D'UNE MATRICE REELLE RECTANGULAIRE (M,N)
!                 APPEL A LA ROUTINE LAPACK : DGESDD
!
! IN     : NM   : INTEGER , SCALAIRE
!                 PREMIERE DIMENSION DES TABLEAUX A, U ET V, DECLAREE
!                 DANS L'APPELANT, NM >= MAX(M,N)
! IN     : M    : INTEGER , SCALAIRE
!                 NOMBRE DE LIGNES DES MATRICES A ET U
!                 NOMBRE DE COLONNES DE U
! IN     : N    : INTEGER , SCALAIRE
!                 NOMBRE DE COLONNES DE A
!                  = ORDRE DE LA MATRICE V
! IN     : A    : REAL*8 , TABLEAU DE DIMENSION(NM,N)
!                 CONTIENT LA MATRICE RECTANGULAIRE A DONT ON VEUT
!                 CALCULER LA DECOMPOSITION AUX VALEURS SINGULIERES
!                 LE CONTENU EST INCHANGE EN SORTIE : LE TABLEAU EST
!                 RECOPIE DANS U EN DEBUT DE CALCUL
! OUT    : W    : REAL*8 , VECTEUR DE DIMENSION N
!                 CONTIENT LES MIN(N,M) VALEURS SINGULIERES DE A
!                 LES VALEURS SINGULIERES SONT ORDONNEES:
!                 (ORDRE DECROISSANT)
! IN     : MATU : LOGICAL , SCALAIRE
!                 MATU = .TRUE.  INDIQUE QUE LA MATRICE U EST DESIREE
!                 MATU = .FALSE. SINON
! OUT    : U    : REAL*8 , TABLEAU DE DIMENSION (NM,M)
!                 SI MATU = .TRUE. LE TABLEAU U CONTIENT LA MATRICE U
!                 (MATRICE (M,N) A COLONNES ORTHOGONALES)
!                 SI MATU = .FALSE. LE TABLEAU U N'EST PAS REMPLI
! IN     : MATV : LOGICAL , SCALAIRE
!                 MATV = .TRUE.  INDIQUE QUE LA MATRICE V EST DESIREE
!                 MATV = .FALSE. SINON
! OUT    : V    : REAL*8 , TABLEAU DE DIMENSION (NM,N)
!                 SI MATV = .TRUE. LE TABLEAU V CONTIENT LA MATRICE V
!                 (MATRICE CARREE D'ORDRE N ORTHOGONALE)
!                 SI MATV = .FALSE. V EST INUTILE
! OUT    : IERR : INTEGER , SCALAIRE , CODE RETOUR
!                 IERR  = 0 : OK
!                 IERR /= 0 : PB LORS DE LA DECOMPOSITION
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "jeveux.h"
#include "asterc/matfpe.h"
#include "asterfort/jedetr.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/dgesdd.h"
#include "blas/dgesvd.h"
    integer :: nm, m, n, ierr
    real(kind=8) :: a(nm, n), w(n), u(nm, m), v(nm, n)
    logical :: matu, matv
!
! VARIABLES LOCALES
! -----------------
    integer(kind=4) :: ierr1
    integer ::    nm1, nm2, ldvt, i, j, lwork
    character(len=1) :: code
    parameter (nm1=20)
    real(kind=8) :: vt(nm1*nm1)
!     JE DOUBLE LA TAILLE DE WORK POUR DE MEILLEURS PERFS :
    real(kind=8) :: work(2*(7*nm1**2 + 4*nm1))
    integer(kind=4) :: iwork(8*nm1)
    logical :: alloc, safe
    integer(kind=4), pointer :: viwork(:) => null()
    real(kind=8), pointer :: vvt(:) => null()
    real(kind=8), pointer :: vwork(:) => null()
!
!
!
!
    call matfpe(-1)
!
    safe=.true.
!     LE BOOLEEN "SAFE" PERMET DE BASCULER ENTRE LES 2 ROUTINES LAPACK
!     SAFE=.TRUE.  => DGESVD
!     SAFE=.FALSE. => DGESDD
!
!
! -- REMARQUES DE JP QUI A REMPLACE LE TEXTE DE CETTE ROUTINE PAR
!    DES APPELS A LA ROUTINE LAPACK DGESDD :
!     1) LA ROUTINE CALSVD UTLISE V ET DGESDD UTILISE SA TRANSPOSEE: VT
! ----------------------------------------------------------------------
!
    nm2=max(n,m)
    ldvt=nm2
    lwork=2*(7*nm2**2 + 4*nm2)
!
!
!     -- POUR NE PAS ALLOUER D'OBJETS JEVEUX POUR LES PETITS CAS :
!     ------------------------------------------------------------
    if (nm1 .ge. nm2) then
        alloc=.false.
    else
        alloc=.true.
        AS_ALLOCATE(vr=vvt, size=nm2*nm2)
        AS_ALLOCATE(vr=vwork, size=lwork)
        AS_ALLOCATE(vi4=viwork, size=8*nm2)
    endif
!
    if (matu .or. matv) then
        code='A'
    else
        code='N'
    endif
!
!     -- APPEL A LA ROUTINE LAPACK (DGESDD):
!     ------------------------------------------------------------
    if (.not.alloc) then
        if (safe) then
            call dgesvd(code, code, m, n, a,&
                        nm, w, u, nm, vt,&
                        ldvt, work, lwork, ierr1)
        else
            call dgesdd(code, m, n, a, nm,&
                        w, u, nm, vt, ldvt,&
                        work, lwork, iwork, ierr1)
        endif
        if (matv) then
            do 1, i=1,nm
            do 2, j=1,n
            v(i,j)=vt((i-1)*ldvt+j)
 2          continue
 1          continue
        endif
!
    else
        if (safe) then
            call dgesvd(code, code, m, n, a,&
                        nm, w, u, nm, vvt,&
                        ldvt, vwork, lwork, ierr1)
        else
            call dgesdd(code, m, n, a, nm,&
                        w, u, nm, vvt, ldvt,&
                        vwork, lwork, viwork, ierr1)
        endif
        if (matv) then
            do 3, i=1,nm
            do 4, j=1,n
            v(i,j)=vvt((i-1)*ldvt+j)
 4          continue
 3          continue
        endif
    endif
!
    ierr=ierr1
!
    if (alloc) then
        AS_DEALLOCATE(vr=vvt)
        AS_DEALLOCATE(vr=vwork)
        AS_DEALLOCATE(vi4=viwork)
    endif
!
!
    call matfpe(1)
!
end subroutine
