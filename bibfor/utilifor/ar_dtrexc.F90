! ===============================================================
! THIS LAPACK 2.0 ROUTINE IS DEPRECATED  
! DO NOT USE IT : YOU SHOULD PREFER UP-TO-DATE LAPACK ROUTINE
!
! BUT DO NOT REMOVE IT :
! THE PRESENT ROUTINE IS MANDATORY FOR ARPACK LIBRARY
! WHICH STICKS TO LAPACK 2.0 VERSION 
! ==============================================================
subroutine ar_dtrexc(compq, n, t, ldt, q,&
                  ldq, ifst, ilst, work, info)
! ======================================================================
! COPYRIGHT (C) LAPACK
! COPYRIGHT (C) 2007 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
!
!     SUBROUTINE LAPACK REORDONNANT LA FACTORISATION DE SCHUR REELLE
!     D'UNE MATRICE REELLE QUELCONQUE.
!-----------------------------------------------------------------------
!  -- LAPACK ROUTINE (VERSION 2.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     MARCH 31, 1993
!
!  PURPOSE
!  =======
!
!  DTREXC REORDERS THE REAL SCHUR FACTORIZATION OF A REAL MATRIX
!  A = Q*T*Q**T, SO THAT THE DIAGONAL BLOCK OF T WITH ROW INDEX IFST IS
!  MOVED TO ROW ILST.
!
!  THE REAL SCHUR FORM T IS REORDERED BY AN ORTHOGONAL SIMILARITY
!  TRANSFORMATION Z**T*T*Z, AND OPTIONALLY THE MATRIX Q OF SCHUR VECTORS
!  IS UPDATED BY POSTMULTIPLYING IT WITH Z.
!
!  T MUST BE IN SCHUR CANONICAL FORM (AS RETURNED BY DHSEQR), THAT IS,
!  BLOCK UPPER TRIANGULAR WITH 1-BY-1 AND 2-BY-2 DIAGONAL BLOCKS, EACH
!  2-BY-2 DIAGONAL BLOCK HAS ITS DIAGONAL ELEMENTS EQUAL AND ITS
!  OFF-DIAGONAL ELEMENTS OF OPPOSITE SIGN.
!
!  ARGUMENTS
!  =========
!
!  COMPQ   (INPUT) CHARACTER*1
!          = 'V':  UPDATE THE MATRIX Q OF SCHUR VECTORS,
!          = 'N':  DO NOT UPDATE Q.
!
!  N       (INPUT) INTEGER
!          THE ORDER OF THE MATRIX T. N >= 0.
!
!  T       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDT,N)
!          ON ENTRY, THE UPPER QUASI-TRIANGULAR MATRIX T, IN SCHUR
!          SCHUR CANONICAL FORM.
!          ON EXIT, THE REORDERED UPPER QUASI-TRIANGULAR MATRIX, AGAIN
!          IN SCHUR CANONICAL FORM.
!
!  LDT     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY T. LDT >= MAX(1,N).
!
!  Q       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDQ,N)
!          ON ENTRY, IF COMPQ = 'V', THE MATRIX Q OF SCHUR VECTORS.
!          ON EXIT, IF COMPQ = 'V', Q HAS BEEN POSTMULTIPLIED BY THE
!          ORTHOGONAL TRANSFORMATION MATRIX Z WHICH REORDERS T.
!          IF COMPQ = 'N', Q IS NOT REFERENCED.
!
!  LDQ     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY Q.  LDQ >= MAX(1,N).
!
!  IFST    (INPUT/OUTPUT) INTEGER
!  ILST    (INPUT/OUTPUT) INTEGER
!          SPECIFY THE REORDERING OF THE DIAGONAL BLOCKS OF T.
!          THE BLOCK WITH ROW INDEX IFST IS MOVED TO ROW ILST, BY A
!          SEQUENCE OF TRANSPOSITIONS BETWEEN ADJACENT BLOCKS.
!          ON EXIT, IF IFST POINTED ON ENTRY TO THE SECOND ROW OF A
!          2-BY-2 BLOCK, IT IS CHANGED TO POINT TO THE FIRST ROW, ILST
!          ALWAYS POINTS TO THE FIRST ROW OF THE BLOCK IN ITS FINAL
!          POSITION (WHICH MAY DIFFER FROM ITS INPUT VALUE BY +1 OR -1).
!          1 <= IFST <= N, 1 <= ILST <= N.
!
!  WORK    (WORKSPACE) REAL*8 ARRAY, DIMENSION (N)
!
!  INFO    (OUTPUT) INTEGER
!          = 0:  SUCCESSFUL EXIT
!          < 0:  IF INFO = -I, THE I-TH ARGUMENT HAD AN ILLEGAL VALUE
!          = 1:  TWO ADJACENT BLOCKS WERE TOO CLOSE TO SWAP (THE PROBLEM
!                IS VERY ILL-CONDITIONED), T MAY HAVE BEEN PARTIALLY
!                REORDERED, AND ILST POINTS TO THE FIRST ROW OF THE
!                CURRENT POSITION OF THE BLOCK BEING MOVED.
!
! ASTER INFORMATION
! 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            REMPLACEMENT DE 9 RETURN PAR GOTO 1000,
!            IMPLICIT NONE.
! INTRINSIC FUNCTION
!   MAX
! ENDLIB
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     .. SCALAR ARGUMENTS ..
#include "asterf_types.h"
#include "asterfort/ar_dlaexc.h"
#include "asterfort/xerbla.h"
#include "blas/lsame.h"
    character(len=1) :: compq
    integer :: ifst, ilst, info, ldq, ldt, n
!     ..
!     .. ARRAY ARGUMENTS ..
    real(kind=8) :: q( ldq, * ), t( ldt, * ), work( * )
!
!     .. PARAMETERS ..
    real(kind=8) :: zero
    parameter          ( zero = 0.0d+0 )
!     ..
!     .. LOCAL SCALARS ..
    aster_logical :: wantq
    integer :: here, nbf, nbl, nbnext
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     DECODE AND TEST THE INPUT ARGUMENTS.
!
    info = 0
    wantq = lsame( compq, 'V' )
    if (.not.wantq .and. .not.lsame( compq, 'N' )) then
        info = -1
    else if (n.lt.0) then
        info = -2
    else if (ldt.lt.max( 1, n )) then
        info = -4
    else if (ldq.lt.1 .or. ( wantq .and. ldq.lt.max( 1, n ) )) then
        info = -6
    else if (ifst.lt.1 .or. ifst.gt.n) then
        info = -7
    else if (ilst.lt.1 .or. ilst.gt.n) then
        info = -8
    endif
    if (info .ne. 0) then
        call xerbla('DTREXC', -info)
        goto 1000
    endif
!
!     QUICK RETURN IF POSSIBLE
!
    if (n .le. 1) goto 1000
!
!     DETERMINE THE FIRST ROW OF SPECIFIED BLOCK
!     AND FIND OUT IT IS 1 BY 1 OR 2 BY 2.
!
    if (ifst .gt. 1) then
        if (t( ifst, ifst-1 ) .ne. zero) ifst = ifst - 1
    endif
    nbf = 1
    if (ifst .lt. n) then
        if (t( ifst+1, ifst ) .ne. zero) nbf = 2
    endif
!
!     DETERMINE THE FIRST ROW OF THE FINAL BLOCK
!     AND FIND OUT IT IS 1 BY 1 OR 2 BY 2.
!
    if (ilst .gt. 1) then
        if (t( ilst, ilst-1 ) .ne. zero) ilst = ilst - 1
    endif
    nbl = 1
    if (ilst .lt. n) then
        if (t( ilst+1, ilst ) .ne. zero) nbl = 2
    endif
!
    if (ifst .eq. ilst) goto 1000
!
    if (ifst .lt. ilst) then
!
!        UPDATE ILST
!
        if (nbf .eq. 2 .and. nbl .eq. 1) ilst = ilst - 1
        if (nbf .eq. 1 .and. nbl .eq. 2) ilst = ilst + 1
!
        here = ifst
!
 10     continue
!
!        SWAP BLOCK WITH NEXT ONE BELOW
!
        if (nbf .eq. 1 .or. nbf .eq. 2) then
!
!           CURRENT BLOCK EITHER 1 BY 1 OR 2 BY 2
!
            nbnext = 1
            if (here+nbf+1 .le. n) then
                if (t( here+nbf+1, here+nbf ) .ne. zero) nbnext = 2
            endif
            call ar_dlaexc(wantq, n, t, ldt, q,&
                        ldq, here, nbf, nbnext, work,&
                        info)
            if (info .ne. 0) then
                ilst = here
                goto 1000
            endif
            here = here + nbnext
!
!           TEST IF 2 BY 2 BLOCK BREAKS INTO TWO 1 BY 1 BLOCKS
!
            if (nbf .eq. 2) then
                if (t( here+1, here ) .eq. zero) nbf = 3
            endif
!
        else
!
!           CURRENT BLOCK CONSISTS OF TWO 1 BY 1 BLOCKS EACH OF WHICH
!           MUST BE SWAPPED INDIVIDUALLY
!
            nbnext = 1
            if (here+3 .le. n) then
                if (t( here+3, here+2 ) .ne. zero) nbnext = 2
            endif
            call ar_dlaexc(wantq, n, t, ldt, q,&
                        ldq, here+1, 1, nbnext, work,&
                        info)
            if (info .ne. 0) then
                ilst = here
                goto 1000
            endif
            if (nbnext .eq. 1) then
!
!              SWAP TWO 1 BY 1 BLOCKS, NO PROBLEMS POSSIBLE
!
                call ar_dlaexc(wantq, n, t, ldt, q,&
                            ldq, here, 1, nbnext, work,&
                            info)
                here = here + 1
            else
!
!              RECOMPUTE NBNEXT IN CASE 2 BY 2 SPLIT
!
                if (t( here+2, here+1 ) .eq. zero) nbnext = 1
                if (nbnext .eq. 2) then
!
!                 2 BY 2 BLOCK DID NOT SPLIT
!
                    call ar_dlaexc(wantq, n, t, ldt, q,&
                                ldq, here, 1, nbnext, work,&
                                info)
                    if (info .ne. 0) then
                        ilst = here
                        goto 1000
                    endif
                    here = here + 2
                else
!
!                 2 BY 2 BLOCK DID SPLIT
!
                    call ar_dlaexc(wantq, n, t, ldt, q,&
                                ldq, here, 1, 1, work,&
                                info)
                    call ar_dlaexc(wantq, n, t, ldt, q,&
                                ldq, here+1, 1, 1, work,&
                                info)
                    here = here + 2
                endif
            endif
        endif
        if (here .lt. ilst) goto 10
!
    else
!
        here = ifst
 20     continue
!
!        SWAP BLOCK WITH NEXT ONE ABOVE
!
        if (nbf .eq. 1 .or. nbf .eq. 2) then
!
!           CURRENT BLOCK EITHER 1 BY 1 OR 2 BY 2
!
            nbnext = 1
            if (here .ge. 3) then
                if (t( here-1, here-2 ) .ne. zero) nbnext = 2
            endif
            call ar_dlaexc(wantq, n, t, ldt, q,&
                        ldq, here-nbnext, nbnext, nbf, work,&
                        info)
            if (info .ne. 0) then
                ilst = here
                goto 1000
            endif
            here = here - nbnext
!
!           TEST IF 2 BY 2 BLOCK BREAKS INTO TWO 1 BY 1 BLOCKS
!
            if (nbf .eq. 2) then
                if (t( here+1, here ) .eq. zero) nbf = 3
            endif
!
        else
!
!           CURRENT BLOCK CONSISTS OF TWO 1 BY 1 BLOCKS EACH OF WHICH
!           MUST BE SWAPPED INDIVIDUALLY
!
            nbnext = 1
            if (here .ge. 3) then
                if (t( here-1, here-2 ) .ne. zero) nbnext = 2
            endif
            call ar_dlaexc(wantq, n, t, ldt, q,&
                        ldq, here-nbnext, nbnext, 1, work,&
                        info)
            if (info .ne. 0) then
                ilst = here
                goto 1000
            endif
            if (nbnext .eq. 1) then
!
!              SWAP TWO 1 BY 1 BLOCKS, NO PROBLEMS POSSIBLE
!
                call ar_dlaexc(wantq, n, t, ldt, q,&
                            ldq, here, nbnext, 1, work,&
                            info)
                here = here - 1
            else
!
!              RECOMPUTE NBNEXT IN CASE 2 BY 2 SPLIT
!
                if (t( here, here-1 ) .eq. zero) nbnext = 1
                if (nbnext .eq. 2) then
!
!                 2 BY 2 BLOCK DID NOT SPLIT
!
                    call ar_dlaexc(wantq, n, t, ldt, q,&
                                ldq, here-1, 2, 1, work,&
                                info)
                    if (info .ne. 0) then
                        ilst = here
                        goto 1000
                    endif
                    here = here - 2
                else
!
!                 2 BY 2 BLOCK DID SPLIT
!
                    call ar_dlaexc(wantq, n, t, ldt, q,&
                                ldq, here, 1, 1, work,&
                                info)
                    call ar_dlaexc(wantq, n, t, ldt, q,&
                                ldq, here-1, 1, 1, work,&
                                info)
                    here = here - 2
                endif
            endif
        endif
        if (here .gt. ilst) goto 20
    endif
    ilst = here
!
1000 continue
!
!     END OF DTREXC
!
end subroutine
