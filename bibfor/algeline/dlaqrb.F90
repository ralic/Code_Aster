subroutine dlaqrb(wantt, n, ilo, ihi, h,&
                  ldh, wr, wi, z, info)
!----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) LAPACK
! COPYRIGHT (C) 2007 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
!     SUBROUTINE ARPACK CALCULANT LES VALEURS PROPRES ET LA DECOMPOSI
!     TION DE SCHUR D'UNE MATRICE DE HESSENBERG.
!-----------------------------------------------------------------------
! BEGINDOC
!
! DESCRIPTION:
!  COMPUTE THE EIGENVALUES AND THE SCHUR DECOMPOSITION OF AN UPPER
!  HESSENBERG SUBMATRIX IN ROWS AND COLUMNS ILO TO IHI.  ONLY THE
!  LAST COMPONENT OF THE SCHUR VECTORS ARE COMPUTED.
!
!  THIS IS MOSTLY A MODIFICATION OF THE LAPACK ROUTINE DLAHQR.
!
! ARGUMENTS
!  WANTT   LOGICAL VARIABLE.  (INPUT)
!          = .TRUE. : THE FULL SCHUR FORM T IS REQUIRED,
!          = .FALSE.: ONLY EIGENVALUES ARE REQUIRED.
!
!  N       INTEGER.  (INPUT)
!          THE ORDER OF THE MATRIX H.  N >= 0.
!
!  ILO     INTEGER.  (INPUT)
!  IHI     INTEGER.  (INPUT)
!          IT IS ASSUMED THAT H IS ALREADY UPPER QUASI-TRIANGULAR IN
!          ROWS AND COLUMNS IHI+1:N, AND THAT H(ILO,ILO-1) = 0 (UNLESS
!          ILO = 1). SLAQRB WORKS PRIMARILY WITH THE HESSENBERG
!          SUBMATRIX IN ROWS AND COLUMNS ILO TO IHI, BUT APPLIES
!          TRANSFORMATIONS TO ALL OF H IF WANTT IS .TRUE..
!          1 <= ILO <= MAX(1,IHI), IHI <= N.
!
!  H       REAL*8 ARRAY, DIMENSION (LDH,N).  (INPUT/OUTPUT)
!          ON ENTRY, THE UPPER HESSENBERG MATRIX H.
!          ON EXIT, IF WANTT IS .TRUE., H IS UPPER QUASI-TRIANGULAR IN
!          ROWS AND COLUMNS ILO:IHI, WITH ANY 2-BY-2 DIAGONAL BLOCKS IN
!          STANDARD FORM. IF WANTT IS .FALSE., THE CONTENTS OF H ARE
!          UNSPECIFIED ON EXIT.
!
!  LDH     INTEGER.  (INPUT)
!          THE LEADING DIMENSION OF THE ARRAY H. LDH >= MAX(1,N).
!
!  WR      REAL*8 ARRAY, DIMENSION (N).  (OUTPUT)
!  WI      REAL*8 ARRAY, DIMENSION (N).  (OUTPUT)
!          THE REAL AND IMAGINARY PARTS, RESPECTIVELY, OF THE COMPUTED
!          EIGENVALUES ILO TO IHI ARE STORED IN THE CORRESPONDING
!          ELEMENTS OF WR AND WI. IF TWO EIGENVALUES ARE COMPUTED AS A
!          COMPLEX CONJUGATE PAIR, THEY ARE STORED IN CONSECUTIVE
!          ELEMENTS OF WR AND WI, SAY THE I-TH AND (I+1)TH, WITH
!          WI(I) > 0 AND WI(I+1) < 0. IF WANTT IS .TRUE., THE
!          EIGENVALUES ARE STORED IN THE SAME ORDER AS ON THE DIAGONAL
!          OF THE SCHUR FORM RETURNED IN H, WITH WR(I) = H(I,I), AND, IF
!          H(I:I+1,I:I+1) IS A 2-BY-2 DIAGONAL BLOCK,
!          WI(I) = SQRT(H(I+1,I)*H(I,I+1)) AND WI(I+1) = -WI(I).
!
!  Z       REAL*8 ARRAY, DIMENSION (N).  (OUTPUT)
!          ON EXIT Z CONTAINS THE LAST COMPONENTS OF THE SCHUR VECTORS.
!
!  INFO    INTEGER.  (OUPUT)
!          = 0: SUCCESSFUL EXIT
!          > 0: SLAQRB FAILED TO COMPUTE ALL THE EIGENVALUES ILO TO IHI
!               IN A TOTAL OF 30*(IHI-ILO+1) ITERATIONS, IF INFO = I,
!               ELEMENTS I+1:IHI OF WR AND WI CONTAIN THOSE EIGENVALUES
!               WHICH HAVE BEEN SUCCESSFULLY COMPUTED.
!
!-----------------------------------------------------------------------
! BEGINLIB
!
! ROUTINES CALLED:
!     DLANHS  LAPACK ROUTINE THAT COMPUTES VARIOUS NORMS OF A MATRIX.
!     DLANV2  LAPACK ROUTINE THAT COMPUTES THE SCHUR FACTORIZATION OF
!             2 BY 2 NONSYMMETRIC MATRIX IN STANDARD FORM.
!     DLARFG  LAPACK HOUSEHOLDER REFLECTION CONSTRUCTION ROUTINE.
!     DCOPY   LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER.
!     DROT    LEVEL 1 BLAS THAT APPLIES A ROTATION TO A 2 BY 2 MATRIX.
!
!     R8PREM  ASTER UTILITY ROUTINE THAT GIVES THE MACHINE PRECISION.
!     R8MIEM  ASTER UTILITY ROUTINE THAT GIVES THE MINIMUN VALUES.
!     ISBAEM  ASTER UTILITY ROUTINE THAT GIVES THE MACHINE BASE.
!
! INTRINSIC FUNCTIONS
!     ABS, MIN, MAX.
!
! AUTHOR
!     DANNY SORENSEN               PHUONG VU
!     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
!     DEPT. OF COMPUTATIONAL &     HOUSTON, TEXAS
!     APPLIED MATHEMATICS
!     RICE UNIVERSITY
!     HOUSTON, TEXAS
!
! REVISION HISTORY:
!     XX/XX/92: VERSION ' 2.4'
!               MODIFIED FROM THE LAPACK ROUTINE DLAHQR SO THAT ONLY THE
!               LAST COMPONENT OF THE SCHUR VECTORS ARE COMPUTED.
!
! FILE: LAQRB.F   SID: 2.2   DATE OF SID: 8/27/96   RELEASE: 2
!
! ASTER INFORMATION
! 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            DISPARITION DE DLAMCH ET DLABAD,
!            UTILISATION DE R8PREM(), R8MIEM() ET ISBAEM(),
!            REMPLACEMENT DE RETURN PAR GOTO 1000,
!            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
!            IMPLICIT NONE.
! ENDLIB
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     %------------------%
!     | SCALAR ARGUMENTS |
!     %------------------%
!
#include "asterf_types.h"
#include "asterc/isbaem.h"
#include "asterc/matfpe.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "asterfort/ar_dlanv2.h"
#include "asterfort/ar_dlarfg.h"
#include "blas/dcopy.h"
#include "blas/dlanhs.h"
#include "blas/drot.h"
    aster_logical :: wantt
    integer :: ihi, ilo, info, ldh, n
!
!     %-----------------%
!     | ARRAY ARGUMENTS |
!     %-----------------%
!
    real(kind=8) :: h( ldh, * ), wi( * ), wr( * ), z( * )
!
!     %------------%
!     | PARAMETERS |
!     %------------%
!
    real(kind=8) :: zero, one, dat1, dat2
    parameter (zero = 0.0d+0, one = 1.0d+0, dat1 = 7.5d-1,&
     &           dat2 = -4.375d-1)
!
!     %------------------------%
!     | LOCAL SCALARS & ARRAYS |
!     %------------------------%
!
    integer :: i, i1, i2, itn, its, j, k, l, m, nh, nr
    real(kind=8) :: cs, h00, h10, h11, h12, h21, h22, h33, h33s, h43h34, h44
    real(kind=8) :: h44s, s, smlnum, sn, sum, t1, t2, t3, tst1, ulp, unfl, v1
    real(kind=8) :: v2, v3
! DUE TO CRS512      REAL*8 OVFL
    real(kind=8) :: v( 3 ), work( 1 )
!
!     %-----------%
!     | FUNCTIONS |
!     %-----------%
!
!
!     %-----------------------%
!     | EXECUTABLE STATEMENTS |
!     %-----------------------%
!
    call matfpe(-1)
!
! DUE TO CRS513
    work(1) = zero
    info = 0
!
!     %--------------------------%
!     | QUICK RETURN IF POSSIBLE |
!     %--------------------------%
!
    if (n .eq. 0) goto 1000
    if (ilo .eq. ihi) then
        wr( ilo ) = h( ilo, ilo )
        wi( ilo ) = zero
        goto 1000
    endif
!
!     %---------------------------------------------%
!     | INITIALIZE THE VECTOR OF LAST COMPONENTS OF |
!     | THE SCHUR VECTORS FOR ACCUMULATION.         |
!     %---------------------------------------------%
!
    do j = 1, n-1
        z(j) = zero
    end do
    z(n) = one
!
    nh = ihi - ilo + 1
!
!     %-------------------------------------------------------------%
!     | SET MACHINE-DEPENDENT CONSTANTS FOR THE STOPPING CRITERION. |
!     | IF NORM(H) <= SQRT(OVFL), OVERFLOW SHOULD NOT OCCUR.        |
!     %-------------------------------------------------------------%
!
!
    unfl = r8miem()
! DUE TO CRS512      OVFL = ONE / UNFL
    ulp = r8prem() * 0.5d0 * isbaem()
    smlnum = unfl*( nh / ulp )
!
!     %---------------------------------------------------------------%
!     | I1 AND I2 ARE THE INDICES OF THE FIRST ROW AND LAST COLUMN    |
!     | OF H TO WHICH TRANSFORMATIONS MUST BE APPLIED. IF EIGENVALUES |
!     | ONLY ARE COMPUTED, I1 AND I2 ARE SET INSIDE THE MAIN LOOP.    |
!     | ZERO OUT H(J+2,J) = ZERO FOR J=1:N IF WANTT = .TRUE.          |
!     | ELSE H(J+2,J) FOR J=ILO:IHI-ILO-1 IF WANTT = .FALSE.          |
!     %---------------------------------------------------------------%
!
    if (wantt) then
        i1 = 1
        i2 = n
        do i = 1, i2-2
            h(i1+i+1,i) = zero
        end do
    else
        do i = 1, ihi-ilo-1
            h(ilo+i+1,ilo+i-1) = zero
        end do
    endif
!
!     %---------------------------------------------------%
!     | ITN IS THE TOTAL NUMBER OF QR ITERATIONS ALLOWED. |
!     %---------------------------------------------------%
!
    itn = 30*nh
!
!     ------------------------------------------------------------------
!     THE MAIN LOOP BEGINS HERE. I IS THE LOOP INDEX AND DECREASES FROM
!     IHI TO ILO IN STEPS OF 1 OR 2. EACH ITERATION OF THE LOOP WORKS
!     WITH THE ACTIVE SUBMATRIX IN ROWS AND COLUMNS L TO I.
!     EIGENVALUES I+1 TO IHI HAVE ALREADY CONVERGED. EITHER L = ILO OR
!     H(L,L-1) IS NEGLIGIBLE SO THAT THE MATRIX SPLITS.
!     ------------------------------------------------------------------
!
    i = ihi
 10 continue
    l = ilo
    if (i .lt. ilo) goto 150
!
!     %--------------------------------------------------------------%
!     | PERFORM QR ITERATIONS ON ROWS AND COLUMNS ILO TO I UNTIL A   |
!     | SUBMATRIX OF ORDER 1 OR 2 SPLITS OFF AT THE BOTTOM BECAUSE A |
!     | SUBDIAGONAL ELEMENT HAS BECOME NEGLIGIBLE.                   |
!     %--------------------------------------------------------------%
!
    do its = 0, itn
!
!        %----------------------------------------------%
!        | LOOK FOR A SINGLE SMALL SUBDIAGONAL ELEMENT. |
!        %----------------------------------------------%
!
        do k = i, l + 1, -1
            tst1 = abs( h( k-1, k-1 ) ) + abs( h( k, k ) )
            if (tst1 .eq. zero) tst1 = dlanhs('1', i-l+1, h( l, l ), ldh, work)
            if (abs( h( k, k-1 ) ) .le. max( ulp*tst1, smlnum )) goto 30
        end do
 30     continue
        l = k
        if (l .gt. ilo) then
!
!           %------------------------%
!           | H(L,L-1) IS NEGLIGIBLE |
!           %------------------------%
!
            h( l, l-1 ) = zero
        endif
!
!        %-------------------------------------------------------------%
!        | EXIT FROM LOOP IF A SUBMATRIX OF ORDER 1 OR 2 HAS SPLIT OFF |
!        %-------------------------------------------------------------%
!
        if (l .ge. i-1) goto 140
!
!        %---------------------------------------------------------%
!        | NOW THE ACTIVE SUBMATRIX IS IN ROWS AND COLUMNS L TO I. |
!        | IF EIGENVALUES ONLY ARE BEING COMPUTED, ONLY THE ACTIVE |
!        | SUBMATRIX NEED BE TRANSFORMED.                          |
!        %---------------------------------------------------------%
!
        if (.not.wantt) then
            i1 = l
            i2 = i
        endif
!
        if (its .eq. 10 .or. its .eq. 20) then
!
!           %-------------------%
!           | EXCEPTIONAL SHIFT |
!           %-------------------%
!
            s = abs( h( i, i-1 ) ) + abs( h( i-1, i-2 ) )
            h44 = dat1*s
            h33 = h44
            h43h34 = dat2*s*s
!
        else
!
!           %-----------------------------------------%
!           | PREPARE TO USE WILKINSON'S DOUBLE SHIFT |
!           %-----------------------------------------%
!
            h44 = h( i, i )
            h33 = h( i-1, i-1 )
            h43h34 = h( i, i-1 )*h( i-1, i )
        endif
!
!        %-----------------------------------------------------%
!        | LOOK FOR TWO CONSECUTIVE SMALL SUBDIAGONAL ELEMENTS |
!        %-----------------------------------------------------%
!
        do m = i - 2, l, -1
!
!           %---------------------------------------------------------%
!           | DETERMINE THE EFFECT OF STARTING THE DOUBLE-SHIFT QR    |
!           | ITERATION AT ROW M, AND SEE IF THIS WOULD MAKE H(M,M-1) |
!           | NEGLIGIBLE.                                             |
!           %---------------------------------------------------------%
!
            h11 = h( m, m )
            h22 = h( m+1, m+1 )
            h21 = h( m+1, m )
            h12 = h( m, m+1 )
            h44s = h44 - h11
            h33s = h33 - h11
            v1 = ( h33s*h44s-h43h34 ) / h21 + h12
            v2 = h22 - h11 - h33s - h44s
            v3 = h( m+2, m+1 )
            s = abs( v1 ) + abs( v2 ) + abs( v3 )
            v1 = v1 / s
            v2 = v2 / s
            v3 = v3 / s
            v( 1 ) = v1
            v( 2 ) = v2
            v( 3 ) = v3
            if (m .eq. l) goto 50
            h00 = h( m-1, m-1 )
            h10 = h( m, m-1 )
            tst1 = abs( v1 )*( abs( h00 )+abs( h11 )+abs( h22 ) )
            if (abs( h10 )*( abs( v2 )+abs( v3 ) ) .le. ulp*tst1) goto 50
        end do
 50     continue
!
!        %----------------------%
!        | DOUBLE-SHIFT QR STEP |
!        %----------------------%
!
        do k = m, i - 1
!
!           ------------------------------------------------------------
!           THE FIRST ITERATION OF THIS LOOP DETERMINES A REFLECTION G
!           FROM THE VECTOR V AND APPLIES IT FROM LEFT AND RIGHT TO H,
!           THUS CREATING A NONZERO BULGE BELOW THE SUBDIAGONAL.
!
!           EACH SUBSEQUENT ITERATION DETERMINES A REFLECTION G TO
!           RESTORE THE HESSENBERG FORM IN THE (K-1)TH COLUMN, AND THUS
!           CHASES THE BULGE ONE STEP TOWARD THE BOTTOM OF THE ACTIVE
!           SUBMATRIX. NR IS THE ORDER OF G.
!           ------------------------------------------------------------
!
            nr = min( 3, i-k+1 )
            if (k .gt. m) call dcopy(nr, h( k, k-1 ), 1, v, 1)
            call ar_dlarfg(nr, v( 1 ), v( 2 ), 1, t1)
            if (k .gt. m) then
                h( k, k-1 ) = v( 1 )
                h( k+1, k-1 ) = zero
                if (k .lt. i-1) h( k+2, k-1 ) = zero
            else if (m.gt.l) then
                h( k, k-1 ) = -h( k, k-1 )
            endif
            v2 = v( 2 )
            t2 = t1*v2
            if (nr .eq. 3) then
                v3 = v( 3 )
                t3 = t1*v3
!
!              %------------------------------------------------%
!              | APPLY G FROM THE LEFT TO TRANSFORM THE ROWS OF |
!              | THE MATRIX IN COLUMNS K TO I2.                 |
!              %------------------------------------------------%
!
                do j = k, i2
                    sum = h( k, j ) + v2*h( k+1, j ) + v3*h( k+2, j )
                    h( k, j ) = h( k, j ) - sum*t1
                    h( k+1, j ) = h( k+1, j ) - sum*t2
                    h( k+2, j ) = h( k+2, j ) - sum*t3
                end do
!
!              %----------------------------------------------------%
!              | APPLY G FROM THE RIGHT TO TRANSFORM THE COLUMNS OF |
!              | THE MATRIX IN ROWS I1 TO MIN(K+3,I).               |
!              %----------------------------------------------------%
!
                do j = i1, min( k+3, i )
                    sum = h( j, k ) + v2*h( j, k+1 ) + v3*h( j, k+2 )
                    h( j, k ) = h( j, k ) - sum*t1
                    h( j, k+1 ) = h( j, k+1 ) - sum*t2
                    h( j, k+2 ) = h( j, k+2 ) - sum*t3
                end do
!
!              %----------------------------------%
!              | ACCUMULATE TRANSFORMATIONS FOR Z |
!              %----------------------------------%
!
                sum = z( k ) + v2*z( k+1 ) + v3*z( k+2 )
                z( k ) = z( k ) - sum*t1
                z( k+1 ) = z( k+1 ) - sum*t2
                z( k+2 ) = z( k+2 ) - sum*t3
!
            else if (nr.eq.2) then
!
!              %------------------------------------------------%
!              | APPLY G FROM THE LEFT TO TRANSFORM THE ROWS OF |
!              | THE MATRIX IN COLUMNS K TO I2.                 |
!              %------------------------------------------------%
!
                do j = k, i2
                    sum = h( k, j ) + v2*h( k+1, j )
                    h( k, j ) = h( k, j ) - sum*t1
                    h( k+1, j ) = h( k+1, j ) - sum*t2
                end do
!
!              %----------------------------------------------------%
!              | APPLY G FROM THE RIGHT TO TRANSFORM THE COLUMNS OF |
!              | THE MATRIX IN ROWS I1 TO MIN(K+3,I).               |
!              %----------------------------------------------------%
!
                do j = i1, i
                    sum = h( j, k ) + v2*h( j, k+1 )
                    h( j, k ) = h( j, k ) - sum*t1
                    h( j, k+1 ) = h( j, k+1 ) - sum*t2
                end do
!
!              %----------------------------------%
!              | ACCUMULATE TRANSFORMATIONS FOR Z |
!              %----------------------------------%
!
                sum = z( k ) + v2*z( k+1 )
                z( k ) = z( k ) - sum*t1
                z( k+1 ) = z( k+1 ) - sum*t2
            endif
        end do
!
    end do
!
!     %-------------------------------------------------------%
!     | FAILURE TO CONVERGE IN REMAINING NUMBER OF ITERATIONS |
!     %-------------------------------------------------------%
!
    info = i
    goto 1000
!
140 continue
!
    if (l .eq. i) then
!
!        %------------------------------------------------------%
!        | H(I,I-1) IS NEGLIGIBLE: ONE EIGENVALUE HAS CONVERGED |
!        %------------------------------------------------------%
!
        wr( i ) = h( i, i )
        wi( i ) = zero
!
    else if (l.eq.i-1) then
!
!        %--------------------------------------------------------%
!        | H(I-1,I-2) IS NEGLIGIBLE,                              |
!        | A PAIR OF EIGENVALUES HAVE CONVERGED.                  |
!        |                                                        |
!        | TRANSFORM THE 2-BY-2 SUBMATRIX TO STANDARD SCHUR FORM, |
!        | AND COMPUTE AND STORE THE EIGENVALUES.                 |
!        %--------------------------------------------------------%
!
        call ar_dlanv2(h( i-1, i-1 ), h( i-1, i ), h( i, i-1 ), h( i, i ), wr( i-1 ),&
                    wi( i-1 ), wr( i ), wi( i ), cs, sn)
!
        if (wantt) then
!
!           %-----------------------------------------------------%
!           | APPLY THE TRANSFORMATION TO THE REST OF H AND TO Z, |
!           | AS REQUIRED.                                        |
!           %-----------------------------------------------------%
!
            if (i2 .gt. i) call drot(i2-i, h( i-1, i+1 ), ldh, h( i, i+1 ), ldh,&
                                     cs, sn)
            call drot(i-i1-1, h( i1, i-1 ), 1, h( i1, i ), 1,&
                      cs, sn)
            sum = cs*z( i-1 ) + sn*z( i )
            z( i ) = cs*z( i ) - sn*z( i-1 )
            z( i-1 ) = sum
        endif
    endif
!
!     %---------------------------------------------------------%
!     | DECREMENT NUMBER OF REMAINING ITERATIONS, AND RETURN TO |
!     | START OF THE MAIN LOOP WITH NEW VALUE OF I.             |
!     %---------------------------------------------------------%
!
    itn = itn - its
    i = l - 1
    goto 10
!
150 continue
1000 continue
!
    call matfpe(1)
!
!     %---------------%
!     | END OF DLAQRB |
!     %---------------%
!
end subroutine
