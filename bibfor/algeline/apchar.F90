subroutine apchar(typcha, k24rc, nk, lambda, theta,&
                  lraide, lmasse, ldynam, solveu, lamor,&
                  lc, impr, ifapm, ind)
    implicit none
#include "jeveux.h"
!
#include "asterc/r8miem.h"
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtcmbl.h"
#include "asterfort/mtdete.h"
#include "asterfort/mtdscr.h"
#include "asterfort/preres.h"
#include "asterfort/u2mesr.h"
    integer :: nk, lraide, lmasse, ldynam, lamor, ifapm, ind
    logical :: lc
    real(kind=8) :: theta
    complex(kind=8) :: lambda
    character(len=3) :: impr
    character(len=8) :: typcha
    character(len=19) :: solveu
    character(len=24) :: k24rc
!     ------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     SUBROUTINE THAT COMPUTE THE CHARACTERISTIC POLYNOMIAL OF AN EIGEN
!     VALUE PROBLEM. TWO METHODS ARE AVAILABLE:
!       TYPCHA='ROMBOUT' WITH THE USE OF THE COEFFICIENTS OF THE POLY
!                         NOMIAL. COEFFICIENTS PREVIOUSLY COMPUTED.
!       TYPCHA='LDLT' WITH AN LDLT FACTORIZATION
!     ------------------------------------------------------------------
! IN TYPCHA : K8 : TYPE OF METHOD TO EVALUATE THE CHARAC. POLYNOMIAL
! IN JMATC  : IN : JEVEUX ADRESS OF THE VECTOR THAT CONTAIN THE COEFFI
!                  CIENTS OF THE CHARACTERISTIC POLYNOMIAL
! IN NK     : IN : SIZE OF THE EIGENVALUE PROBLEM
! IN LAMBDA : C16: VALUE OF ONE CHECKING POINT OF THE CHECK-SHAPE
! OUT THETA : R8 : ARGUMENT OF THE CHARAC. POLYNOMIAL ON LAMBDA
! IN LRAIDE : IN : JEVEUX ATTRIBUT OF THE STIFFNESS MATRIX
! IN LMASSE : IN : JEVEUX ATTRIBUT OF THE MASS MATRIX
! IN LDYNAM : IN : JEVEUX ATTRIBUT OF THE DYNAMIC MATRIX
! IN SOLVEU : K19: JEVEUX SD OF THE LINEAR SOLVER
! IN K24RC  : K24: JEVEUX NAME OF THE COEFFICIENTS OF THE CHARACTERIS
!                  TIC POLYNOMIAL
! IN LAMOR  : IN : JEVEUX ATTRIBUT OF THE DAMPING MATRIX
! IN LC     : LOG: FLAG THAT INDICATES IF THE PB IS QUADRATIC OR NOT
! IN IMPR/IFAPM: IN/K3 : PRINT PARAMETERS FOR DEBBUGING
! IN IND   : IN : NUMEROUS OF THE CHECK POINT (FOR PRINT/DEBUGGING ONLY)
!     ------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
!
    integer :: j, nkm1, nbcmb, ibid, iret, jmatc, islvk, islvi, lmatsh
    real(kind=8) :: rauxx, rauxy, rauxm, prec, prec1, pi, coef(6), valr(2), rmin
    real(kind=8) :: rayon, r8bid, rindc
    complex(kind=8) :: caux2
    character(len=1) :: typcst(3)
    character(len=8) :: nomddl
    character(len=19) :: matpre
    character(len=24) :: nmat(3), nmatsh, metres
!
!     ------------------------------------------------------------------
    data typcst /'C','C','C'/
    data nomddl /'        '/
!     ------------------------------------------------------------------
!
!   --- MISCELLANEOUS ---
    call jemarq()
    prec=r8prem()*100.d0
    prec1=1.d0-prec
    pi=r8pi()
    nkm1=nk-1
    rmin=r8miem()*100
!
!     ------------------------------------------------------------------
!     ------------------- METHOD ROMBOUT -------------------------------
!     ------------------------------------------------------------------
!
    if (typcha(1:7) .eq. 'ROMBOUT') then
!   --- COMPUTATION OF THE CHARACTERISTIC POLYNOMIAL THANKS TO THE ---
!   --- CLASSICAL HORNER SCHEME. IT CAUSES NUMERICAL ERRORS AND SO ---
!   --- WE TRIED, WITH NO SUFFICIENT SUCESS UNTIL NOW, TO FIX THEM ---
!   --- WITH THE HELP OF COMPENSATED ARITHMETIC                    ---
!   --- (CF. LANGLOIS, GRAILLAT, LOUVET).                          ---
        call jeveuo(k24rc, 'L', jmatc)
        caux2=zc(jmatc+nk)
        do 10 j = nkm1, 0, -1
            caux2=caux2*lambda+zc(jmatc+j)
10      continue
!
!     ------------------------------------------------------------------
!     ------------------- METHOD LDLT ----------------------------------
!     ------------------------------------------------------------------
    else if (typcha(1:4).eq.'LDLT') then
!   --- COMPUTATION THANKS TO THE TRADITIONNAL LDLT FACTORIZATION ---
!   --- STEP 1: COMPUTATION OF THE SHIFTED DYNAMIC MATRIX         ---
!
        if (.not.lc) then
!   --- COMPUTE DYNAM=(1.D0,0.D0)*RAIDE - (RE(LAMBDA),IM(LAMBDA))*MASSE
            nbcmb = 2
            coef(1) = 1.d0
            coef(2) = 0.d0
            coef(3) = -dble(lambda)
            coef(4) = -dimag(lambda)
            nmat(1) = zk24(zi(lraide+1))
            nmat(2) = zk24(zi(lmasse+1))
            nmatsh = zk24(zi(ldynam+1))
        else
            nbcmb = 3
            coef(1) = dble(lambda*lambda)
            coef(2) = dimag(lambda*lambda)
            coef(3) = dble(lambda)
            coef(4) = dimag(lambda)
            coef(5) = 1.d0
            coef(6) = 0.d0
            nmat(1) = zk24(zi(lmasse+1))
            nmat(2) = zk24(zi(lamor+1))
            nmat(3) = zk24(zi(lraide+1))
            nmatsh = zk24(zi(ldynam+1))
        endif
        call mtdscr(nmatsh)
        call jeveuo(nmatsh(1:19)//'.&INT', 'E', lmatsh)
        call mtcmbl(nbcmb, typcst, coef, nmat, nmatsh,&
                    nomddl, ' ', 'ELIM=')
!
!   --- STEP 1.5: IF LINEAR SOLVER='MUMPS'
!   --- WE CHANGE TWO PARAMETERS OF THE SD_SOLVER RECORD TO ORDER MUMPS
!   --- TO COMPUTE THE DETERMINANT WITHOUT KEEPING THE FACTORS
        call jeveuo(solveu//'.SLVK', 'L', islvk)
        metres=zk24(islvk)
        call jeveuo(solveu//'.SLVI', 'E', islvi)
        if (metres(1:5) .eq. 'MUMPS') then
            zi(islvi-1+4)=1
            zi(islvi-1+5)=1
        endif
!
!   --- STEP 2: FACTORIZATION OF THIS DYNAMIC MATRIX              ---
        matpre=' '
        iret=0
        matpre=' '
        call preres(solveu, 'V', iret, matpre, nmatsh(1:19),&
                    ibid, 2)
        valr(1)=dble(lambda)
        valr(2)=dimag(lambda)
!
        if (iret .ge. 1) then
!   --- ERROR CASE: LAMBDA IS CLOSE TO AN EIGENVALUE              ---
!   --- OR THE LINEAR SOLVER FAILED                               ---
            call u2mesr('F', 'ALGELINE4_15', 2, valr)
        else
!
!   --- STEP 3: COMPUTATION OF THE NORMALIZED CHARAC. POLYNOMIAL  ---
!   --- IT'S OK BECAUSE ONLY THE ARGUMENT INTERESTED US           ---
!   --- THIS IS A TIP TO PREVENT OVERFLOW WHEN MULTPLYING NUMBERS ---
!   --- THIS TIP IS ACTIVED ONLY FOR LDLT AND MULT_FRONT LINEAR   ---
!   --- SOLVER. FOR MUMPS, WE USE THE DETERMINANT COMPUTE BY THE  ---
!   --- THE TOOL.                                                 ---
            call mtdete(2, metres, lmatsh, r8bid, ibid,&
                        caux2)
        endif
!
    else
!
!   --- ILLEGAL OPTION ---
        call assert(.false.)
!
    endif
!
!
!   --- PROCEDURE TO CORRECT SOME APPROXIMATIONS ---
!   --- IN THE COMPUTATION OF ARG(PC(LAMBDA))    ---
    rauxx=dble(caux2)
    rauxy=dimag(caux2)
    rauxm=sqrt(rauxx*rauxx+rauxy*rauxy)
    if (rauxm .lt. rmin) rauxm=1.d0
    rauxx=rauxx/rauxm
    rauxy=rauxy/rauxm
    if (abs(rauxx) .lt. prec) rauxx=prec
    if (abs(rauxx) .gt. prec1) rauxx=sign(1.d0,rauxx)
    if (abs(rauxy) .lt. prec) rauxy=prec
    if (abs(rauxy) .gt. prec1) rauxy=sign(1.d0,rauxy)
    theta=atan2(rauxy,rauxx)
    if (abs(theta) .lt. prec) theta=0.d0
    if (theta .lt. 0.d0) theta=2*pi+theta
!
!   --- FOR DEBUGING ONLY ---
    if (impr .eq. 'OUI') then
!   --- SCALE COEFFICIENT TO MATCH THE VISUALISATION CURVES ---
!   --- RINDC=NB_POINT_CONTOUR_REF/NB_POINT_CONTOUR_TEST    ---
        rindc=1000.d0/10.d0
        rindc=1.d0
        rayon=1.d0+ind*(1.d0/50.d0)*rindc
        write(ifapm,*)rayon*dble(caux2),rayon*dimag(caux2)
    endif
    call jedema()
end subroutine
