subroutine postsm(option, fm, df, sigm, sigp,&
                  dsidep)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
#include "asterf_types.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
    character(len=16) :: option
    real(kind=8) :: fm(3, 3), df(3, 3), sigm(6), sigp(6), dsidep(6, 3, 3)
!
!------------------------------------------------------------
!   IN  OPTION : OPTION DEMANDEE : RIGI_MECA_TANG, FULL_MECA, RAPH_MECA
!   IN  FM : GRADIENT DE LA TRANSFORMATION EN T-
!   IN  DF : GRADIENT DE LA TRANSFORMATION DE T- A T+
!   IN  SIGM : CONTRAINTE EN T-
!   IN/OUT  SIGP : CONTRAINTE CAUCHY EN T+ -> CONTRAINTE KIRCHHOF EN T+
!   IN/OUT  DSIDEP : MATRICE TANGENTE D(SIG)/DF  ->
!                    D(TAU)/D(FD) * (FD)t
!-----------------------------------------------------------------------
    aster_logical :: resi, rigi
    integer :: kl, p, q, i
    real(kind=8) :: jm, dj, jp, tau(6), j, mat(6, 3, 3), id(3, 3)
!
    data    id   /1.d0, 0.d0, 0.d0,&
     &              0.d0, 1.d0, 0.d0,&
     &              0.d0, 0.d0, 1.d0/
!
!
    resi = option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL'
    rigi = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
!
    jm=fm(1,1)*(fm(2,2)*fm(3,3)-fm(2,3)*fm(3,2))&
     &  -fm(2,1)*(fm(1,2)*fm(3,3)-fm(1,3)*fm(3,2))&
     &  +fm(3,1)*(fm(1,2)*fm(2,3)-fm(1,3)*fm(2,2))
!
    dj=df(1,1)*(df(2,2)*df(3,3)-df(2,3)*df(3,2))&
     &  -df(2,1)*(df(1,2)*df(3,3)-df(1,3)*df(3,2))&
     &  +df(3,1)*(df(1,2)*df(2,3)-df(1,3)*df(2,2))
!
    jp=jm*dj
!
    if (resi) then
        call dscal(6, jp, sigp, 1)
        call dcopy(6, sigp, 1, tau, 1)
        j = jp
    else
        call dcopy(6, sigm, 1, tau, 1)
        call dscal(6, jm, tau, 1)
        j = jm
    endif
!
    if (rigi) then
        call dcopy(54, dsidep, 1, mat, 1)
        call dscal(54, j, mat, 1)
        do 100 kl = 1, 6
            do 200 p = 1, 3
                do 300 q = 1, 3
                    dsidep(kl,p,q) = tau(kl)*id(p,q)
                    do 400 i = 1, 3
                        dsidep(kl,p,q) = dsidep(kl,p,q) + mat(kl,p,i)* df(q,i)
400                 continue
300             continue
200         continue
100     continue
    endif
!
end subroutine
