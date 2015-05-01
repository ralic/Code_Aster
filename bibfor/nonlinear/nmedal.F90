subroutine nmedal(alphap, sigmc, gc, s, q,&
                  seuil)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/diago2.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
    real(kind=8) :: alphap(2), seuil
    real(kind=8) :: s(2), q(2, 2)
    real(kind=8) :: sigmc, gc
!
!----------------------------------------------------------------
!   CALCUL DU SAUT ALPHA DANS l'ELEMENT A DISCONTINUITE INTERNE :
!   RESOLUTION DU SYSTEME NON LINEAIRE S+Q.ALPHA=VECT_SIG(ALPHA)
!
! IN  : S,Q,SEUIL,SIGMC ET GC
! OUT : ALPHAP (SAUT A L'INSTANT +)
!
!-----------------------------------------------------------------
!
    aster_logical :: critg, critd
    integer :: i, j, k
    real(kind=8) :: x, xp, fx, dfx, gx, dgx, norms, norma, eta, det
    real(kind=8) :: p(2, 2), valp(2), sp(2), pa(2), qvect(3)
    real(kind=8) :: temp1, temp2, temp3, temp4, tempal(2), mat(3), sign
!
! INITIALISATIONS :
!
    call r8inir(2, 0.d0, alphap, 1)
    eta = 1.d-12
    norms = sqrt( s(1)*s(1) + s(2)*s(2) )
    critg = (s(1).le.0.d0) .and. (abs(s(2)).le.sigmc)
    critd = (s(1).gt.0.d0) .and. (norms .le. sigmc)
!
!        *****************************
!     I. CAS DANS CRITERE ET SEUIL NUL
!        *****************************
!
    if ((seuil.eq.0.d0) .and. (critg .or. critd)) then
        alphap(1) = 0.d0
        alphap(2) = 0.d0
        goto 999
    endif
!
!         **********************
!     II. CAS DECHARGE ELASTIQUE
!         **********************
!
    if (seuil .ne. 0.d0) then
!
!       ON SUPPOSE QUE ALPHAP1=0, ON CALCUL ALPHAP2 (TEMPAL(2)) :
!
        call r8inir(2, 0.d0, tempal, 1)
        tempal(2) = s(2)/(-q(2,2)+ sigmc*exp(-sigmc*seuil/gc)/seuil)
!
        norma = abs(tempal(2))
        sign = s(1) + q(1,2)*tempal(2)
!
!     SI SIGMA NORMALE EST <=0 ET QU'ON EST SOUS LE SEUIL
        if (((sign).le.1.d-10) .and. (norma.lt.seuil)) then
            alphap(1) = 0.d0
            alphap(2) = tempal(2)
            goto 999
        endif
!
!       ON SUPPOSE QUE ALPHA1.NE.0, ON CALCUL ALPHAP1 et ALPHAP2 :
!       RESOLTION DE (V.ID-Q)ALPHAP=S AVEC
!       V = EXP(-SIGMC*NORMA/GC)/NORMA . ON MET (vID-Q) DANS MAT,
!
        call r8inir(3, 0.d0, mat, 1)
        mat(1) = sigmc*exp(-sigmc*seuil/gc)/seuil - q(1,1)
        mat(2) = sigmc*exp(-sigmc*seuil/gc)/seuil - q(2,2)
        mat(3) = -q(1,2)
!
        det = mat(1)*mat(2)-mat(3)*mat(3)
!
        ASSERT(abs(det).gt.1.d-12)
!
        call r8inir(2, 0.d0, tempal, 1)
        tempal(1) = ( mat(2)*s(1) - mat(3)*s(2)) / det
        tempal(2) = (-mat(3)*s(1) + mat(1)*s(2)) / det
!
        norma = sqrt( tempal(1)**2 + tempal(2)**2 )
!
        if (norma .lt. seuil) then
            alphap(1) = tempal(1)
            alphap(2) = tempal(2)
            goto 999
        endif
!
    endif
!
!        **********************
!    III. CAS SORTIE DU CRITERE
!        **********************
!
!       1) ON SUPPOSE ALPHAP1.EQ.0, CALCUL DE ALPHAP2
!          --------------------------------------------
!
!     TESTS D'EXISTANCE ET D'UNICITE D'UNE SOLUTION
    if (q(2,2) .gt. 0.d0) then
        call utmess('F', 'ALGORITH7_63')
    endif
!
    if (abs(q(2,2)) .lt. (sigmc*sigmc/gc)) then
        call utmess('F', 'ALGORITH7_64')
    endif
!
!     CALCUL DE ALPHAP(2) PAR NEWTON
    if (s(2) .gt. sigmc) then
!
        k=0
        xp=0.d0
2000     continue
!
        x = xp
        fx = s(2) + q(2,2)*x - sigmc*exp(-sigmc*abs(x)/gc)
        dfx = q(2,2) + sigmc*sigmc*exp(-sigmc*abs(x)/gc)/gc
        xp = x - fx/dfx
        k=k+1
!
        if ((abs(xp-x).ge.1.d10) .or. (k.gt.3000)) then
            call utmess('F', 'ALGORITH7_65')
        endif
        if (abs(xp-x) .le. eta) goto 1000
        goto 2000
1000     continue
!
    else if (s(2).lt.-sigmc) then
!
        k=0
        xp=0.d0
3000     continue
!
        x = xp
        fx = s(2) + q(2,2)*x + sigmc*exp(-sigmc*abs(x)/gc)
        dfx = q(2,2) - sigmc*sigmc*exp(-sigmc*abs(x)/gc)/gc
        xp = x - fx/dfx
        k=k+1
!
        if ((abs(xp-x).ge.1.d10) .or. (k.gt.3000)) then
            call utmess('F', 'ALGORITH7_66')
        endif
!
        if (abs(xp-x) .le. eta) goto 4000
        goto 3000
4000     continue
!
    endif
!
!     ON VERIFIE SI XP A ETE CALCULE DANS UN DES DEUX CAS PRECEDENTS
    if ((s(2).gt.sigmc) .or. (s(2).lt.-sigmc)) then
!
        norma = abs(xp)
        sign = s(1) + q(1,2)*xp
!
!       SI SIGMA NORMALE EST <=0 ET QU'ON EST AU DELA DU SEUIL ALORS
        if (((sign).le.1.d-10) .and. (norma.ge.seuil)) then
            alphap(1) = 0.d0
            alphap(2) = xp
            goto 999
        endif
!
    endif
!
!       2) ON SUPPOSE ALPHAP(1).NE.0, CALCUL DE ALPHAP(1) ET ALPHAP(2)
!          -----------------------------------------------------------
!
! ECRITURE DE Q EN VECTEUR : QVECT(3)= (Q(1,1)  ,  Q(2,2)  ,  Q(1,2))
    qvect(1) = q(1,1)
    qvect(2) = q(2,2)
    qvect(3) = q(1,2)
!
!     DIAGONALISATION DE Q :
    call r8inir(4, 0.d0, p, 1)
    call r8inir(2, 0.d0, valp, 1)
    call diago2(qvect, p, valp)
!
!     S EXPRIME DANS LA BASE PROPRE P DE Q :
    call r8inir(2, 0.d0, sp, 1)
    do 10 i = 1, 2
        do 20 j = 1, 2
            sp(i) = sp(i) + p(j,i)*s(j)
 20     continue
 10 end do
!
!     CALCUL DE ALPHAP(1) ET ALPHAP(2) PAR NEWTON :
!
    k=0
    xp=0.d0
5000 continue
!
    x = xp
!
    gx = sigmc*exp(-sigmc*x/gc)
    dgx = - (sigmc*sigmc/gc)*exp(-sigmc*x/gc)
!
    temp1 = sp(1)/(gx-valp(1)*x)
    temp2 = sp(2)/(gx-valp(2)*x)
    temp3 = (dgx-valp(1))/(gx-x*valp(1))
    temp4 = (dgx-valp(2))/(gx-x*valp(2))
!
    fx = 1.d0 - temp1*temp1 - temp2*temp2
    dfx = 2*(temp1*temp1*temp3 + temp2*temp2*temp4)
    xp = x - fx/dfx
    k=k+1
!
    if ((abs(xp-x).ge.1.d10) .or. (k.gt.3000)) then
        call utmess('F', 'ALGORITH7_67')
    endif
!
    if (abs(xp-x) .le. eta) goto 6000
    goto 5000
6000 continue
!
    pa(1) = sp(1)/( (sigmc/xp)*exp(-sigmc*xp/gc) - valp(1) )
    pa(2) = sp(2)/( (sigmc/xp)*exp(-sigmc*xp/gc) - valp(2) )
!
!    ON REMET LA SOLUTION DS LA BONNE BASE ET ON LA STOCKE DANS TEMPAL
!
    call r8inir(2, 0.d0, tempal, 1)
    do 30 i = 1, 2
        do 40 j = 1, 2
            tempal(i) = tempal(i) + p(i,j)*pa(j)
 40     continue
 30 end do
!
    norma = sqrt( tempal(1)**2 + tempal(2)**2 )
!
!     SI ON EST AU DELA DU SEUIL :
    if (norma .gt. seuil) then
!
        alphap(1) = tempal(1)
        alphap(2) = tempal(2)
        goto 999
!
    endif
!
!     ON EST PASSE DANS AUCUN TEST :
    call utmess('F', 'ALGORITH7_68')
!
999 continue
!
!
end subroutine
