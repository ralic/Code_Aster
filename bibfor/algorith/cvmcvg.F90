subroutine cvmcvg(dy, ddy, nr, itmax, toler,&
                  iter, intg, typess, essai, icomp,&
                  irteti)
    implicit none
!       ================================================================
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!       VISCOCHABOCHE        : CONTROLE DE LA CONVERGENCE
!                                  DE LA CONFORMITE DE LA SOLUTION DP
!                                  DE LA RE-INTEGRATION
!                                  ET DU REDECOUPAGE DU PAS DE TEMPS
!       ----------------------------------------------------------------
!       IN   ITMAX  :  NB MAXI D ITERATIONS LOCALES
!            TOLER  :  TOLERANCE A CONVERGENCE
!            ITER   :  NUMERO ITERATION COURANTE
!            NR     :  DIMENSION DY DDY
!            DY     :  VECTEUR SOLUTION DY = ( DSIG DP DX1 )
!            DDY    :  VECTEUR CORRECTION SUR LA SOLUTION
!            ICOMP  :  COMPTEUR POUR LE REDECOUPAGE DU PAS DE TEMPS
!       VAR  INTG   :  NUMERO INTEGRATION COURANTE
!       OUT  ESSAI  :  SOLUTION D ESSAI
!            TYPESS :  TYPE DE SOLUTION D ESSAI
!                               0 = NUL(0)
!                               1 = ELASTIQUE
!                               2 = EXPLICITE
!                               3 = ESSAI
!            IRTETI = 0:  CONVERGENCE
!            IRTETI = 1:  ITERATION SUIVANTE
!            IRTETI = 2:  RE-INTEGRATION
!            IRTETI = 3:  REDECOUPAGE DU PAS DE TEMPS
!       ----------------------------------------------------------------
#include "asterfort/codent.h"
#include "asterfort/codree.h"
#include "asterfort/lcverr.h"
#include "asterfort/utmess.h"
    integer :: typess, itmax, iter, intg, nr, icomp
    real(kind=8) :: toler, essai, ddy(*), dy(*)
!       ----------------------------------------------------------------
    real(kind=8) :: tolim, dplim
!-----------------------------------------------------------------------
    integer :: i, irteti
!-----------------------------------------------------------------------
    parameter       ( dplim = 1.d-10 )
    parameter       ( tolim = 1.d-3  )
!
    integer :: ndt, ndi, vali
    integer :: itsup, ndp
    real(kind=8) :: ter(100), err(1)
    real(kind=8) :: der(10), dp, valr
    character(len=10) :: cdp, ctol, citer, cintg
    character(len=24) :: valk(2)
    save            itsup,ter
    common /tdim/   ndt , ndi
!       ----------------------------------------------------------------
    data itsup      /0/
!
!
! -- ICOMP = 0 ==> PAS DE REDECOUPAGE EN COURS
! -- ICOMP = 1 ==> 1 REDECOUPAGE EN COURS, UN DEUXIEME REDECOUPAGE EST
!                  POSSIBLE
! -- ICOMP = 2 ==> PAS DE REDECOUPAGE
!
!
    ndp = 3 * ndt+1
    dp = dy(ndp)
    irteti = 0
!
! -     EVALUATION  DE L'ERREUR RELATIVE EN DY, ERR =  !!DDY!!/!!DY!!
!
! -------------TEMPORAIRE-------------
!         ERR=DDY(NDP)/DY(NDP)
!
!        CALL LCVERR ( DY, DDY, NR, 2, ERR  )
!        PRINT *," --- ITERATION ",ITER," ERREUR 2= ",ERR
    call lcverr(dy, ddy, nr, 1, err)
!        PRINT *," --- ITERATION ",ITER," ERREUR 1= ",ERR
    ter(iter) = err(1)
!
! -     CAS DE DP NEGATIF
!       -----------------
!
    if (dp .lt. 0.d0) then
!
! -             SI -DP < 1.D-10 ET ERR < TOLER
!
        if (abs(dp) .lt. dplim .and. err(1) .lt. toler) then
            call codree(abs(dp), 'E', cdp)
            call utmess('A', 'ALGORITH2_54', sk=cdp)
            irteti = 0
            goto 9999
        endif
!
! -     SI ITER > 3 ,ON ESSAYE AVEC UNE SOLUTION DE DEPART ELASTIQUE
!
        if (iter .ge. 3) then
            intg = intg + 1
            if (intg .eq. 1) then
                typess = 1
                irteti = 2
                goto 9999
!
! -     SI ITER > 3 ,ON ESSAYE AVEC DIFFERENTES VALEURS POUR ESSAI
!
            else if (intg .eq. 2) then
                essai = 1.d-25
                typess = 3
                irteti = 2
                goto 9999
            else if (intg .eq. 3) then
                essai = 1.d-2
                typess = 3
                irteti = 2
                goto 9999
            else if (intg .eq. 4) then
                essai = 1.d-10
                typess = 3
                irteti = 2
                goto 9999
            else if (intg .eq. 5) then
                essai = 1.d-5
                typess = 3
                irteti = 2
                goto 9999
            else if (intg .eq. 6) then
                essai = 1.d-20
                typess = 3
                irteti = 2
                goto 9999
            else if (intg .eq. 7) then
                essai = 1.d-15
                typess = 3
                irteti = 2
                goto 9999
            else if (intg .eq. 8) then
!
                if (icomp .eq. 0 .or. icomp .eq. 1) then
                    call codent(intg, 'G', cintg)
                    call codree(abs(dp), 'E', cdp)
                    valk(1) = cintg
                    valk(2) = cdp
                    call utmess('I', 'ALGORITH2_55', nk=2, valk=valk)
                    irteti = 3
                    goto 9999
                else
                    vali = intg
                    valr = dp
                    call utmess('Z', 'ALGORITH16_60', si=vali, sr=valr, num_except=23)
                endif
            endif
!
! -         SINON ITERATION SUIVANTE
!
        else
            irteti = 1
            goto 9999
        endif
!
! -     CAS DE DP POSITIF
!       -----------------
!
    else if (dp .ge. 0.d0) then
!
! -         ITER < ITMAX
!           ------------
!
        if (iter .lt. itmax) then
!
! -             CONVERGENCE
!
            if (err(1) .le. toler) then
                irteti = 0
                goto 9999
            else
!
! -             NON CONVERGENCE ITERATION SUIVANTE
!
                irteti = 1
                goto 9999
            endif
!
! -         ITER >= ITMAX
!           ------------
!
        else if (iter .ge. itmax) then
!
! -             NON CONVERGENCE ET ITMAX ATTEINT
!
            if (err(1) .gt. toler) then
!
! -               ITER >= 6
!
                if (iter .ge. 6) then
!
                    do 20 i = 1, 5
                        der(i) = abs(ter(iter-i-1) - ter(iter-i))
20                  continue
!
! -                 CONVERGENCE REGULIERE SUR LES 5 DERNIERES ITERATIONS
!
                    if ((&
                        ter(iter) .lt. ter(iter-1) .and. ter(iter- 1) .lt. ter(iter-2)&
                        .and. ter(iter-2) .lt. ter( iter-3) .and. ter(iter-3) .lt.&
                        ter(iter-4) .and. ter(iter-4) .lt. ter(iter-5)&
                        )&
                        .or.&
                        (&
                        der(1) .lt. der(2) .and. der(2) .lt. der(3) .and. der(3) .lt.&
                        der(4) .and. der(4) .lt. der(5)&
                        )) then
!
                        itsup = itsup + 1
!
! -                     SI ERR < TOLIM ET DP < DPLIM , ON ACCEPTE
!
                        if (err(1) .lt. tolim .and. dp .lt. dplim) then
                            irteti = 0
                            goto 9999
!
! -                     SINON ON ESSAIE ENCORE 10 ITERATIONS ..
!
                        else if (itsup .lt. 10) then
                            irteti = 1
                            goto 9999
!
! -                     SINON STOP
!
                        else
                            if (icomp .eq. 0 .or. icomp .eq. 1) then
                                call codent(iter, 'G', citer)
                                call codree(toler, 'E', ctol)
                                valk(1) = citer
                                valk(2) = ctol
                                call utmess('I', 'ALGORITH2_56', nk=2, valk=valk)
                                irteti = 3
                                goto 9999
                            else
                                irteti = 3
                                goto 9999
                            endif
                        endif
                    else
                        if (icomp .eq. 0 .or. icomp .eq. 1) then
                            call codent(iter, 'G', citer)
                            call codree(toler, 'E', ctol)
                            valk(1) = citer
                            valk(2) = ctol
                            call utmess('I', 'ALGORITH2_57', nk=2, valk=valk)
                            irteti = 3
                            goto 9999
                        else
                            irteti = 3
                            goto 9999
                        endif
                    endif
!
! -               ITER < 6 STOP
!
                else
                    if (icomp .eq. 0 .or. icomp .eq. 1) then
                        call codent(iter, 'G', citer)
                        call codree(toler, 'E', ctol)
                        valk(1) = citer
                        valk(2) = ctol
                        call utmess('I', 'ALGORITH2_58', nk=2, valk=valk)
                        irteti = 3
                        goto 9999
                    else
                        irteti = 3
                        goto 9999
                    endif
                endif
!
! -           CONVERGENCE A ITMAX
!
            else
                itsup = 0
                irteti = 0
                goto 9999
            endif
        endif
    endif
!
9999  continue
end subroutine
