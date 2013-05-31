subroutine vp1pro(optiom, lraide, lmasse, ldynam, neq,&
                  nfreq, nfreqb, tolv, nitv, iexcl,&
                  fcorig, vec, resufi, resufr, resufk,&
                  nbrssa, nbpari, nbparr, nbpark, typres,&
                  optiof, solveu)
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
!     CALCUL DES VECTEURS ET VALEURS PROPRES PAR LA METHODE D'ITERATION
!     INVERSE.
!     ------------------------------------------------------------------
! IN  VALP : R8 : TABLEAU DES VALEURS PROPRES INITIALES
! IN  SOLVEU : K19 : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
! ----------------------------------------------------------------------
!
! aslint: disable=W1504
    implicit none
!
! PARAMETRES D'APPEL
    include 'jeveux.h'
    include 'asterfort/freqom.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/omega2.h'
    include 'asterfort/rectfr.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vp1ite.h'
    include 'asterfort/vpstur.h'
    include 'asterfort/wkvect.h'
    integer :: nbpari, nbparr, nbpark, nitv, iexcl(*), nbrssa, lraide, lmasse
    integer :: ldynam, neq, nfreq, nfreqb, resufi(nfreqb, nbpari)
    real(kind=8) :: tolv, vec(neq, *), resufr(nfreqb, nbparr), fcorig
    character(len=*) :: optiom, resufk(nfreqb, nbpark)
    character(len=16) :: typres, optiof
    character(len=19) :: solveu
!
!
! VARIABLES LOCALES
    integer :: idet, place, irperm, lmx, lx0, iquoti, iprec, ifreq, ier, imode
    integer :: jfreq, iter, nbessa, i, iperm, j, kl, naux
    real(kind=8) :: det, err, eps, valeur, rperm
    character(len=24) :: kperm, cmulti, cvect0
!     ------------------------------------------------------------------
!
    call jemarq()
    cmulti = '&&VP1PRO.MODES.MULTIPLES'
    cvect0 = '&&VP1PRO.VECTEUR.TAMPON '
!
!     --- SAUVEGARDE DES TERMES MX POUR ORTHOGONALISER MODES MULTIPLES -
    call wkvect(cmulti, 'V V R', neq*nfreq, lmx)
    call wkvect(cvect0, 'V V R', neq, lx0)
!
    iquoti = 0
    if (optiom(1:8) .eq. 'RAYLEIGH') iquoti = 1
!     INITIALISATION A UNE VALEUR NON ATTEINTE
    iprec = -( nfreqb + 1 )
!
    do 10 ifreq = 1, nfreq
        valeur = resufr(ifreq,2)
20      continue
! --- POUR OPTIMISER ON NE CALCULE PAS LE DET
        call vpstur(lraide, valeur, lmasse, ldynam, det,&
                    idet, place, ier, solveu, .false.,&
                    .true.)
        if (ier .gt. 1) then
            valeur = 1.01d0 * valeur
            goto 20
        endif
        if (resufi(ifreq,1) .eq. 0) then
            iprec = -( nfreqb + 1 )
            imode = 1
            jfreq = ifreq
        else if (resufi(ifreq,1) .eq. iprec) then
            imode = imode + 1
        else
            iprec = resufi(ifreq,1)
            imode = 1
            jfreq = ifreq
        endif
        call vp1ite(lmasse, lraide, ldynam, vec(1, jfreq), imode,&
                    valeur, neq, nitv, tolv, iter,&
                    zr(lx0), zr(lmx), err, iexcl, place,&
                    iquoti, solveu)
        if (resufi(ifreq,1) .eq. 0) then
            resufi(ifreq,1) = place
        else if (imode .gt. 1) then
            place = resufi(ifreq,1) - imode + 1
            resufi(ifreq,1) = place
        endif
        resufr(ifreq,2) = valeur
        resufi(ifreq,4) = iter
        resufr(ifreq,15) = err
10  end do
!
! RECALCUL DU NUME_MODE POUR CHAQUE FREQUENCE
!
    if ((typres .eq. 'DYNAMIQUE') .and. (optiof.ne.'PROCHE')) then
!
        ifreq = 1
30      continue
        valeur = resufr(ifreq,2)
        if (abs(valeur) .le. omega2(fcorig)) then
            ifreq = ifreq + 1
            if (ifreq .gt. nfreq) then
                call u2mess('A', 'ALGELINE3_53')
            else
                goto 30
            endif
        endif
        nbessa = 0
        if (valeur .ge. 0.d0) then
            valeur = 0.95d0 * valeur
        else
            valeur = 1.05d0 * valeur
        endif
40      continue
! --- POUR OPTIMISER ON NE GARDE PAS LA FACTO (SI MUMPS) ET ON NE
! --- CALCULE PAS LE DET.
        call vpstur(lraide, valeur, lmasse, ldynam, det,&
                    idet, place, ier, solveu, .false.,&
                    .false.)
        if (ier .ne. 0) then
            nbessa = nbessa + 1
            if (nbessa .gt. nbrssa) then
                call u2mess('F', 'ALGELINE3_54')
            else
                if (valeur .ge. 0.d0) then
                    valeur = 0.95d0 * valeur
                else
                    valeur = 1.05d0 * valeur
                endif
                goto 40
            endif
        endif
        do 50 ifreq = 1, nfreq
            resufr(ifreq,2) = resufr(ifreq,2) - valeur
50      end do
!
    endif
!
! TRI DES VALEURS PROPRES SUIVANT ORDRE CROISSANT
! EN DYNAMIQUE: EN VALEUR ABSOLUE
! EN FLAMBEMENT: EN VALEUR ALGEBRIQUE
    eps = 1.d-7
    do 310 i = 1, nfreq
        iperm = i
        if (typres .eq. 'DYNAMIQUE') then
            rperm = abs(resufr(i,2))
        else
            rperm = resufr(i,2)
        endif
        do 312 j = i+1, nfreq
            if (typres .eq. 'DYNAMIQUE') then
                if (abs(resufr(j,2)) .lt. (rperm *(1.d0 -eps))) then
                    iperm = j
                    rperm = abs(resufr(iperm,2))
                endif
                if ((abs(resufr(j,2))-rperm) .le. (eps*rperm)) then
                    if (((resufr(j,2)*resufr(iperm,2)).ge. 0.d0) .and.&
                        (abs(resufr(j,2)) .lt. rperm )) then
                        iperm = j
                        rperm = abs(resufr(iperm,2))
                    endif
                    if (((resufr(j,2)*resufr(iperm,2)).lt. 0.d0) .and.&
                        ( resufr(j,2) .lt. 0.d0 )) then
                        iperm = j
                        rperm = abs(resufr(iperm,2))
                    endif
                endif
            else
                if (resufr(j,2) .lt. rperm) then
                    iperm = j
                    rperm = resufr(iperm,2)
                endif
            endif
312      continue
!
! PERMUTATION DES DONNEES LIEES AUX VALEURS PROPRES
        if (iperm .ne. i) then
            do 320 kl = 1, nbparr
                rperm = resufr(iperm,kl)
                resufr(iperm,kl) = resufr(i,kl)
                resufr(i,kl) = rperm
320          continue
            do 321 kl = 1, nbpari
                irperm = resufi(iperm,kl)
                resufi(iperm,kl) = resufi(i,kl)
                resufi(i,kl) = irperm
321          continue
            do 322 kl = 1, nbpark
                kperm = resufk(iperm,kl)
                resufk(iperm,kl) = resufk(i,kl)
                resufk(i,kl) = kperm
322          continue
            do 330 j = 1, neq
                rperm = vec(j,i)
                vec(j,i) = vec(j,iperm)
                vec(j,iperm) = rperm
330          continue
        endif
310  end do
!
    if ((typres.eq.'DYNAMIQUE') .and. (optiof.ne.'PROCHE')) then
!
        naux = 0
        call rectfr(nfreq, nfreq, valeur, place, naux,&
                    resufr(1, 2), nfreqb, resufi, resufr, nfreqb)
    else
        do 51 ifreq = 1, nfreq
            resufi(ifreq,1) = ifreq
51      continue
    endif
!
    do 52 ifreq = 1, nfreq
        resufr(ifreq,1) = freqom(resufr(ifreq,2))
        resufk(ifreq,2) = 'INVERSE_R'
52  end do
!
!     --- DESTRUCTION ZONE DE TRAVAIL ---
    call jedetr(cmulti)
    call jedetr(cvect0)
!
    call jedema()
end subroutine
