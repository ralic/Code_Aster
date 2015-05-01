subroutine defext(np4, nbm, npfts, ndef, tc,&
                  textts, fextts, fmod, indt, niter)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! DESCRIPTION : RECUPERATION DES EFFORTS EXTERIEURS GENERALISES
! -----------
!               APPELANT : CALFNL
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: np4, nbm, npfts, ndef
    real(kind=8) :: tc, textts(*), fextts(np4, *), fmod(*)
    integer :: indt, niter
!
! VARIABLES LOCALES
! -----------------
    integer :: i, indt1
    real(kind=8) :: t1, t2, f1, f2
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!-----------------------------------------------------------------------
! 1.  PREMIER INSTANT DE CALCUL
!-----------------------------------------------------------------------
!
    if (ndef .eq. 0) then
        indt = 1
        indt1 = 2
        ndef = 1
    endif
!
!-----------------------------------------------------------------------
! 2.  INSTANTS SUIVANTS
!-----------------------------------------------------------------------
!
! 2.1 SI PREMIERE ITERATION POUR L'INSTANT COURANT : ON AVANCE
! --- PAR RAPPORT A L'INSTANT PRECEDENT
!
    if (niter .eq. 0) then
        do 10 i = indt, npfts
            if (tc .gt. textts(i)) then
                indt = i
                indt1 = i + 1
            else if (tc.eq.textts(i)) then
                indt = i
                indt1 = i
            else
                goto 30
            endif
10      continue
!
! 2.2 SINON : ON A DECREMENTE LE PAS DE TEMPS, DONC ON RECULE PAR
! --- RAPPORT A L'ITERATION PRECEDENTE
!
    else
        if (tc .gt. textts(indt)) then
            indt1 = indt + 1
        else if (tc.eq.textts(indt)) then
            indt1 = indt
        else
            if (indt .gt. 2) then
                do 20 i = indt, 1, -1
                    if (tc .lt. textts(i)) then
                        indt = i - 1
                        indt1 = i
                    else if (tc.eq.textts(i)) then
                        indt = i
                        indt1 = i
                    else
                        goto 30
                    endif
20              continue
            else
                indt = 1
                indt1 = 2
            endif
        endif
    endif
!
!-----------------------------------------------------------------------
! 3.  INTERPOLATION LINEAIRE DES EFFORTS GENERALISES A L'INSTANT COURANT
!-----------------------------------------------------------------------
!
30  continue
    if (indt .ne. indt1) then
        t1 = textts(indt)
        t2 = textts(indt1)
        do 40 i = 1, nbm
            f1 = fextts(indt,i)
            f2 = fextts(indt1,i)
            fmod(i) = f1 + (tc - t1)*(f2 - f1)/(t2 - t1)
40      continue
    else
        do 50 i = 1, nbm
            fmod(i) = fextts(indt,i)
50      continue
    endif
!
! --- FIN DE DEFEXT.
end subroutine
