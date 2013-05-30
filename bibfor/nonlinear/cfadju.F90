subroutine cfadju(alias, ksi1, ksi2, toleou, iproj)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'asterfort/assert.h'
    character(len=8) :: alias
    real(kind=8) :: ksi1, ksi2, toleou
    integer :: iproj
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE- APPARIEMENT)
!
! AJUSTE LES COORDONNES PARAMETRIQUES POUR RESTER DANS LA MAILLE
!
! ----------------------------------------------------------------------
!
!
! IN  ALIAS  : TYPE DE L'ELEMENT
!               'SE2','SE3'
!               'TR3','TR6'
!               'QU4','QU8','QU9'
! I/O KSI1   : POINT DE CALCUL SUIVANT KSI1 DES
!               FONCTIONS DE FORME ET LEURS DERIVEES
! I/O KSI2   : POINT DE CALCUL SUIVANT KSI2 DES
!               FONCTIONS DE FORME ET LEURS DERIVEES
! IN  TOLEOU : TOLERANCE POUR PROJECTION HORS SEGMENT
! OUT IPROJ  : VAUT 0 SI POINT PROJETE DANS L'ELEMENT
!                   1 SI POINT PROJETE DANS LA ZONE DEFINIE PAR TOLEOU
!                   2 SI POINT PROJETE EN DEHORS (EXCLUS)
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: ecart, ksi1e, ksi2e
    integer :: izone
!
! ----------------------------------------------------------------------
!
    iproj = 0
    ecart = -1.d0
!
    if (alias(1:2) .eq. 'SE') then
!
        if ((ksi1.ge.-1.d0) .and. (ksi1.le.1.d0)) then
            goto 999
        endif
!
! --- CALCUL DE L'ECART
!
        ecart = abs(ksi1)-1.d0
!
! --- RABATTEMENT
!
        iproj = 1
!
        if (ksi1 .lt. -1.d0) then
            ksi1 = -1.d0
        else if (ksi1.gt.1.d0) then
            ksi1 = 1.d0
        endif
!
        if (ecart .gt. toleou) then
            iproj = 2
        endif
!
    else if (alias(1:2).eq.'TR') then
!
        if ((ksi1.ge.0.d0) .and. (ksi2.ge.0.d0) .and. ((ksi1+ksi2) .le.1.d0)) then
            goto 999
        endif
!
! --- SECTEUR CONCERNE
!
        izone = 0
        if (ksi1 .lt. 0.d0) then
            if (ksi2 .lt. 0.d0) then
                izone = 1
            else if ((ksi2.ge.0.d0).and.(ksi2.le.1.d0)) then
                izone = 2
            else if (ksi2.gt.1.d0) then
                izone = 3
            else
                call assert(.false.)
            endif
        endif
        if (ksi2 .lt. 0.d0) then
            if (ksi1 .lt. 0.d0) then
                izone = 1
            else if ((ksi1.ge.0.d0).and.(ksi1.le.1.d0)) then
                izone = 8
            else if (ksi1.gt.1.d0) then
                izone = 7
            else
                call assert(.false.)
            endif
        endif
        if (ksi1 .ge. 0.d0) then
            if (ksi2 .gt. (ksi1+1.d0)) then
                izone = 4
                elseif ((ksi2.gt.(-ksi1+1.d0)).and. (ksi2.ge.(ksi1-1.d0))&
            .and. (ksi2.le.(ksi1+1.d0))) then
                izone = 5
                ksi1e = 5.d-1*(1.d0+ksi1-ksi2)
                ksi2e = 5.d-1*(1.d0-ksi1+ksi2)
            else if ((ksi2.ge.0.d0).and. (ksi2.lt.(ksi1-1.d0))) then
                izone = 6
            endif
        endif
!
! --- CALCUL DE L'ECART
!
        if (izone .eq. 1) then
            ecart = sqrt(abs(ksi1)*abs(ksi1)+ abs(ksi2)*abs(ksi2))
        else if (izone.eq.2) then
            ecart = sqrt(abs(ksi1)*abs(ksi1))
        else if (izone.eq.3.or.izone.eq.4) then
            ecart = sqrt(abs(ksi1)*abs(ksi1)+ (ksi2-1.d0)*(ksi2-1.d0))
        else if (izone.eq.5) then
            ecart = sqrt((ksi1-ksi1e)*(ksi1-ksi1e)+ (ksi2-ksi2e)*( ksi2-ksi2e))
        else if (izone.eq.6.or.izone.eq.7) then
            ecart = sqrt(abs(ksi2)*abs(ksi2)+ (ksi1-1.d0)*(ksi1-1.d0))
        else if (izone.eq.8) then
            ecart = sqrt(abs(ksi2)*abs(ksi2))
        else
            call assert(.false.)
        endif
!
! --- RABATTEMENT
!
        iproj = 1
!
        if (izone .eq. 1) then
            ksi1 = 0.d0
            ksi2 = 0.d0
        else if (izone.eq.2) then
            ksi1 = 0.d0
        else if (izone.eq.3.or.izone.eq.4) then
            ksi1 = 0.d0
            ksi2 = 1.d0
        else if (izone.eq.5) then
            ksi1 = ksi1e
            ksi2 = ksi2e
        else if (izone.eq.6.or.izone.eq.7) then
            ksi1 = 1.d0
            ksi2 = 0.d0
        else if (izone.eq.8) then
            ksi2 = 0.d0
        endif
!
        if (ecart .gt. toleou) then
            iproj = 2
        endif
!
    else if (alias(1:2).eq.'QU') then
!
        if ((abs(ksi1).le.1.d0) .and. (abs(ksi2).le.1.d0)) then
            goto 999
        endif
!
! --- SECTEUR CONCERNE
!
        izone = 0
        if (ksi1 .lt. -1.d0) then
            if (ksi2 .lt. -1.d0) then
                izone = 1
!
            else if ((ksi2.ge.-1.d0).and.(ksi2.le.1.d0)) then
                izone = 2
            else if (ksi2.gt.1.d0) then
                izone = 3
            else
                call assert(.false.)
            endif
        endif
        if (ksi1 .gt. 1.d0) then
            if (ksi2 .lt. -1.d0) then
                izone = 7
            else if ((ksi2.ge.-1.d0).and.(ksi2.le.1.d0)) then
                izone = 6
            else if (ksi2.gt.1.d0) then
                izone = 5
            else
                call assert(.false.)
            endif
        endif
        if ((ksi1.ge.-1.d0) .and. (ksi1.le.1.d0)) then
            if (ksi2 .lt. -1.d0) then
                izone = 8
            else if (ksi2.gt.1.d0) then
                izone = 4
            endif
        endif
!
! --- CALCUL DE L'ECART
!
        if (izone .eq. 1) then
            ecart = sqrt(( abs(ksi1)-1.d0)*(abs(ksi1)-1.d0)+ (abs(ksi2) -1.d0)*(abs(ksi2)-1.d0 ))
        else if (izone.eq.2) then
            ecart = sqrt((abs(ksi1)-1.d0)*(abs(ksi1)-1.d0))
        else if (izone.eq.3) then
            ecart = sqrt(( abs(ksi1)-1.d0)*(abs(ksi1)-1.d0)+ (abs(ksi2) -1.d0)*(abs(ksi2)-1.d0 ))
        else if (izone.eq.4) then
            ecart = sqrt((abs(ksi2)-1.d0)*(abs(ksi2)-1.d0))
        else if (izone.eq.5) then
            ecart = sqrt(( abs(ksi1)-1.d0)*(abs(ksi1)-1.d0)+ (abs(ksi2) -1.d0)*(abs(ksi2)-1.d0 ))
        else if (izone.eq.6) then
            ecart = sqrt((abs(ksi1)-1.d0)*(abs(ksi1)-1.d0))
        else if (izone.eq.7) then
            ecart = sqrt(( abs(ksi1)-1.d0)*(abs(ksi1)-1.d0)+ (abs(ksi2) -1.d0)*(abs(ksi2)-1.d0 ))
        else if (izone.eq.8) then
            ecart = sqrt((abs(ksi2)-1.d0)*(abs(ksi2)-1.d0))
        else
            call assert(.false.)
        endif
!
! --- RABATTEMENT
!
        iproj = 1
!
        if (izone .eq. 1) then
            ksi1 = -1.d0
            ksi2 = -1.d0
        else if (izone.eq.2) then
            ksi1 = -1.d0
        else if (izone.eq.3) then
            ksi1 = -1.d0
            ksi2 = 1.d0
        else if (izone.eq.4) then
            ksi2 = 1.d0
        else if (izone.eq.5) then
            ksi1 = 1.d0
            ksi2 = 1.d0
        else if (izone.eq.6) then
            ksi1 = 1.d0
        else if (izone.eq.7) then
            ksi1 = 1.d0
            ksi2 = -1.d0
        else if (izone.eq.8) then
            ksi2 = -1.d0
        endif
!
        if (ecart .gt. toleou) then
            iproj = 2
        endif
!
    else
        call assert(.false.)
    endif
!
999  continue
!
end subroutine
