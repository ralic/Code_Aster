subroutine impc0(isor, ibl, nbc, tcm, tcmax,&
                 tcmin, nrebo, trebm, tct, t,&
                 nbpt)
!***********************************************************************
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
!     IMPRESSION DES CHOCS
!
!
    implicit none
    integer :: isor, nbc, nrebo, nbpt
    real(kind=8) :: tcm, tcmax, tcmin, trebm, tct
    real(kind=8) :: t(*), dt, tacqui
!
!-----------------------------------------------------------------------
    integer :: ibl, nrepc
!-----------------------------------------------------------------------
    dt=t(2)-t(1)
    tacqui = t(nbpt) - t(1)
    if (nbc .ne. 0) then
        nrepc=nrebo/nbc
    else
        nrepc=0
    endif
    if (ibl .eq. 1) then
        write(isor,*)' '
        write(isor,*)' ***** STATISTIQUES DES CHOCS    ***** '
!
        write(isor,*) '------------------------------'
        write(isor,*) '! PAS ACQUIS  ! DUREE ACQUIS !'
        write(isor,9) dt,tacqui
        write(isor,*) '------------------------------'
        write(isor,*) '-----------------------------------------'//&
        '--------------------------------------------'
        write(isor,*) '!IB! CHOC/S ! REB/CH ! TCHOC MOYEN !'//&
     &         ' TCHOC MAX   ! TCHOC MIN   ! T REBOND MOY!%T. CHOC!'
        write(isor,*) '-----------------------------------------'//&
        '--------------------------------------------'
    else if (ibl.eq.0) then
        write(isor,*)' '
        write(isor,*)' ***** STATISTIQUES GLOBALES DES CHOCS    ***** '
!
        write(isor,*) '------------------------------'
        write(isor,*) '! PAS ACQUIS  ! DUREE ACQUIS !'
        write(isor,9) dt,tacqui
        write(isor,*) '------------------------------'
        write(isor,*) '-------------------------------'//&
     & '------------------------------------------------------'
        write(isor,*) '!IB! CHOC/S ! REB/CH ! TCHOC MOYEN !'//&
     &         ' TCHOC MAX   ! TCHOC MIN   ! T REBOND MOY!%TEMPS CHOC!'
        write(isor,*) '-----------------------------------------'//&
        '--------------------------------------------'
    endif
    write(isor,8) ibl,int(nbc/tacqui),nrepc,tcm,tcmax,tcmin,trebm,&
     &                  (100.d0*tct/tacqui)
!
    8 format(' !',i2,'!',i5,'   !',i5,'   !',1pd12.5,' !',&
     &          1pd12.5,' !',1pd12.5,' !',1pd12.5,' !',1pd12.5,' %!')
    9 format(' !',1pd12.5,' !',1pd12.5,' !')
!
end subroutine
