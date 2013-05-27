subroutine impdep(isor, idep, ibl, dmoy, detyp,&
                  drms, dmax, dmin)
!***********************************************************************
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
!     IMPRESSION DES DEPLACEMENTS
!
    implicit none
    integer :: isor, idep
    real(kind=8) :: dmoy, detyp, drms, dmax, dmin
!
!
!-----------------------------------------------------------------------
    integer :: ibl
!-----------------------------------------------------------------------
    if (idep .eq. 1) then
        if (ibl .eq. 1) then
            write(isor,*)
            write(isor,*) ' ***** STATISTIQUES DEPLACEMENTS X LOCAL *****'
            write(isor,*)
            write(isor,*) '---------------------------------------------',&
     &                '----------------------------'
            write(isor,*)  '!IB! DX MOYEN    ! DX E.TYPE   !',&
     &                ' DX RMS      ! DX MAX      ! DX MIN      !'
            write(isor,*) '---------------------------------------------',&
     &                '----------------------------'
        else if (ibl.eq.0) then
            write(isor,*)
            write(isor,*) ' ***** STATISTIQUES GLOBALES  DEPX *****'
            write(isor,*)
            write(isor,*) '---------------------------------------------',&
     &                '----------------------------'
            write(isor,*)'!IB! DX MOYEN    ! DX E.TYPE   ! DX RMS      !',&
     &                ' DX MAX      ! DX MIN      !'
            write(isor,*)'---------------------------------------------',&
     &                '----------------------------'
        endif
        write(isor,10) ibl,dmoy,detyp,drms,dmax,dmin
    else if (idep.eq.2) then
        if (ibl .eq. 1) then
            write(isor,*)
            write(isor,*)' ***** STATISTIQUES DEPLACEMENTS Y LOCAL *****'
            write(isor,*)
            write(isor,*)'---------------------------------------------',&
     &                '----------------------------'
            write(isor,*)'!IB! DY MOYEN    ! DY E.TYPE   ! DY RMS      !',&
     &                ' DY MAX      ! DY MIN      !'
            write(isor,*)'---------------------------------------------',&
     &                '----------------------------'
        else if (ibl.eq.0) then
            write(isor,*)
            write(isor,*) ' ***** STATISTIQUES GLOBALES  DEPY *****'
            write(isor,*)
            write(isor,*)'---------------------------------------------',&
     &                '----------------------------'
            write(isor,*)'!IB! DY MOYEN    ! DY E.TYPE   ! DY RMS      !',&
     &                ' DY MAX      ! DY MIN      !'
            write(isor,*)'---------------------------------------------',&
     &                '----------------------------'
        endif
        write(isor,10) ibl,dmoy,detyp,drms,dmax,dmin
    else if (idep.eq.3) then
        if (ibl .eq. 1) then
            write(isor,*)
            write(isor,*)' ***** STATISTIQUES DEPLACEMENTS Z LOCAL *****'
            write(isor,*)
            write(isor,*)'----------------------------------------------',&
     &                '----------------------------'
            write(isor,*)'!IB! DZ MOYEN    ! DZ E.TYPE   ! DZ RMS      !',&
     &                ' DZ MAX      ! DZ MIN      !'
            write(isor,*)'----------------------------------------------',&
     &                '----------------------------'
        else if (ibl.eq.0) then
            write(isor,*)
            write(isor,*) ' ***** STATISTIQUES GLOBALES  DEPZ *****'
            write(isor,*)
            write(isor,*)'----------------------------------------------',&
     &                '----------------------------'
            write(isor,*)'!IB! DZ MOYEN    ! DZ E.TYPE   ! DZ RMS      !',&
     &                ' DZ MAX      ! DZ MIN      !'
            write(isor,*)'----------------------------------------------',&
     &                '----------------------------'
        endif
        write(isor,10) ibl,dmoy,detyp,drms,dmax,dmin
    else if (idep.eq.4) then
        if (ibl .eq. 1) then
            write(isor,*)
            write(isor,*)' *****  STATISTIQUES DEPLACEMENT  RADIAL *****'
            write(isor,*)
            write(isor,*)'----------------------------------------------',&
     &                '----------------------------'
            write(isor,*)'!IB! R  MOYEN    ! R  E.TYPE   ! R  RMS      !',&
     &                ' R  MAX      ! R  MIN      !'
            write(isor,*)'----------------------------------------------',&
     &                '----------------------------'
        else if (ibl.eq.0) then
            write(isor,*)
            write(isor,*) ' ***** STATISTIQUES GLOBALES DEPL RADIAL ****'
            write(isor,*)
            write(isor,*)'----------------------------------------------',&
     &                '----------------------------'
            write(isor,*)'!IB! R  MOYEN    ! R  E.TYPE   ! R  RMS      !',&
     &                ' R  MAX      ! R  MIN      !'
            write(isor,*)'----------------------------------------------',&
     &                '----------------------------'
        endif
        write(isor,10) ibl,dmoy,detyp,drms,dmax,dmin
    else if (idep.eq.5) then
        if (ibl .eq. 1) then
            write(isor,*)
            write(isor,*)' ***** STATISTIQUES DEPLACEMENT ANGULAIRE ****'
            write(isor,*)
            write(isor,*)'----------------------------------------------',&
     &                '----------------------------'
            write(isor,*)'!IB! THETA MOYEN ! THETA E.TYP ! THETA RMS   !',&
     &                ' THETA MAX   ! THETA MIN   !'
            write(isor,*)'----------------------------------------------',&
     &                '----------------------------'
        else if (ibl.eq.0) then
            write(isor,*)
            write(isor,*) ' ***** STATISTIQUES GLOBALES DEPL ANGLE  ****'
            write(isor,*)
            write(isor,*)'----------------------------------------------',&
     &                '----------------------------'
            write(isor,*)'!IB! THETA MOYEN ! THETA E.TYP ! THETA RMS   !',&
     &                ' THETA MAX   ! THETA MIN   !'
            write(isor,*)'----------------------------------------------',&
     &                '----------------------------'
        endif
        write(isor,10) ibl,dmoy,detyp,drms,dmax,dmin
    endif
!
!
    10 format(' !',i2,'!',1pe12.5,' !',1pe12.5,' !',1pe12.5,' !',&
     &         1pe12.5,' !',1pe12.5,' !')
end subroutine
