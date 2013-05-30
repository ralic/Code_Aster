subroutine impft0(isor, ift, ibl, fmoy, fetyp,&
                  frms, fmax, fmin)
!***********************************************************************
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
!     IMPRESSION DES FORCES TANGENTIELLES AMV
!
!
!
    implicit none
    integer :: isor, ift
    real(kind=8) :: fmoy, frms, fetyp, fmax, fmin
!
!
!
!-----------------------------------------------------------------------
    integer :: ibl
!-----------------------------------------------------------------------
    if (ift .eq. 1) then
        if (ibl .eq. 1) then
            write(isor,*)
            write(isor,*)' ***** STATISTIQUES FORCE TANGENTE 1 *****'
            write(isor,*)'+--+-------------+-------------+-------------+'//&
     &               '-------------+-------------+'
            write(isor,*)'!IB! FT1 MOY     ! FT1 E.TYPE  ! FT1 RMS     !',&
     &               ' FT1 MIN     ! FT1 MAX     !'
            write(isor,*)'+--+-------------+-------------+-------------+'//&
     &               '-------------+-------------+'
        else if (ibl.eq.0) then
            write(isor,*)
            write(isor,*)' ***** STATISTIQUES GLOBALES FTANG1 *****'
            write(isor,*)'+--+-------------+-------------+-------------+'//&
     &               '-------------+-------------+'
            write(isor,*)'!IB! FT1 MOY     ! FT1 E.TYPE  ! FT1 RMS     !',&
     &               ' FT1 MIN     ! FT1 MAX     !'
            write(isor,*)'+--+-------------+-------------+-------------+'//&
     &               '-------------+-------------+'
        endif
        write(isor,10) ibl,fmoy,fetyp,frms,fmin,fmax
    else if (ift.eq.2) then
        if (ibl .eq. 1) then
            write(isor,*)
            write(isor,*)' ***** STATISTIQUES FORCE TANGENTE 2 *****'
            write(isor,*)'+--+-------------+-------------+-------------+'//&
     &               '-------------+-------------+'
            write(isor,*)'!IB! FT2 MOY     ! FT2 E.TYPE  ! FT2 RMS     !',&
     &               ' FT2 MIN     ! FT2 MAX     !'
            write(isor,*)'+--+-------------+-------------+-------------+'//&
     &               '-------------+-------------+'
        else if (ibl.eq.0) then
            write(isor,*)
            write(isor,*)' ***** STATISTIQUES GLOBALES FTANG2 *****'
            write(isor,*)'+--+-------------+-------------+-------------+'//&
     &               '-------------+-------------+'
            write(isor,*)'!IB! FT2 MOY     ! FT2 E.TYPE  ! FT2 RMS     !',&
     &               ' FT2 MIN     ! FT2 MAX     !'
            write(isor,*)'+--+-------------+-------------+-------------+'//&
     &               '-------------+-------------+'
        endif
        write(isor,10) ibl,fmoy,fetyp,frms,fmin,fmax
    endif
!
    10 format(' !',i2,'!',1pe12.5,' !',1pe12.5,' !',1pe12.5,' !',&
     &        1pe12.5,' !',1pe12.5 ,' !')
!
end subroutine
