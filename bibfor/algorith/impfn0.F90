subroutine impfn0(isor, ibl, fnmoyt, fnmoyc, fnrmst,&
                  fnrmsc, fmax)
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
!     IMPRESSION DES FORCES NORMALES AMV
!
!
!
    implicit none
    integer :: isor
    real(kind=8) :: fnmoyt, fnmoyc, fnrmst, fnrmsc, fmax
!
!
!-----------------------------------------------------------------------
    integer :: ibl
!-----------------------------------------------------------------------
    if (ibl .eq. 1) then
        write(isor,*)
        write(isor,*) ' ***** STATISTIQUES FORCE NORMALE *****'
        write(isor,*)  '+--+-------------+-------------+-------------+',&
     &                '-------------+-------------+'
        write(isor,*)  '!IB! FN MOY TTOT ! FN MOY TCHOC! FN RMS TTOT !',&
     &                   ' FN RMS TCHOC! FN MAX      !'
        write(isor,*)  '+--+-------------+-------------+-------------+',&
     &                '-------------+-------------+'
    else if (ibl.eq.0) then
        write(isor,*)
        write(isor,*) ' ***** STATISTIQUES GLOBALES FNORM *****'
        write(isor,*)  '+--+-------------+-------------+-------------+',&
     &                '-------------+-------------+'
        write(isor,*)  '!IB! FN MOY TTOT ! FN MOY TCHOC! FN RMS TTOT !',&
     &                   ' FN RMS TCHOC! FN MAX      !'
        write(isor,*)  '+--+-------------+-------------+-------------+',&
     &                '-------------+-------------+'
    endif
    write(isor,10) ibl,fnmoyt,fnmoyc,fnrmst,fnrmsc,fmax
!
    10 format(' !',i2,'!',1pe12.5,' !',1pe12.5,' !',1pe12.5,' !',&
     &        1pe12.5,' !',1pe12.5 ,' !')
end subroutine
