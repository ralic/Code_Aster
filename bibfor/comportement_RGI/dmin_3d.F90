subroutine dmin_3d(d03, df3)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!     verif explicite de la condition de croissance de micro defaut
!     variables externes
!=====================================================================
    implicit none
    real(kind=8) :: d03(3), df3(3)
!     variables locales
    integer :: imini0, imaxi0, imoy0, iminif, imaxif, imoyf
!
!     recherche des indices croissants ds d0
!     recherche de lindice de dmin
    imini0=1
    if (d03(2) .lt. d03(imini0)) imini0=2
    if (d03(3) .lt. d03(imini0)) imini0=3
!     recherche de l indice de dmax
    imaxi0=1
    if (d03(2) .ge. d03(imaxi0)) imaxi0=2
    if (d03(3) .ge. d03(imaxi0)) imaxi0=3
    imoy0=6-(imini0+imaxi0)
!      print*,imini0,imoy0,imaxi0,D03(imini0),d03(imoy0),d03(imaxi0)
!     recherche des indices croissants ds df
    iminif=1
    if (df3(2) .lt. df3(iminif)) iminif=2
    if (df3(3) .lt. df3(iminif)) iminif=3
    imaxif=1
    if (df3(2) .ge. df3(imaxif)) imaxif=2
    if (df3(3) .ge. df3(imaxif)) imaxif=3
    imoyf=6-(iminif+imaxif)
!      print*,iminif,imoyf,imaxif,df3(iminif),df3(imoyf),df3(imaxif)
!      print*
!
!     verif des conditions de croissance des valeurs principales
    if (df3(iminif) .lt. d03(imini0)) then
!       print*,'dD>0 actif ds b3d_util dmin_3D pour dmin'
!       print*,df3(iminif),d03(imini0),df3(iminif)-d03(imini0)
!       print*,imini0,imoy0,imaxi0,d03(imini0),d03(imoy0),d03(imaxi0)
!       print*,iminif,imoyf,imaxif,Df3(iminif),df3(imoyf),df3(imaxif)
!       read*
        df3(iminif)=d03(imini0)
    end if
    if (df3(imoyf) .lt. d03(imoy0)) then
!       print*,'dD>0 actif ds b3d_util dmin_3D pour dmoy'
!       print*,df3(imoyf),d03(imoy0),df3(imoyf)-d03(imoy0)
!       print*,imini0,imoy0,imaxi0,d03(imini0),d03(imoy0),d03(imaxi0)
!       print*,iminif,imoyf,imaxif,Df3(iminif),df3(imoyf),df3(imaxif)
!       read*
        df3(imoyf)=d03(imoy0)
    end if
    if (df3(imaxif) .lt. d03(imaxi0)) then
!       print*,'dD>0 actif ds b3d_util dmin_3D pour dmax'
!       print*,df3(imaxif),d03(imaxi0),df3(imaxif)-d03(imaxi0)
!       print*,imini0,imoy0,imaxi0,d03(imini0),d03(imoy0),d03(imaxi0)
!       print*,iminif,imoyf,imaxif,Df3(iminif),df3(imoyf),df3(imaxif)
!       read*
        df3(imaxif)=d03(imaxi0)
    end if
!      print*,df3(1),df3(2),df3(3)
!      read*
end subroutine
