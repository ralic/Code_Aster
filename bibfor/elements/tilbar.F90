subroutine tilbar(stild, vectt, bars)
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
!
    implicit none
!
    include 'asterfort/btkb.h'
    include 'asterfort/sigbar.h'
    include 'asterfort/sigvte.h'
    real(kind=8) :: stild ( 5 )
    real(kind=8) :: vectt ( 3 , 3 )
    real(kind=8) :: bars ( 9 , 9 )
!
    real(kind=8) :: bid33 ( 3 , 3 )
!
    real(kind=8) :: stil ( 3 , 3 )
!
!
    real(kind=8) :: s ( 3 , 3 )
!
!DEB
!
!---- TENSEUR 3 * 3 CONTRAINTES LOCALES
!
    call sigvte(stild, stil)
!
!---- ROTATION DU TENSEUR CONTRAINTES : LOCALES --> GLOBALES
!
!              S     =  ( VECTT ) T * STIL  * VECTT
!
    call btkb(3, 3, 3, stil, vectt,&
              bid33, s)
!
!---- BARS   ( 9 , 9 )
!
    call sigbar(s, bars)
!
!
! FIN
!
end subroutine
