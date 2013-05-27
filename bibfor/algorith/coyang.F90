function coyang(dist, dteta, rayon, omega, uc,&
                uct, l, lt)
!------------------------------------------------------------------
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
!-------------------------------------------------------------------
! CALCUL DE LA FONCTION DE COHERENCE SUIVANT AU-YANG
    implicit none
!
! *****************   DECLARATIONS DES VARIABLES   ********************
!
!
! ARGUMENTS
! ---------
    include 'jeveux.h'
    real(kind=8) :: dist, dteta, rayon, omega, uc, uct, l, lt
!
!
! VARIABLES LOCALES
! -----------------
    real(kind=8) :: coyang, codist, coteta
!
! CALCUL DE LA FONCTION DE COHERENCE EN X
!
    codist=exp(-dist/l)*cos(omega*dist/uc)
!
! CALCUL DE LA FONCTION DE COHERENCE EN TETA
!
    coteta=exp(-rayon*dteta/lt)*cos(rayon*omega*dteta/uct)*rayon*rayon
!
! CALCUL DE LA FONCTION DE COHERENCE SUIVANT AU-YANG
!
    coyang = codist * coteta
!
end function
