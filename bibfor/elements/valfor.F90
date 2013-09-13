subroutine valfor(indn, lt1, lt2, l1, l2,&
                  l3)
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
!
! ......................................................................
!     FONCTION :  CALCUL DES POINTEURS DES FONCTIONS DE FORMES
!
!                 LT1 LT2   : TRANSLATION
!                 L1 L2 L3  : ROTATION
!
!                 SELON INTEGRATION
!
! ......................................................................
!
!
!
    implicit none
#include "asterfort/utmess.h"
!
    integer :: indn
!
    integer :: lt1, lt2
!
    integer :: l1, l2, l3
!
!DEB
!
!
!---- LES ADRESSES DES FONCTIONS DE FORME ET DE LEURS DERIVEES
!     SELON INDN ( VOIR ROUTINE BTDFN )
!
!
!
!------- NOEUDS DE SERENDIP POUR LA TRANSLATION
!
!            D N ( 1 ) D QSI 1      LT1 POUR  I1
!            D N ( 1 ) D QSI 2      LT2 POUR  I2
!
!------- NOEUDS DE LAGRANGE POUR LA ROTATION
!
!              N ( 2 )              L1  POUR  I3
!            D N ( 2 ) D QSI 1      L2  POUR  I4
!            D N ( 2 ) D QSI 2      L3  POUR  I5
!
!
    if (indn .eq. 1) then
!
!------- INDN =  1 INTEGRATION NORMALE
!
!        VOIR ROUTINE INI080 ET VECTGT
!
        lt1 = 207
        lt2 = 279
!
!        VOIR ROUTINE BTDFN
!
        l1 = 459
        l2 = 540
        l3 = 621
!
    else if (indn .eq. 0) then
!
!------- INDN =  0 INTEGRATION REDUITE
!
!        VOIR ROUTINE BTDMSR
!
        lt1 = 44
        lt2 = 76
!
        l1 = 351
        l2 = 387
        l3 = 423
!
    else
!
        call utmess('F', 'ELEMENTS4_57')
!
    endif
!
!FIN
!
end subroutine
