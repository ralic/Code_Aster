subroutine calamr(phib24, phi1j, bi, num, j,&
                  cij2)
!---------------------------------------------------------------------
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
!---------------------------------------------------------------------
    implicit none
!
!ROUTINE STOCKANT LE VECTEUR PRESSION ISSUE D UNE RESOLUTION DE LAPLACE
! IN : VECSOL : VECTEUR SOLUTION K*
! OUT : VESTO : VECTEUR STOCKE K*
!---------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/calci.h"
#include "asterfort/chnucn.h"
#include "asterfort/codent.h"
    character(len=3) :: incr
    character(len=8) :: k8bid
    character(len=14) :: num
    character(len=19) :: phi1j, ph1plo, phib19
    character(len=24) :: phib24, bi
    real(kind=8) :: cij2
! ---------------------------------------------------------------
!
!--------- CALCUL DE LA MASSE AJOUTEE POUR UN FLUIDE-------------
!-------------------N AYANT PAS FORCEMENT------------------------
!-----------------LA MEME DENSITE PARTOUT------------------------
!
!-----------PLONGEMENT DE LA PRESSION----
!---------------SUR LE MODELE THERMIQUE D INTERFACE--------------
!-----------------------------------------------------------------------
    integer :: j
!-----------------------------------------------------------------------
    phib19='PHIB19'
    incr='BID'
    call chnucn(phib24(1:19), num, 0, k8bid, 'V',&
                phib19)
    call codent(j, 'D0', incr)
    ph1plo='PHPLO'//incr
    call chnucn(phi1j, num, 0, k8bid, 'V',&
                ph1plo)
!-------------------CALCUL DE L'AMORTISSEMENT AJOUTE-------------------
!---------------SUR LE MODELE THERMIQUE D INTERFACE--------------
!
    call calci(phib19, ph1plo, bi(1:19), cij2)
!       CALL JEDETC('V',PH1PLO,1)
!       CALL JEDETC('V',BI(1:19),1)
!
end subroutine
