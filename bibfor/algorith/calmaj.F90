subroutine calmaj(option, max, may, maz, model,&
                  vesto, modmec, chamno, num, vrai,&
                  i, j, mij)
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
#include "asterfort/calcin.h"
#include "asterfort/ploint.h"
    logical :: vrai
    integer :: i, j
    character(len=*) :: modmec, chamno, model
    character(len=9) :: option
    character(len=14) :: num
    character(len=19) :: modx, mody, modz, veprj, vesto, may, max, maz
    real(kind=8) :: mij
! ---------------------------------------------------------------
!
!--------- CALCUL DE LA MASSE AJOUTEE POUR UN FLUIDE-------------
!-------------------N AYANT PAS FORCEMENT------------------------
!-----------------LA MEME DENSITE PARTOUT------------------------
!
!-----------PLONGEMENT DE LA PRESSION ET DES CHAMPS DE DEPL_R----
!---------------SUR LE MODELE THERMIQUE D INTERFACE--------------
!
    call ploint(vesto, modmec, chamno, num, i,&
                vrai, model, veprj, modx, mody,&
                modz)
!
!-------------------CALCUL DE LA MASSE AJOUTEE-------------------
!---------------SUR LE MODELE THERMIQUE D INTERFACE--------------
!
    call calcin(option, max, may, maz, model,&
                veprj, modx, mody, modz, i,&
                j, mij)
!
!
end subroutine
