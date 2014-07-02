function verinr(nbval, tbins1, tbins2)
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    aster_logical :: verinr
    integer :: nbval
    character(len=19) :: tbins1, tbins2
! ======================================================================
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
!
!
! ======================================================================
! ======================================================================
! --- BUT : VERIFICATION QUE LES LISTES D'INSTANTS DES CHAMPS ----------
! ------- : MECANIQUES SONT IDENTIQUES ---------------------------------
! ======================================================================
! IN  : NBVAL  : DIMENSION DE LA LISTE D'INSTANT -----------------------
! --- : TBINS1 : TABLE 1 -----------------------------------------------
! --- : TBINS2 : TABLE 2 -----------------------------------------------
! OUT : VERINR : FALSE SI LES LISTES SONT IDENTIQUES -------------------
! ------------ : TRUE  SI LES LISTES SONT DIFFERENTES ------------------
! ======================================================================
! ======================================================================
    integer :: ii, jtbini, jtbin1, jtbin2
    real(kind=8) :: somme
    character(len=19) :: vecins
! ======================================================================
    vecins = '&&VERINR.VECINS'
    call wkvect(vecins, 'V V R', nbval, jtbini)
    call jeveuo(tbins1, 'L', jtbin1)
    call jeveuo(tbins2, 'L', jtbin2)
! ======================================================================
! --- CALCUL DE LA NORME -----------------------------------------------
! ======================================================================
    somme = 0.0d0
    verinr = .false.
    do 10 ii = 1, nbval
        zr(jtbini-1+ii) = zr(jtbin1-1+ii) - zr(jtbin2-1+ii)
        somme = somme + zr(jtbini-1+ii)
 10 end do
    if (somme .gt. 0.0d0) verinr = .true.
! ======================================================================
! --- DESTRUCTION DE VECTEURS INUTILES ---------------------------------
! ======================================================================
    call jedetr(vecins)
! ======================================================================
end function
