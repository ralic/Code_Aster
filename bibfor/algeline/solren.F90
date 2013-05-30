subroutine solren(sn, nbmat, mater, q, codret)
!
    implicit   none
    include 'asterfort/calcq.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: nbmat, codret
    real(kind=8) :: sn(6), mater(nbmat, 2), q(6)
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --- BUT : CALCUL DE Q ------------------------------------------------
! ======================================================================
! IN  : NDT    : NOMBRE DE COMPOSANTES TOTAL DU TENSEUR ----------------
! --- : SN     : TENSEUR DU DEVIATEUR DES CONTRAINTES ------------------
! --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
! --- : MATER  : PARAMETRES MATERIAU -----------------------------------
! OUT : Q      : DERIVEE Q = DG/DSIG -----------------------------------
! ======================================================================
    real(kind=8) :: gamcjs, pref, epssig
! ======================================================================
! --- INITIALISATION DE PARAMETRE --------------------------------------
! ======================================================================
    parameter       ( epssig  =  1.0d-8  )
! ======================================================================
    call jemarq()
! ======================================================================
! --- RECUPERATION DE PARAMETRES MATERIAU ------------------------------
! ======================================================================
    gamcjs = mater(12,2)
    pref = mater(15,2)
! ======================================================================
! --- CALCUL DE Q ------------------------------------------------------
! ======================================================================
    call calcq(sn, gamcjs, pref, epssig, q,&
               codret)
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
