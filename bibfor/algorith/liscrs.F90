subroutine liscrs(lischa, nbchar, base)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit      none
#include "asterfort/wkvect.h"
    character(len=19) :: lischa
    integer :: nbchar
    character(len=1) :: base
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! CREATION DE LA SD LISTE_CHARGES
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : NOM DE LA SD LISTE_CHARGES
! IN  NBCHAR : NOMBRE DE CHARGEMENTS DANS LA LISTE
! IN  BASE   : BASE POUR CREER LA SD
!
! ----------------------------------------------------------------------
!
    character(len=24) :: nomcha, genrch, typcha, typeap, precha, mocfch
    integer :: jncha, jgenc, jtypc, jtypa, jprec, jmcfc
    character(len=24) :: nomfon, typfon, valfon
    integer :: jnfon, jtfon, jvfon
!
! ----------------------------------------------------------------------
!
    nomcha = lischa(1:19)//'.NCHA'
    genrch = lischa(1:19)//'.GENC'
    mocfch = lischa(1:19)//'.MCFC'
    typcha = lischa(1:19)//'.TYPC'
    typeap = lischa(1:19)//'.TYPA'
    precha = lischa(1:19)//'.PREO'
    nomfon = lischa(1:19)//'.NFON'
    typfon = lischa(1:19)//'.TFON'
    valfon = lischa(1:19)//'.VFON'
!
    call wkvect(nomcha, base//' V K8', nbchar, jncha)
    call wkvect(genrch, base//' V I', nbchar, jgenc)
    call wkvect(mocfch, base//' V I', 2*nbchar, jmcfc)
    call wkvect(typcha, base//' V K8', nbchar, jtypc)
    call wkvect(typeap, base//' V K16', nbchar, jtypa)
    call wkvect(precha, base//' V K24', nbchar, jprec)
    call wkvect(nomfon, base//' V K8', nbchar, jnfon)
    call wkvect(typfon, base//' V K16', nbchar, jtfon)
    call wkvect(valfon, base//' V R', 2*nbchar, jvfon)
!
end subroutine
