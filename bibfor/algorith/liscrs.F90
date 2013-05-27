subroutine liscrs(lischa, nbchar, base)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
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
!
!
!
    character(len=24) :: nomcha, codech, typcha, typeap, precha
    integer :: jncha, jcodc, jtypc, jtypa, jprec
    character(len=24) :: nomfon, typfon, valfon
    integer :: jnfon, jtfon, jvfon
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nomcha = lischa(1:19)//'.NCHA'
    codech = lischa(1:19)//'.CODC'
    typcha = lischa(1:19)//'.TYPC'
    typeap = lischa(1:19)//'.TYPA'
    precha = lischa(1:19)//'.PREO'
    nomfon = lischa(1:19)//'.NFON'
    typfon = lischa(1:19)//'.TFON'
    valfon = lischa(1:19)//'.VFON'
!
    call wkvect(nomcha, base//' V K8', nbchar, jncha)
    call wkvect(codech, base//' V I', nbchar, jcodc)
    call wkvect(typcha, base//' V K8', nbchar, jtypc)
    call wkvect(typeap, base//' V K16', nbchar, jtypa)
    call wkvect(precha, base//' V K24', nbchar, jprec)
    call wkvect(nomfon, base//' V K8', nbchar, jnfon)
    call wkvect(typfon, base//' V K16', nbchar, jtfon)
    call wkvect(valfon, base//' V R', 2*nbchar, jvfon)
!
    call jedema()
end subroutine
