subroutine lissav(lischa, ichar, charge, typech, codcha,&
                  prefob, typapp, nomfct, typfct, phase,&
                  npuis)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=19) :: lischa
    character(len=13) :: prefob
    integer :: ichar
    character(len=8) :: charge
    character(len=16) :: typapp, typfct
    integer :: codcha
    character(len=8) :: typech, nomfct
    real(kind=8) :: phase
    integer :: npuis
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! SAUVEGARDE DANS LA SD LISTE_CHARGES
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : NOM DE LA SD LISTE_CHARGES
! IN  ICHAR  : INDICE DE LA CHARGE
! IN  CHARGE : NOM DE LA CHARGE (AFFE_CHAR_*)
! IN  TYPECH : TYPE DE LA CHARGE
!               'REEL'    - CHARGE CONSTANTE REELLE
!               'COMP'    - CHARGE CONSTANTE COMPLEXE
!               'FONC_F0' - CHARGE FONCTION QUELCONQUE
!               'FONC_FT' - CHARGE FONCTION DU TEMPS
! IN  CODCHA : CODE (ENTIER CODE) CONTENANT LES GENRES
! IN  PREFOB : PREFIXE DE L'OBJET DE LA CHARGE
! IN  TYPAPP : TYPE D'APPLICATION DE LA CHARGE
!              FIXE_CSTE
!              FIXE_PILO
!              SUIV
!              DIDI
! IN  NOMFCT : NOM DE LA FONCTION MULTIPLICATRICE
! IN  TYPFCT : TYPE DE LA FONCTION MULTIPLICATRICE
!              'FONCT_REEL' FONCTION MULTIPLICATRICE REELLE
!              'FONCT_COMP' FONCTION MULTIPLICATRICE COMPLEXE
!              'CONST_REEL' FONCTION MULTIPLICATRICE CONSTANTE REELLE
!              'CONST_COMP' FONCTION MULTIPLICATRICE CONSTANTE COMPLEXE
! IN  PHASE  : PHASE POUR LES FONCTIONS MULTIPLICATRICES COMPLEXES
! IN  NPUIS  : PUISSANCE POUR LES FONCTIONS MULTIPLICATRICES COMPLEXES
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
    call jeveuo(nomcha, 'E', jncha)
    call jeveuo(codech, 'E', jcodc)
    call jeveuo(typcha, 'E', jtypc)
    call jeveuo(typeap, 'E', jtypa)
    call jeveuo(precha, 'E', jprec)
    call jeveuo(nomfon, 'E', jnfon)
    call jeveuo(typfon, 'E', jtfon)
    call jeveuo(valfon, 'E', jvfon)
!
    zk8 (jncha-1+ichar) = charge
    zi  (jcodc-1+ichar) = codcha
    zk8 (jtypc-1+ichar) = typech
    zk16(jtypa-1+ichar) = typapp
    zk24(jprec-1+ichar) = prefob
    zk8 (jnfon-1+ichar) = nomfct
    zk16(jtfon-1+ichar) = typfct
    zr  (jvfon-1+2*(ichar-1)+1) = phase
    zr  (jvfon-1+2*(ichar-1)+2) = npuis
!
    call jedema()
end subroutine
