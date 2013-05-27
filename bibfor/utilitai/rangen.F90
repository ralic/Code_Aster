subroutine rangen(prgene, isst, inumod, irang)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    character(len=19) :: prgene
    integer :: isst, inumod, irang
!     ------------------------------------------------------------
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
!     ------------------------------------------------------------
!     DONNE LE NUMERO DU NOEUD
!           NUNOE = 0 SI LE NOEUD N'EXISTE PAS
!     DONNE LE NUMERO DU DDL ASSOCIE AU NOEUD ET A SA COMPOSANTE
!           NUDDL = 0 SI LE COUPLE (NOEUD,COMPOSANTE) N'EXISTE PAS
!     ------------------------------------------------------------
! IN  NUMEGE    : K14: NOM D'UN NUME_DDL
! IN  ISST   : I  : NUMERO DE LA SOUS-STRUCTURE
! IN  INUMOD : I  : NUMERO DU MODE DYNAMIQUE OU CONTRAINT ASSOCIEE A
!                   LA SOUS-STRUCTURE
! OUT IRANG  : I  : POSITION DANS LE VECTEUR DES DDLS GENERALISES
!                   DU MODE
!     -------------------------------------------------------------
!     -----------------------------------------------------------------
!     FONCTION EXTERNE
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iaprno, ibid, iddl
!-----------------------------------------------------------------------
    call jemarq()
!
!
!     --- NUMERO DDL DU NOEUD NOEUD ET DE SA COMPOSANTE CMP
!
    call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), ibid)
    call jeveuo(jexnum(prgene//'.PRNO', ibid), 'L', iaprno)
    iddl = zi(iaprno+2*isst-2)
    if (iddl .eq. 0) goto 9999
    irang = inumod + iddl - 1
!
9999  continue
    call jedema()
end subroutine
