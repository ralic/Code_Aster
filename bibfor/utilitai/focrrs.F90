subroutine focrrs(nomfon, resu, base, nomcha, maille,&
                  noeud, cmp, npoint, nusp, ivari,&
                  ier)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/focrr0.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rsutn2.h'
    integer :: npoint, nusp, ivari, ier
    character(len=1) :: base
    character(len=8) :: maille, noeud, cmp
    character(len=16) :: nomcha
    character(len=19) :: nomfon, resu
!     ------------------------------------------------------------------
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
!     RECUPERATION D'UNE FONCTION DANS UNE STRUCTURE "RESULTAT"
!     ------------------------------------------------------------------
! VAR : NOMFON : NOM DE LA FONCTION
! IN  : RESU   : NOM DE LA STRUCTURE RESULTAT
! IN  : BASE   : BASE OU L'ON CREE LA FONCTION
! IN  : NOMCHA : NOM DU CHAMP
! IN  : NOEUD  : NOEUD
! IN  : MAILLE : MAILE
! IN  : CMP    : COMPOSANTE
! IN  : NPOINT : NUMERO DU POINT ( CAS DES CHAM_ELEMS )
! IN  : NUSP   : NUMERO DU SOUS-POINT ( CAS DES CHAM_ELEMS )
! IN  : IVARI   : NUMERO DE LA CMP (POUR VARI_R)
! OUT : IER    : CODE RETOUR, = 0 : OK
!     ------------------------------------------------------------------
    integer :: nbordr, lordr
    character(len=8) :: interp
    character(len=19) :: knume
!     ------------------------------------------------------------------
!
    call jemarq()
    ier = 0
    knume = '&&FOCRRS.NUME_ORDR'
!
!     --- RECUPERATION DES NUME_ORDRE FOURNIS PAR L'UTILISATEUR ---
!
    call rsutn2(resu, nomcha, ' ', 1, knume,&
                nbordr)
    call jeveuo(knume, 'L', lordr)
!
    interp = 'NON NON '
!
    call focrr0(nomfon, interp, base, resu, nomcha,&
                maille, noeud, cmp, npoint, nusp,&
                ivari, nbordr, zi(lordr))
!
    call jedetr(knume)
!
    call jedema()
end subroutine
