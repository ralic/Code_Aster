subroutine mtdsc3(nommat)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/ismaem.h'
    include 'asterfort/assert.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jeveut.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: nommat
!     ------------------------------------------------------------------
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
!     ALLOCATION DES DESCRIPTEURS D'UNE MATRICE de CONTACT-FROTTEMENT
!     PLEINE SYMETRIQUE, REELLE SANS DDLS ELIMINES
!     ------------------------------------------------------------------
!
! IN  NOMMAT  : K19 : NOM DE LA MATRICE
!     ------------------------------------------------------------------
!     CETTE ROUTINE CREE 2 OBJETS DE TRAVAIL SUR LA BASE VOLATILE
!
!     DE NOM  NOMMAT//'.&INT'   VECTEUR D'ENTIER
!             NOMMAT//'.&IN2'   VECTEUR DE K24
!
!     ZI(+0) : INUTILISE
!     ZK24(ZI(+1) : NOM DEVELOPPEUR DE LA MATRICE + 4 BLANCS
!     ZI(+2) : NOMBRE D'EQUATIONS
!     ZI(+3) : 1 (REELLE)
!     ZI(+4) : 1 (SYMETRIQUE)
!     ZI(+7) : 0
!     ZI(+14) : TAILLE DES BLOCS DE LA MATRICE
!     ------------------------------------------------------------------
!
!
!
!     ----- PARAMETRES DE DEFINITION DES MATRICES ----------------------
    character(len=4) :: kbid
    character(len=14) :: nu
    character(len=19) :: mat19, nomsto
!     ------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
    integer :: ibid, ier, jrefa, jscde, k, lmat
    integer :: lnom
!-----------------------------------------------------------------------
    call jemarq()
    mat19 = nommat
!
!
!        ------ ALLOCATION DES OBJETS SI NECESSAIRE :
    call jeexin(mat19//'.&INT', ier)
    if (ier .eq. 0) then
        call jecreo(mat19//'.&INT', ' V V I')
        call jeecra(mat19//'.&INT', 'LONMAX', 19, '  ')
    endif
!
    call jeveuo(mat19//'.&INT', 'E', lmat)
    do 10,k = 1,19
    zi(lmat-1+k) = ismaem()
    10 end do
!
    call jeexin(mat19//'.&IN2', ier)
    if (ier .eq. 0) then
        call wkvect(mat19//'.&IN2', ' V V K24', 1, lnom)
    endif
!
    call jeveut(mat19//'.&IN2', 'E', lnom)
    zk24(lnom) = mat19
!
!
!     -- LMAT+1 :
!     ------------
    zi(lmat+1) = lnom
!
!
    call jeveuo(mat19//'.REFA', 'L', jrefa)
    nu = zk24(jrefa-1+2)
    nomsto = nu//'.SLCS'
!
!
!     -- LMAT+2 :
!     ------------
    call jeveuo(nomsto//'.SCDE', 'L', jscde)
    zi(lmat+2) = zi(jscde-1+1)
!
!
!     -- LMAT+3 :
!     ------------
    call jelira(mat19//'.UALF', 'TYPE', ibid, kbid)
    call assert(kbid(1:1).eq.'R')
    zi(lmat+3) = 1
!
!
!     -- LMAT+4 :
!     ------------
    zi(lmat+4) = 1
!
!
!     -- LMAT+14
!     ----------
    zi(lmat+14) = zi(jscde-1+2)
!
!
!     -- LMAT+7 ET LMAT+18  (SI CHARGES CINEMATIQUES) :
!     -------------------------------------------------
    call jeexin(mat19//'.CCID', ier)
    call assert(ier.eq.0)
    zi(lmat+7) = 0
    zi(lmat+18) = 0
!
!
    call jedema()
end subroutine
