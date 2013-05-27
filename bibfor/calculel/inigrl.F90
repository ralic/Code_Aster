subroutine inigrl(ligrel, igrel, nmax, adtabl, k24tab,&
                  nval)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! TOLE CRS_513
!
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
!
    include 'asterfort/ini002.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    character(len=*) :: ligrel
    integer :: igrel, nmax, adtabl(nmax), nval
    character(len=24) :: k24tab(nmax)
! ----------------------------------------------------------------------
!     BUT:
!     INITIALISER LE TYPE_ELEMENT ASSOCIE AU GREL  (INI00K)
!
!     IN:
!      LIGREL : NOM DU LIGREL A INITIALISER
!      IGREL  : NUMERO DU GREL
!      NMAX   : DIMENSION DE K24TAB ET ADTABL
!
!     OUT:
!         CREATION DES OBJETS JEVEUX PROPRES AU
!             TYPE_ELEMENT PRESENTS DANS LE LIGREL(IGREL).
!
!         NVAL  : NOMBRE DE NOMS RENDUS DANS K24TAB
!         K24TAB: TABLEAU DES NOMS DES OBJETS '&INEL.XXXX'
!         ADTABL : TABLEAU D'ADRESSES DES '&INEL.XXXXX'
!         ADTABL(I) = 0 SI L'OBJET CORRESPONDANT N'EXISTE PAS
!         SI NVAL > NMAX  : ON  S'ARRETE EN ERREUR FATALE
!
! ----------------------------------------------------------------------
    character(len=1) :: k1bid
    character(len=24) :: noliel
    character(len=16) :: nomte
    integer :: liel, l, te, k
!
!
!
    noliel = ligrel(1:19)//'.LIEL'
    call jeveuo(jexnum(noliel, igrel), 'L', liel)
    call jelira(jexnum(noliel, igrel), 'LONMAX', l, k1bid)
    te = zi(liel-1+l)
    call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
!
!     -- ON MET LES ADRESSES A ZERO :
    do 10,k = 1,nmax
    k24tab(k) = ' '
    adtabl(k) = 0
    10 end do
!
    call ini002(nomte, nmax, adtabl, k24tab, nval)
!
!
end subroutine
