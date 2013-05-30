subroutine kndif2(long, lk1, l1, lk2, l2,&
                  lk3, l3)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jexnom.h'
    integer :: long, l1, l2, l3
    character(len=*) :: lk1(l1), lk2(l2), lk3(l3)
! ---------------------------------------------------------------------
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
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
! BUT: DIFFERENCE ENTRE 2 LISTES   LK3 = LK1 - LK2
!      CET UTILITAIRE EST A UTILISER LORSQUE LES LISTES LK1 ET LK2
!      SONT LONGUES : ON UTILISE DES POINTEURS DE NOMS JEVEUX
! ---------------------------------------------------------------------
!     ARGUMENTS:
! LONG   IN   I     : 8,16 OU 24 : LONGUEUR DES CHAINES DE LK1 ET LK2
! LK1    IN   V(K*) : LISTE DE K*
! L1     IN   I     : LONGUEUR DE LA LISTE LK1
! LK2    IN   V(K*) : LISTE DES K*
! L2     IN   I     : LONGUEUR DE LA LISTE LK2
! LK3    OUT  V(K*) : LISTE DES K* QUI DOIT CONTENIR LK1 - LK2
! L3     IN   I     : DIMENSION DU TABLEAU LK3
! L3     OUT  I     : LONGUEUR DE LA LISTE LK3
!----------------------------------------------------------------------
!----------------------------------------------------------------------
    character(len=24) :: pn2, kbid
    integer :: k1, k2, nbk3
!
    call assert((long.eq.8).or.(long.eq.16).or.(long.eq.24))
!
    nbk3=l3
    l3 = 0
    if (l1 .eq. 0) goto 9999
!
    if (l2 .eq. 0) then
        l3=l1
        do 20, k1 = 1 , l1
        lk3(k1)=lk1(k1)
20      continue
        goto 9999
    endif
!
!
!     -- ON RECOPIE LK2 DANS UN POINTEUR DE NOMS : PN2
!     -------------------------------------------------
    pn2='KNDIF2.PN2'
    call jecreo(pn2, 'V N K24')
    call jeecra(pn2, 'NOMMAX', l2, kbid)
    do 1, k2=1,l2
    call jecroc(jexnom(pn2, lk2(k2)))
    1 end do
!
!
!     -- ON BOUCLE SUR LES ELEMENTS DE LK1 :
!     ---------------------------------------
    do 2, k1=1,l1
    call jenonu(jexnom(pn2, lk1(k1)), k2)
    if (k2 .eq. 0) then
        l3 = l3 + 1
        call assert(l3.le.nbk3)
        lk3(l3) = lk1(k1)
    endif
    2 end do
!
!
!     -- MENAGE :
!     ------------
    call jedetr(pn2)
!
!
9999  continue
end subroutine
