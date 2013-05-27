subroutine rc32f6(nbp12, nbp23, nbp13, nbsigr, nbsg1,&
                  nbsg2, nbsg3, sigr, nocc, saltij)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rc32f4.h'
    include 'asterfort/u2mess.h'
    integer :: nbp12, nbp23, nbp13, nbsigr, nocc(*), nbsg1, nbsg2, nbsg3
    integer :: sigr(*)
    real(kind=8) :: saltij(*)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     CALCUL DU FACTEUR D'USAGE POUR LES SITUATIONS DE PASSAGE
!        SI NOCC(CHEMINS DE PASSAGE) = 0
!        ALORS IL N'EXISTE PLUS DE CHEMIN DE PASSAGE
!
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: nbsips, jnpass, i, k, ioc1, nsitup
    character(len=3) :: typass
    character(len=8) :: k8b
!     ------------------------------------------------------------------
!
    typass = '1_2'
    if (nbp12 .eq. 0) goto 9999
    call jelira('&&RC32SI.PASSAGE_'//typass, 'LONUTI', nbsips, k8b)
    call jeveuo('&&RC32SI.PASSAGE_'//typass, 'L', jnpass)
    do 10 i = 1, nbsips
        nsitup = zi(jnpass+i-1)
        do 12 k = 1, nbsigr
            if (sigr(k) .eq. nsitup) then
                ioc1 = k
                goto 14
            endif
12      continue
        call u2mess('F', 'POSTRCCM_36')
14      continue
        if (nocc(ioc1) .ne. 0) goto 9999
10  end do
    nbp12 = 0
    call rc32f4(typass, nbp12, nbp23, nbp13, nbsigr,&
                nbsg1, nbsg2, nbsg3, saltij)
!
9999  continue
    typass = '2_3'
    if (nbp23 .eq. 0) goto 9997
    call jelira('&&RC32SI.PASSAGE_'//typass, 'LONUTI', nbsips, k8b)
    call jeveuo('&&RC32SI.PASSAGE_'//typass, 'L', jnpass)
    do 20 i = 1, nbsips
        nsitup = zi(jnpass+i-1)
        do 22 k = 1, nbsigr
            if (sigr(k) .eq. nsitup) then
                ioc1 = k
                goto 24
            endif
22      continue
        call u2mess('F', 'POSTRCCM_36')
24      continue
        if (nocc(ioc1) .ne. 0) goto 9997
20  end do
    nbp23 = 0
    call rc32f4(typass, nbp12, nbp23, nbp13, nbsigr,&
                nbsg1, nbsg2, nbsg3, saltij)
!
9997  continue
    typass = '1_3'
    if (nbp13 .eq. 0) goto 9995
    call jelira('&&RC32SI.PASSAGE_'//typass, 'LONUTI', nbsips, k8b)
    call jeveuo('&&RC32SI.PASSAGE_'//typass, 'L', jnpass)
    do 30 i = 1, nbsips
        nsitup = zi(jnpass+i-1)
        do 32 k = 1, nbsigr
            if (sigr(k) .eq. nsitup) then
                ioc1 = k
                goto 34
            endif
32      continue
        call u2mess('F', 'POSTRCCM_36')
34      continue
        if (nocc(ioc1) .ne. 0) goto 9995
30  end do
    nbp13 = 0
    call rc32f4(typass, nbp12, nbp23, nbp13, nbsigr,&
                nbsg1, nbsg2, nbsg3, saltij)
!
9995  continue
!
end subroutine
