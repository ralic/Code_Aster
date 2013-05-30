subroutine resu60(resu1, resu2)
    implicit none
! ----------------------------------------------------------------------
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
!
!     CETTE ROUTINE PERMET LA CONCATENATION DE DEUX CONCEPTS DYNA_GENE
!     DE TYPE HARMONIQUE CALCULES PAR DEUX COMMANDE DYNA_LINE_HARM
!     RESU1 ET RESU2 SONT COPIES DANS RESU1
!
!     LA ROUTINE RESU74 FAIT LA MEME CHOSE MAIS POUR DES CALCULS TRANS.
! ----------------------------------------------------------------------
    include 'jeveux.h'
    include 'asterfort/copvis.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetc.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'blas/dcopy.h'
    include 'blas/zcopy.h'
    character(len=8) :: resu1, resu2
!
! IN  : RESU1 : PREMIER CONCEPT DYNA_GENE HARMONIQUE
! IN  : RESU2 : SECOND CONCEPT DYNA_GENE HARMONIQUE
!
    integer :: nbsto1, nbsau1, nbsto2, nbsau2
    integer :: nbstoc, nbsauv
    character(len=1) :: k1bid
    character(len=8) :: resu
    integer :: i
    integer :: flagd1, flagv1, flaga1, flagd2, flagv2, flaga2
    logical :: flagd, flagv, flaga
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: lref1, lref2
    integer :: jacce, jacce1, jacce2, jvite, jvite1, jvite2
    integer :: jdepl, jdepl1, jdepl2, jdesc
    integer :: jfreq, jfreq1, jfreq2, jordr, jordr1, jordr2
!-----------------------------------------------------------------------
    call jemarq()
    resu = '88888'
!
!     --- VERIFICATION DE LA COMPATIBILITE DES RESULTATS A MERGER
    call jeexin(resu1//'           .DEPL', flagd1)
    call jeexin(resu1//'           .VITE', flagv1)
    call jeexin(resu1//'           .ACCE', flaga1)
    call jeexin(resu2//'           .DEPL', flagd2)
    call jeexin(resu2//'           .VITE', flagv2)
    call jeexin(resu2//'           .ACCE', flaga2)
!
    flagd = ((flagd1.eq.0).and.(flagd2.eq.0)) .or. ((flagd1.ne.0).and.(flagd2.ne.0))
    flagv = ((flagv1.eq.0).and.(flagv2.eq.0)) .or. ((flagv1.ne.0).and.(flagv2.ne.0))
    flaga = ((flaga1.eq.0).and.(flaga2.eq.0)) .or. ((flaga1.ne.0).and.(flaga2.ne.0))
!
!     CONDITION POUR SAVOIR SI LES FLAGS SONT BIEN TOUS LES 2 ZEROS
!     OU BIEN DIFFERENTS DE ZERO = COMPATIBILITE DES RESUS
    if (.not.(flagd.and.flagv.and.flaga)) then
        call u2mess('F', 'ALGORITH17_25')
    endif
!
    call jeveuo(resu1//'           .DESC', 'E', jdesc)
!
!     --- RECHERCHE DU NUMERO D'ORDRE DE LA FREQUENCE DE REPRISE
    if (flagd1 .gt. 0) then
        call jelira(resu1//'           .DEPL', 'LONUTI', nbsto1, k1bid)
        call jelira(resu2//'           .DEPL', 'LONUTI', nbsto2, k1bid)
    else if (flagv1.gt.0) then
        call jelira(resu1//'           .VITE', 'LONUTI', nbsto1, k1bid)
        call jelira(resu2//'           .VITE', 'LONUTI', nbsto2, k1bid)
    else
        call jelira(resu1//'           .ACCE', 'LONUTI', nbsto1, k1bid)
        call jelira(resu2//'           .ACCE', 'LONUTI', nbsto2, k1bid)
    endif
!
    nbstoc = nbsto1+nbsto2
!
!     --- RECUPERATION DES CHAMPS DEPL VITE ET ACCE ---
    if (flagd1 .ne. 0) then
        call jeveuo(resu1//'           .DEPL', 'E', jdepl1)
        call jeveuo(resu2//'           .DEPL', 'E', jdepl2)
        call wkvect(resu //'           .DEPL', 'G V C', nbstoc, jdepl)
        call zcopy(nbsto1, zc(jdepl1), 1, zc(jdepl), 1)
        call zcopy(nbsto2, zc(jdepl2), 1, zc(jdepl+nbsto1), 1)
    endif
!                         ^
!                         |______________
!       --- VALEURS COMPLEXES = COPIER |2| FOIS PLUS DE REELS
!
    if (flagv1 .ne. 0) then
        call jeveuo(resu1//'           .VITE', 'E', jvite1)
        call jeveuo(resu2//'           .VITE', 'E', jvite2)
        call wkvect(resu //'           .VITE', 'G V C', nbstoc, jvite)
        call zcopy(nbsto1, zc(jvite1), 1, zc(jvite), 1)
        call zcopy(nbsto2, zc(jvite2), 1, zc(jvite+nbsto1), 1)
    endif
!
    if (flaga1 .ne. 0) then
        call jeveuo(resu1//'           .ACCE', 'E', jacce1)
        call jeveuo(resu2//'           .ACCE', 'E', jacce2)
        call wkvect(resu //'           .ACCE', 'G V C', nbstoc, jacce)
        call zcopy(nbsto1, zc(jacce1), 1, zc(jacce), 1)
        call zcopy(nbsto2, zc(jacce2), 1, zc(jacce+nbsto1), 1)
    endif
!
!     --- RECUPERATION DES CHAMPS ORDR
!
    call jeveuo(resu1//'           .ORDR', 'E', jordr1)
    call jelira(resu1//'           .ORDR', 'LONUTI', nbsau1, k1bid)
!
    call jeveuo(resu2//'           .ORDR', 'E', jordr2)
    call jelira(resu2//'           .ORDR', 'LONUTI', nbsau2, k1bid)
!     --- CUMULER LES NUMEROS D'ORDRE POUR CONSERVER LA MONOTONIE
    do 20 i = 0, nbsau2-1
        zi(jordr2+i) = zi(jordr2+i) + zi(jordr1+nbsau1-1) + 1
20  end do
!
    nbsauv = nbsau1 + nbsau2
!
    call wkvect(resu//'           .ORDR', 'G V I', nbsauv, jordr)
    call copvis(nbsau1, zi(jordr1), zi(jordr))
    call copvis(nbsau2, zi(jordr2), zi(jordr+nbsau1))
!
    call jeveuo(resu1//'           .DISC', 'E', jfreq1)
    call jeveuo(resu2//'           .DISC', 'E', jfreq2)
    call wkvect(resu//'           .DISC', 'G V R', nbsauv, jfreq)
    call dcopy(nbsau1, zr(jfreq1), 1, zr(jfreq), 1)
    call dcopy(nbsau2, zr(jfreq2), 1, zr(jfreq+nbsau1), 1)
!
!     --- DUPLICATION ---
!
    if (flagd1 .ne. 0) call jedupo(resu//'           .DEPL', 'G', resu1//'           .DEPL',&
                                   .false.)
    if (flagv1 .ne. 0) call jedupo(resu//'           .VITE', 'G', resu1//'           .VITE',&
                                   .false.)
    if (flaga1 .ne. 0) call jedupo(resu//'           .ACCE', 'G', resu1//'           .ACCE',&
                                   .false.)
    call jedupo(resu//'           .ORDR', 'G', resu1//'           .ORDR', .false.)
    call jedupo(resu//'           .DISC', 'G', resu1//'           .DISC', .false.)
!
!
!     --- COPIE DU NOUVEAU .REFD DANS LA SD FINALE ---
!
    call jeveuo(resu1//'           .REFD', 'E', lref1)
    call jeveuo(resu2//'           .REFD', 'L', lref2)
    do 10 i = 0, 4
        zk24(lref1+i) = zk24(lref2+i)
10  end do
!
!     --- DESTRUCTION DES OBJETS PROVISOIRES
!
    call jedetc('G', resu//'           ', 1)
    call jedetc('G', resu2//'           ', 1)
!
    call jedema()
!
end subroutine
