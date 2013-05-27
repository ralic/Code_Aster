subroutine ligrma(ma, listgr)
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
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/dismoi.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: ma
    character(len=24) :: listgr
!
!   POUR CHAQUE MAILLE :
!   CREER LA LISTE DES GROUPES DE MAILLES CONTENANT CETTE MAILLE
!
!   IN
!       MA      : MAILLAGE
!
!   OUT
!       LISTGR  : LISTE DES GROUPES DE MAILLES
!
    integer :: ibid, nbma, iret, nbgma, i, iagma, n, ii, ima, jlong, jlist
    integer :: nbmat, jcpt
    character(len=8) :: kbid
    character(len=24) :: long, vcpt
!
!
    call jemarq()
!
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbma,&
                kbid, ibid)
!
!     CREATION DU VECTEUR CONTENANT LE NOMBRE DE GROUPES PAR MAILLE
    long = '&&LIGRMA.LONG'
    call wkvect(long, 'V V I', nbma, jlong)
!
!     CREATION DU VECTEUR COMPTEUR
    vcpt = '&&LIGRMA.VCPT'
    call wkvect(vcpt, 'V V I', nbma, jcpt)
!
    call jeexin(ma//'.GROUPEMA', iret)
    nbgma = 0
    nbmat = 0
    if (iret .gt. 0) call jelira(ma//'.GROUPEMA', 'NUTIOC', nbgma, kbid)
!
!     ON PARCOURT LES GROUPES DE MAILLES ET ON REMPLIT L'OBJET LONG
    do 10,i=1,nbgma
    call jeveuo(jexnum(ma//'.GROUPEMA', i), 'L', iagma)
    call jelira(jexnum(ma//'.GROUPEMA', i), 'LONUTI', n, kbid)
    do 11, ii=1,n
    ima=zi(iagma-1+ii)
    zi(jlong-1+ima)=zi(jlong-1+ima)+1
    nbmat = nbmat +1
11  continue
    10 end do
!
!     CREATION DE LA COLLECTION CONTINUE A NBMA LIGNES DE LONGUEUR
!     TOTALE EGALE A NBMAT
    call jecrec(listgr, 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbma)
    call jeecra(listgr, 'LONT', nbmat, kbid)
    do 20 ima = 1, nbma
        call jeecra(jexnum(listgr, ima), 'LONMAX', zi(jlong-1+ima), kbid)
        call jecroc(jexnum(listgr, ima))
20  end do
!
!     REMPLISSAGE DE LISTGR AVEC LES NUMEROS DES GROUPES
    do 30,i=1,nbgma
    call jeveuo(jexnum(ma//'.GROUPEMA', i), 'L', iagma)
    call jelira(jexnum(ma//'.GROUPEMA', i), 'LONUTI', n, kbid)
    do 31, ii=1,n
    ima=zi(iagma-1+ii)
    call jeveuo(jexnum(listgr, ima), 'E', jlist)
!         ON AJOUTE LE GROUPE I A LA LISTE DES GROUPES POUR CETTE MAILLE
    zi(jcpt-1+ima) = zi(jcpt-1+ima) + 1
    zi(jlist-1+zi(jcpt-1+ima)) = i
31  continue
    30 end do
!
    call jedetr(long)
    call jedetr(vcpt)
!
    call jedema()
end subroutine
