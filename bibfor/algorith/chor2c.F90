subroutine chor2c(lischa, vecele)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
! ======================================================================
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/corich.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lisltc.h'
    include 'asterfort/sdchgd.h'
    include 'asterfort/wkvect.h'
    include 'blas/dcopy.h'
    character(len=19) :: lischa, vecele
!
! ----------------------------------------------------------------------
!
! TRANSFORMATION D'UNE COLLECTION DE CHAM_NO A VALEURS REELLES EN
! UNE COLLECTION DE CHAM_NO A VALEURS COMPLEXES AVEC PARTIE
! IMAGINAIRE NULLE
!
! ----------------------------------------------------------------------
!
! I/O VECELE : NOM DU VECT_ELEM
!
!
!
!
    character(len=24) :: vachar
    integer :: jvacha
    character(len=19) :: chamno
    integer :: jcn
    character(len=24) :: resuel
    character(len=8) :: k8bid, typech, typsca
    integer :: iret, kvale, ibid, ichar
    integer :: ivec, nbvec, nbvdim, ivale, nbvale, jvec
    character(len=4) :: tyresl
    character(len=1) :: typchn
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- VERIFICATION DU VACHAR
!
    vachar = vecele//'.CHNO'
    call jeexin(vachar, iret)
    call assert(iret.ne.0)
    call jelira(vachar, 'LONMAX', nbvec, k8bid)
    call assert(nbvec.ne.0)
!
! --- DIMENSIONNEMENT SD DE SAUVEGARDE
!
    call jeveuo(vachar, 'L', jvacha)
    chamno = zk24(jvacha+1-1)(1:19)
    call jeveuo(vecele//'.RELR', 'L', jvec)
    call jelira(chamno//'.VALE', 'LONMAX', nbvdim, k8bid)
    call wkvect('&&CHOR2C.COPIE_TRAVAIL', 'V V R', nbvdim, kvale)
!
! --- BOUCLES SUR LES CHAMNO
!
    do 10 ivec = 1, nbvec
!
! ----- NOM DU RESU_ELEM
!
        resuel = zk24(jvec-1+ivec)
!
! ----- NOM DU CHAMNO
!
        chamno = zk24(jvacha+ivec-1)(1:19)
!
! ----- RECUPERATION DU NUMERO DE LA CHARGE DU RESU_ELEM
!
        call corich('L', resuel, ibid, ichar)
        call assert((ichar.ne.0).and.(ichar.ge.-2))
!
! ----- TYPE DU CHARGEMENT
!
        call dismoi('F', 'TYPE_CHAMP', resuel, 'CHAMP', ibid,&
                    tyresl, ibid)
        if (tyresl .eq. 'RESL') then
            call dismoi('F', 'TYPE_SCA', resuel, 'RESUELEM', ibid,&
                        typsca, ibid)
            if (typsca .eq. 'R') then
                typchn = 'R'
            else if (typsca.eq.'C') then
                typchn = 'C'
            else
                call assert(.false.)
            endif
        else if (tyresl.eq.'NOEU') then
            call lisltc(lischa, ichar, typech)
            typchn = 'R'
            if (typech .eq. 'COMP') typchn = 'C'
        else
!
        endif
!
! ----- CONVERSION
!
        if (typchn .eq. 'R') then
            call jeveuo(chamno//'.VALE', 'L', jcn)
            call jelira(chamno//'.VALE', 'LONMAX', nbvale, k8bid)
            if (nbvdim .ne. nbvale) call assert(.false.)
!
! ------- SAUVEGARDE DES VALEURS
!
            call dcopy(nbvale, zr(jcn), 1, zr(kvale), 1)
!
! ------- DESTRUCTION CHAMNO A VALEURS REELLES
!
            call jedetr(chamno//'.VALE')
!
! ------- CREATION CHAMNO A VALEURS COMPLEXES
!
            call wkvect(chamno//'.VALE', 'V V C', nbvale, jcn)
            do 20 ivale = 1, nbvale
                zc(jcn+ivale-1) = dcmplx(zr(kvale+ivale-1),0.d0)
20          continue
!
! ------- CHANGEMENT DE LA REFERENCE A LA GRANDEUR
!
            call sdchgd(chamno, 'C')
        endif
10  end do
!
    call jedetr('&&CHOR2C.COPIE_TRAVAIL')
    call jedema()
end subroutine
