subroutine masyns(matas)
! person_in_charge: jacques.pellet at edf.fr
    implicit none
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
!  BUT :
!   TRANSFORMER UNE MATR_ASSE SYMETRIQUE EN UNE MATR_ASSE NON-SYMETRIQUE
!   POUR CELA, ON DUPPLIQUE LES VALEURS DE LA MOITIE SUPERIEURE
!
!  ARGUMENT :
!    MATAS K19  JXVAR : MATR_ASSE A RENDRE NON-SYMETRIQUE
!
! ========================= DEBUT DES DECLARATIONS ====================
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    character(len=*) :: matas
    character(len=1) :: base, ktyp, kbid
    character(len=19) :: mat19
    integer :: jrefa, i, nbloc, ibid, lgbloc, jvalmi, jvalms, jvalma
!
!---------------------------------------------------------------------
    call jemarq()
!
    mat19 = matas(1:19)
    call jelira(mat19//'.VALM', 'CLAS', ibid, base)
    call jelira(mat19//'.VALM', 'TYPE', ibid, ktyp)
    call assert(ktyp.eq.'R'.or.ktyp.eq.'C')
    call jeveuo(mat19//'.REFA', 'E', jrefa)
    call assert(zk24(jrefa-1+9).eq.'MS')
    zk24(jrefa-1+9)='MR'
!
    call jelira(mat19//'.VALM', 'NMAXOC', nbloc, kbid)
    call assert(nbloc.eq.1)
    call jelira(jexnum(mat19//'.VALM', 1), 'LONMAX', lgbloc, kbid)
!
!
    call jedupo(mat19//'.VALM', 'V', '&&MASYNS.VALM', .false.)
    call jedetr(mat19//'.VALM')
!
    call jecrec(mat19//'.VALM', base//' V '//ktyp, 'NU', 'DISPERSE', 'CONSTANT',&
                2)
    call jeecra(mat19//'.VALM', 'LONMAX', lgbloc, ' ')
    call jecroc(jexnum(mat19//'.VALM', 1))
    call jeveuo(jexnum(mat19//'.VALM', 1), 'E', jvalms)
    call jecroc(jexnum(mat19//'.VALM', 2))
    call jeveuo(jexnum(mat19//'.VALM', 2), 'E', jvalmi)
!
!
    call jeveuo(jexnum('&&MASYNS.VALM', 1), 'L', jvalma)
    if (ktyp .eq. 'R') then
        do 20 i = 1, lgbloc
            zr(jvalms+i-1) = zr(jvalma+i-1)
            zr(jvalmi+i-1) = zr(jvalma+i-1)
20      continue
    else
        do 21 i = 1, lgbloc
            zc(jvalms+i-1) = zc(jvalma+i-1)
            zc(jvalmi+i-1) = zc(jvalma+i-1)
21      continue
    endif
    call jedetr('&&MASYNS.VALM')
!
!
    call jedema()
end subroutine
