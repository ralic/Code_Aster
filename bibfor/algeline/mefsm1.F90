subroutine mefsm1(vale, matgen, base, nomnum, nomsto,&
                  nbmode, nbloc, nterm)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/ualfva.h'
    include 'asterfort/wkvect.h'
    integer :: nbmode, nbloc, nterm
    real(kind=8) :: vale(*)
    character(len=19) :: matgen
    character(len=1) :: base
    character(len=19) :: nomnum, nomsto
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
    integer :: i, j, iblo, iadesc, ialime, iaconl, jrefa, ldblo
    character(len=8) :: k8b
    character(len=19) :: matrge
! DEB------------------------------------------------------------------
!
    call jemarq()
!
    matrge = matgen
!
    call wkvect(matrge//'.DESC', 'G V I', 3, iadesc)
    zi(iadesc) = 2
    zi(iadesc+1) = nbmode
    zi(iadesc+2) = 2
!
    call wkvect(matrge//'.LIME', 'G V K24', 1, ialime)
    zk24(ialime) = '                        '
!
    call wkvect(matrge//'.CONL', 'G V R', nbmode, iaconl)
    do 10 i = 1, nbmode
        zr(iaconl+i-1) = 1.0d0
10  end do
!
    call wkvect(matrge//'.REFA', 'G V K24', 11, jrefa)
    zk24(jrefa-1+11)='MPI_COMPLET'
    zk24(jrefa-1+1) = base
    zk24(jrefa-1+2) = nomnum
    zk24(jrefa-1+9) = 'MS'
    zk24(jrefa-1+10) = 'GENE'
!
    call jecrec(matrge//'.UALF', 'G V R', 'NU', 'DISPERSE', 'CONSTANT',&
                nbloc)
    call jeecra(matrge//'.UALF', 'LONMAX', nterm, k8b)
!
    iblo = 1
!
    call jecroc(jexnum(matrge//'.UALF', iblo))
    call jeveuo(jexnum(matrge//'.UALF', iblo), 'E', ldblo)
!
    nterm = 0
    do 20 i = 1, nbmode
        do 22 j = 1, i
            nterm = nterm + 1
            zr(ldblo+nterm-1) = vale( j + (i-1)*nbmode )
22      continue
20  end do
!
    call ualfva(matrge, 'G')
!
!
    call jedema()
!
end subroutine
