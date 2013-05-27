subroutine asgnbc(ibla, bloca, nbterm, inobl, iadbl,&
                  nomblo, numblo, fact)
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
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    character(len=24) :: nomblo
    integer :: nbterm, inobl(nbterm), iadbl(nbterm), ibla, numblo
    real(kind=8) :: fact
    complex(kind=8) :: bloca(*)
!
!-----------------------------------------------------------------------
!  BUT:      < ASSEMBLAGE GENERALISE BAS NIVEAU >
!
!   ASSEMBLER LES TERME D'UN BLOC ELEMENTAIRE DANS LE BLOC ASSEMBLE
!     (TOUS LES TERMES DU BLOC ELEMENTAIRES N'ONT PAS LE BLOC ASSEMBLE
!   COURANT POUR DESTINATION)
!
!-----------------------------------------------------------------------
!
! NOM----- / /:
!
! IBLA     /M/: NUMERO DU BLOC ASSEMBLE COURANT
! BLOCA    /I/: BLOC ASSEMBLE COURANT
! NBTERM   /I/: NOMBRE DE TERMES BLOC ELEMENTAIRE
! INOBL    /I/: VECTEUR NUMERO BLOC ARRIVEES TERME BLOC ELEMENTAIRE
! IADBL    /I/: VECTEUR DES ADRESSE RELATIVE DANS BLOC ASSEMBLE
! NOMBLO   /I/: NOM K24 DU OU DE LA FAMILLE DES BLOCS ELEMENTAIRES
! NUMBLO   /I/: NUMERO DU BLOC ELEMENTAIRE DANS LA FAMILLE OU 0
! FACT     /I/: FACTEUR REEL MULTIPLICATIF
!
!
!
!
!
    integer :: llblo, i
    complex(kind=8) :: dcmplx
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
    call jemarq()
    if (numblo .eq. 0) then
        call jeveuo(nomblo, 'L', llblo)
        do 10 i = 1, nbterm
            if (inobl(i) .eq. ibla) then
                bloca(iadbl(i))=bloca(iadbl(i))+(fact*zc(llblo+i-1))
            endif
10      continue
    else
        call jeveuo(jexnum(nomblo, numblo), 'L', llblo)
        do 20 i = 1, nbterm
            if (inobl(i) .eq. ibla) then
                bloca(iadbl(i))=bloca(iadbl(i))+ dcmplx(fact*zr(llblo+&
                i-1),0.d0)
            endif
20      continue
    endif
!
    call jedema()
end subroutine
