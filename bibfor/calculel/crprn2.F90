subroutine crprn2(pfchno, base, nbnoeu, nequa, nec)
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
    implicit none
!     ------------------------------------------------------------------
!     CREATION ET ALLOCATION D'UNE STRUCTURE PROF_CHNO "PFCHNO"
!     ------------------------------------------------------------------
! IN  PFCHNO : CH19: NOM DU PROF_CHNO A CREER
! IN  BASE   : CH1 : NOM DE LA BASE SUR LAQUELLE LE PROF_CHNO DOIT ETRE
!                    CREE
! IN  NBNOEU : I   : NOMBRE DE NOEUDS DU MAILLAGE ASSOCIE AU PROF_CHNO
! IN  NEQUA  : I   : NOMBRE DE CMPS TOTAL DU PFCHNO (LONG(.VALE))
! IN  NEC    : I   : NOMBRE D'ENTIERS CODES POUR LA GRANDEUR
!     ------------------------------------------------------------------
!     PRECAUTIONS D'EMPLOI :
!       1) LE PROF_CHNO "PFCHNO" NE DOIT PAS EXISTER
!       2) LE .PRNO N'EST PAS AFFECTE
!       3) LE DEEQ N'EST PAS AFFECTE DANS CETTE ROUTINE (VOIR PTEEQU)
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    include 'jeveux.h'
!
    include 'asterfort/jecrec.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    integer :: nbval, nbnoeu, nequa, nec, lprno, lonprn, ianueq, i
    character(len=*) :: pfchno, base
    character(len=1) :: classe
    character(len=8) :: cbid
    character(len=24) :: lili, prno, nueq
!     ------------------------------------------------------------------
    data lili/'                   .LILI'/
    data prno/'                   .PRNO'/
    data nueq/'                   .NUEQ'/
!     ------------------------------------------------------------------
    call jemarq()
    classe = base(1:1)
!
!     --------------------------- LILI --------------------------------
!     --- REPERTOIRE DES NOMS DE LIGREL ---
!     --- ATTENTION ICI UN SEUL LIGREL : &MAILLA ---
    nbval = 1
    lili(1:19) = pfchno
    call jecreo(lili, classe//' N K24')
    call jeecra(lili, 'NOMMAX', nbval, '  ')
!
!
!
!
!     --------------------------- PRNO --------------------------------
!     ------------- CREATION DE LA COLLECTION PROFIL NOEUD ------------
!
    prno(1:19) = pfchno
    lonprn = (2+nec)*nbnoeu
    call jecrec(prno, classe//' V I', 'NU', 'CONTIG', 'CONSTANT',&
                1)
    call jeecra(prno, 'LONMAX', lonprn, cbid)
    call jecroc(jexnom(prno(1:19)//'.LILI', '&MAILLA'))
    call jecroc(jexnum(prno, 1))
    call jeveuo(prno, 'E', lprno)
!     --------------------------- PRNO --------------------------------
!     ------------- CREATION DE L'OBJET .NUEQ -------------------------
!     ----------ON LE REMPLIT PAR L'INDIRECTION "IDENTITE" !!
!
    nueq(1:19) = pfchno
    call wkvect(nueq, classe//' V I', nequa, ianueq)
    do 1,i = 1,nequa
    zi(ianueq-1+i) = i
    1 end do
!
    call jedema()
end subroutine
