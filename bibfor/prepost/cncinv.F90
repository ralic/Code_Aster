subroutine cncinv(mail, lima, nlima, base, nomz)
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
!
!**********************************************************************
!   OPERATION REALISEE
!   ------------------
!     CONSTRUCTION DE LA TABLE DE CONNECTIVITE INVERSE, I.E. :
!
!     NOEUD --> LISTE DES MAILLES CONTENANT LE NOEUD
!
!   ATTENTION :
!   -----------
!      - SI LIMA EST FOURNI (NBMA > 0) , LA CONNECTIVITE INVERSE
!        RETOURNEE FAIT REFERENCE AUX INDICES DES MAILLES DANS LIMA:
!        NUMA=LIMA(CONINV(NUNO))
!      - SI LIMA N'EST PAS FOURNI (NBMA = 0) , LA CONNECTIVITE INVERSE
!        RETOURNEE FAIT REFERENCE AUX NUMEROS DES MAILLES DU MAILLAGE:
!        NUMA=CONINV(NUNO)
!      - CONVENTION : SI UN NOEUD NUNO EST ORPHELIN :
!        LONG(CONINV(NUNO))=1 ET CONINV(NUNO)(1)=0
!
!   ARGUMENTS EN ENTREE
!   ------------------
!     MAIL   : NOM DU MAILLAGE
!     LIMA   : LISTE DES NUMEROS DE MAILLES
!     NLIMA  : NOMBRE DE MAILLES DANS LIMA
!              SI NLIMA=0 ON PREND TOUT LE MAILLAGE
!     BASE   : BASE DE CREATION POUR NOMZ
!     NOMZ   : NOM DE L' OJB A CREER
!
!   ORGANISATION DE L'OBJET CREE DE NOM NOMZ :
!   --------------------------------------------
!     TYPE : XC V I ACCES(NUMEROTE) LONG(VARIABLE)
!
!**********************************************************************
!
    implicit none
!
!
!  FONCTIONS EXTERNES
!  ------------------
    include 'jeveux.h'
!
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
!
!  -----------------------------------------
!
!
!  ---------------------------------
!
! --- VARIABLES
    character(len=*) :: nomz
    character(len=24) :: nom
    character(len=8) :: mail
    character(len=1) :: base
    integer :: lima(*), nlima, nno, nma, ima, nare
    integer :: i, j, n, p0, p1, p2, p3, q0, q1, q2, q3
!
! --- LECTURE DONNEES
!
    call jemarq()
!
    nom = nomz
!
    call jeveuo(mail//'.DIME', 'L', p0)
    nno = zi(p0)
    if (nlima .eq. 0) then
        nma = zi(p0+2)
    else
        nma = nlima
    endif
!
    call jeveuo(mail//'.CONNEX', 'L', p1)
    call jeveuo(jexatr(mail//'.CONNEX', 'LONCUM'), 'L', p2)
!
    if ((nno.le.0) .or. (nma.le.0)) goto 90
!
! --- ALLOCATION OBJETS TEMPORAIRES
!
    call wkvect('&&CNCINV.INDICE', 'V V I', nma, q0)
    call wkvect('&&CNCINV.NMAILLE', 'V V I', nno, q1)
    call wkvect('&&CNCINV.POINTEUR', 'V V I', nno+1, q2)
!
    if (nlima .eq. 0) then
!
        do 10 i = 1, nma
            zi(q0-1+i) = i
10      continue
!
    else
!
        do 20 i = 1, nma
            zi(q0-1+i) = lima(i)
20      continue
!
    endif
!
    do 30 i = 1, nno
        zi(q1-1+i) = 0
30  end do
!
! --- NOMBRE DE MAILLES POUR CHAQUE NOEUD
!
    do 40 i = 1, nma
!
        ima = zi(q0-1+i)
        p0 = zi(p2-1+ima)
        n = zi(p2+ima)-p0
        p0 = p1 + p0 - 1
!
        do 40 j = 1, n
            p3 = q1-1+zi(p0)
            p0 = p0 + 1
            zi(p3) = zi(p3) + 1
40      continue
!
! --- NOMBRE TOTAL D'ARETES NOEUD/MAILLE
!
    nare = 0
    zi(q2) = 0
!
    do 50 i = 1, nno
!
        n = zi(q1-1+i)
        if (n .eq. 0) n = 1
!
        nare = nare + n
        zi(q2+i) = nare
!
50  end do
!
! --- ALLOCATION DU GRAPHE NOEUD/MAILLE
!
    call jecrec(nom, base//' V I', 'NU', 'CONTIG', 'VARIABLE',&
                nno)
    call jeecra(nom, 'LONT', nare, ' ')
!
    do 60 i = 1, nno
!
        n = zi(q1-1+i)
        if (n .eq. 0) n = 1
        call jecroc(jexnum(nom, i))
        call jeecra(jexnum(nom, i), 'LONMAX', n, ' ')
!
60  end do
!
! --- GRAPHE NOEUD/MAILLE
!
    call jeveuo(nom, 'E', q3)
!
    do 70 i = 1, nare
        zi(q3-1+i) = 0
70  end do
!
    do 80 i = 1, nma
!
        ima = zi(q0-1+i)
        p0 = zi(p2-1+ima)
        n = zi(p2+ima)-p0
        p0 = p1 + p0 - 1
!
        do 80 j = 1, n
!
            q1 = q2-1+zi(p0)
            p0 = p0 + 1
            zi(q3+zi(q1)) = i
            zi(q1) = zi(q1) + 1
!
80      continue
!
! --- DESALLOCATION
!
    call jedetr('&&CNCINV.INDICE')
    call jedetr('&&CNCINV.NMAILLE')
    call jedetr('&&CNCINV.POINTEUR')
!
90  continue
!
    call jedema()
!
end subroutine
