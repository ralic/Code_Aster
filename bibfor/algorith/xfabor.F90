subroutine xfabor(noma, cnxinv, nunoa, nunob, nunoc,&
                  fabord)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    integer :: nunoa, nunob, nunoc
    character(len=8) :: noma
    character(len=19) :: cnxinv
    logical :: fabord
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!     ------------------------------------------------------------------
!
!      ON VERIFIE SI LES TROIS POINTS APPARTIENNENT A UNE FACE DE BORD
!       DANS LE CADRE DE XFEM (UNIQUEMENT APPELEE EN 3D)
!
!  ENTREES :
!     NOMA     :   NOM DU CONCEPT MAILLAGE
!     CNXINV   :   CONNECTIVITE INVERSE
!     NUNOA    :   ADRESSE DU NOEUD A
!     NUNOB    :   ADRESSE DU NOEUD B
!     NUNOC    :   ADRESSE DU NOEUD C
!
!  SORTIES :
!     FABORD   :   TRUE SSI LA FACE A LAQUELLE APPARTIENNENT LES 3
!                   POINTS EST UNE FACE DE BORD
!
!     ------------------------------------------------------------------
!
    integer :: jma, nmanoa, nmanob, nmanoc, jmanoa, jmanob, jmanoc
    integer :: imaa, imab, imac, itypma, numaa, numab, numac, nbmaco
    integer :: ndime, jtmdim
    character(len=19) :: mai
    character(len=8) :: k8b, typma
! ----------------------------------------------------------------------
!
    call jemarq()
!
    mai=noma//'.TYPMAIL'
    call jeveuo(mai, 'L', jma)
    call jeveuo('&CATA.TM.TMDIM', 'L', jtmdim)
!
!     RECUPERATION DES MAILLES CONTENANT LE NOEUD A
    call jelira(jexnum(cnxinv, nunoa), 'LONMAX', nmanoa, k8b)
    call jeveuo(jexnum(cnxinv, nunoa), 'L', jmanoa)
!
!     RECUPERATION DES MAILLES CONTENANT LE NOEUD B
    call jelira(jexnum(cnxinv, nunob), 'LONMAX', nmanob, k8b)
    call jeveuo(jexnum(cnxinv, nunob), 'L', jmanob)
!
!     RECUPERATION DES MAILLES CONTENANT LE NOEUD C
    call jelira(jexnum(cnxinv, nunoc), 'LONMAX', nmanoc, k8b)
    call jeveuo(jexnum(cnxinv, nunoc), 'L', jmanoc)
!
!     ON COMPTE LE NBRE DE MAILLES VOLUMIQUES COMMUNES AUX 3 NOEUDS :
    nbmaco=0
!
!     BOUCLE SUR LES MAILLES CONTENANT LE NOEUD A
    do 100 imaa = 1, nmanoa
        numaa = zi(jmanoa-1+imaa)
!
        itypma = zi(jma-1+numaa)
        call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
!
!       NDIME : DIMENSION TOPOLOGIQUE DE LA MAILLE
        ndime= zi(jtmdim-1+itypma)
!
!       SI MAILLE NON VOLUMIQUE ON CONTINUE À 100
        if (ndime .ne. 3) goto 100
!
!       BOUCLE SUR LES MAILLES CONTENANT LE NOEUD B
        do 110 imab = 1, nmanob
            numab = zi(jmanob-1+imab)
!
            itypma = zi(jma-1+numab)
            call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
!
            ndime= zi(jtmdim-1+itypma)
!
!         SI MAILLE NON VOLUMIQUE ON CONTINUE À 110
            if (ndime .ne. 3) goto 110
!
!         SI LA MAILLE EST EN COMMUN AUX NOEUDS A ET B,
!         ON BOUCLE SUR LES MAILLES CONTENANT LE NOEUD C
            if (numaa .eq. numab) then
                do 120 imac = 1, nmanoc
                    numac = zi(jmanoc-1+imac)
!
                    itypma = zi(jma-1+numac)
                    call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
!
                    ndime= zi(jtmdim-1+itypma)
!
!             SI MAILLE NON VOLUMIQUE ON CONTINUE À 120
                    if (ndime .ne. 3) goto 120
!
!             SI LA MAILLE EST EN COMMUN AUX NOEUDS B ET C (ET A),
!             ON A DECOUVERT UNE MAILLE QUE LES TROIS NOEUDS
!             ONT EN COMMUN
                    if (numab .eq. numac) nbmaco=nbmaco+1
!
120              continue
            endif
110      continue
100  end do
!
    call assert(nbmaco.gt.0)
!
    fabord=.false.
    if (nbmaco .eq. 1) fabord=.true.
!
    call jedema()
end subroutine
