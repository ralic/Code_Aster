subroutine xtmafi(noma, ndim, fiss, nfiss, lismai,&
                  mesmai, nbma)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! TOLE CRS_1404
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    integer :: nfiss, nbma, ndim
    character(len=8) :: noma, fiss(nfiss)
    character(len=24) :: lismai, mesmai
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM
!
!  CREATION DE LA LISTE TOTALE DES NOMS DES MAILLES FISSUREES
!  (MAILLES HEAV + CTIP + HECT) POUR LES FISSURES CONTENUES
!  DANS LA LISTE FISS
!  SI NDIM = 0 : ON LES PREND TOUTES
!  SI NDIM NON NUL : ON NE PREND QUE LES MAILLES DE DIMENSION NDIM
!
! ----------------------------------------------------------------------
!
! IN     NOMA   : NOM DU MAILLAGE
! IN     NDIM   : DIMENSION DES MAILLES A LISTER
! IN     FISS   : LISTE DES NOMS DES SD FISS_XFEM
! IN     NFISS  : LONGUEUR DE FISS
! IN/OUT LISMAI : NOM DE LA LISTE CREEE CONTENANT LES NUMEROS DE MAILLES
! IN/OUT MESMAI : NOM DE LA LISTE CREEE CONTENANT LES NOMS DES MAILLES
! OUT    NBMA   : LONGUEUR DE MESMAI
!
!
!
!
    integer :: ifiss, kk, jgrp, nmaenr, i, ima, jma, cpt, iret
    integer :: jtmdim, jtypma, ndime, jmad, jmai, mxstac
    character(len=8) :: nomail, k8bid
    character(len=24) :: nommai, grp(nfiss, 3)
!
    parameter (mxstac=100)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     VERIF QUE LES TABLEAUX LOCAUX DYNAMIQUES NE SONT PAS TROP GRANDS
!     (VOIR CRS 1404)
    call assert(nfiss.le.mxstac)
!
    nommai = noma//'.NOMMAI'
    call jeveuo('&CATA.TM.TMDIM', 'L', jtmdim)
    call jeveuo(noma//'.TYPMAIL', 'L', jtypma)
!
!     DIMENTIONNEMENT GROSSIER DE LA LISTE
    cpt = 0
    do 10 ifiss = 1, nfiss
        grp(ifiss,1) = fiss(ifiss)//'.MAILFISS.HEAV'
        grp(ifiss,2) = fiss(ifiss)//'.MAILFISS.CTIP'
        grp(ifiss,3) = fiss(ifiss)//'.MAILFISS.HECT'
        do 20 kk = 1, 3
            call jeexin(grp(ifiss, kk), iret)
            if (iret .eq. 0) goto 20
            call jelira(grp(ifiss, kk), 'LONMAX', nmaenr, k8bid)
            cpt = cpt + nmaenr
20      continue
10  end do
!
!     CREATION DE LA LISTE TEMPORAIRE
    call wkvect('&&XTMAFI.TEMP', 'V V K8', cpt, jma)
!     CREATION DE LA LISTE TEMPORAIRE
    call wkvect('&&XTMAFI.TEMI', 'V V I', cpt, jmai)
!
!     REMPLISSAGE DE LA LISTE
    nbma = 0
    do 100 ifiss = 1, nfiss
!
!
!       BOUCLE SUR LES 3 GROUPES : HEAV, CTIP ET HECT
        do 110 kk = 1, 3
!
            call jeexin(grp(ifiss, kk), iret)
            if (iret .ne. 0) then
                call jeveuo(grp(ifiss, kk), 'L', jgrp)
                call jelira(grp(ifiss, kk), 'LONMAX', nmaenr, k8bid)
!
!           BOUCLE SUR LES MAILLES DE CHAQUE GROUPE
                do 120 i = 1, nmaenr
                    ima = zi(jgrp-1+i)
!             NDIME : DIMENSION TOPOLOGIQUE DE LA MAILLE
                    ndime= zi(jtmdim-1+zi(jtypma-1+ima))
                    if ((ndim.eq.ndime) .or. (ndim.eq.0)) then
                        call jenuno(jexnum(nommai, ima), nomail)
                        nbma =nbma + 1
                        zk8(jma-1+nbma) = nomail
                        zi(jmai-1+nbma) = ima
                    endif
120              continue
!
            endif
!
110      continue
!
100  end do
!
!     VERIFICATION
    call assert(nbma.le.cpt)
    call assert(nbma.ge.1)
!
!     CREATION DE LA LISTE DEFINITIVE
    call wkvect(mesmai, 'V V K8', nbma, jmad)
    do 200 i = 1, nbma
        zk8(jmad-1+i) = zk8(jma-1+i)
200  end do
!
!     CREATION DE LA LISTE DEFINITIVE
    call wkvect(lismai, 'V V I', nbma, jmad)
    do 201 i = 1, nbma
        zi(jmad-1+i) = zi(jmai-1+i)
201  end do
!
    call jedetr('&&XTMAFI.TEMP')
    call jedetr('&&XTMAFI.TEMI')
!
    call jedema()
end subroutine
