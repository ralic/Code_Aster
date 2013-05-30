subroutine fonno1(noma, cnxinv, ndim, na, nb,&
                  nbmac, macofo)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    integer :: na, nb, ndim, nbmac
    character(len=8) :: noma
    character(len=19) :: macofo, cnxinv
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
!       ----------------------------------------------------------------
!       RECUPERATION DES NUMEROS DES MAILLES CONNECTEES AU
!       SEGMENT DU FOND -> REMPLISSAGE DU VECTEUR &&FONNOR.MACOFOND
!       ----------------------------------------------------------------
!    ENTREES
!       NOMA   : NOM DU MAILLAGE
!       CNXINV : CONNECTIVITE INVERSE
!       NDIM   : DIMENSION DU MODELE
!       NA     : NUMERO DU NOEUD SOMMET COURANT
!       NB     : NUMERO DU NOEUD SOMMET SUIVANT
!    SORTIE
!       MACOFO : VECTEUR DES MAILLES (PRINCIPALES) CONNECTEES AU SEGMENT
!                DU FOND DE FISSURE COURANT
!       NBMAC  : NOMBRE DE MAILLES REMPLIES DANS MACOFO
!
!
    integer :: jdrvlc, iatyma, jmaco, iamase, jcncin, ityp
    integer :: nbmaca, adra, iret, comp1, ima, numac, ino1, ndime, nn
    character(len=8) :: k8b, type
!     -----------------------------------------------------------------
!
    call jemarq()
!
!     RECUPERATION DE LA CONNECTIVITE INVERSE
    call jeveuo(jexatr(cnxinv, 'LONCUM'), 'L', jdrvlc)
    call jeveuo(jexnum(cnxinv, 1), 'L', jcncin)
!
!
!     RECUPERATION DE L'ADRESSE DES TYPFON DE MAILLES
    call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
!
!     MAILLES CONNECTEES A NA
    adra = zi(jdrvlc-1 + na)
    nbmaca = zi(jdrvlc-1 + na+1) - zi(jdrvlc-1 + na)
!
!     ALLOCATION DU VECTEUR DES MAILLES CONNECTEES AU SEGMENT DU FOND
    call wkvect(macofo, 'V V I', nbmaca, jmaco)
!
    comp1=0
    do 10 ima = 1, nbmaca
!       NUMERO DE LA MAILLE
        numac = zi(jcncin-1 + adra+ima-1)
        ityp = iatyma-1+numac
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(ityp)), type)
        call dismoi('F', 'DIM_TOPO', type, 'TYPE_MAILLE', ndime,&
                    k8b, iret)
!       ON ZAPPE LES MAILLES DE BORDS
        if (ndime .ne. ndim) goto 10
!
        if (ndim .eq. 2) then
            comp1=comp1+1
            zi(jmaco-1+comp1)=numac
        else if (ndim.eq.3) then
!         EN 3D ON DOIT AVOIR AUSSI LE NOEUD NB
            call jeveuo(jexnum(noma//'.CONNEX', numac), 'L', iamase)
            call dismoi('F', 'NBNO_TYPMAIL', type, 'TYPE_MAILLE', nn,&
                        k8b, iret)
            do 100 ino1 = 1, nn
                if (zi(iamase-1 + ino1) .eq. nb) then
                    comp1=comp1+1
                    zi(jmaco-1+comp1)=numac
                endif
100          continue
        endif
10  end do
!
!     NB MAILLES CONNECTEES AU SEGMENT
    nbmac = comp1
    call jedema()
end subroutine
