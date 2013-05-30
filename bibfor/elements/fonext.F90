subroutine fonext(noma, cnxinv, jbasno, inoext, inoseg,&
                  nbnoff, jborl, jdirol, jnvdir, iseg)
!
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/cengra.h'
    include 'asterfort/confac.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/normev.h'
    include 'asterfort/xextre.h'
    include 'asterfort/xfabor.h'
    include 'asterfort/xnorme.h'
    integer :: jbasno, inoext, inoseg, nbnoff, jborl, jdirol
    integer :: jnvdir, iseg
    character(len=8) :: noma
    character(len=19) :: cnxinv
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
! FONCTION REALISEE:
!
!     CALCUL DU VECTEUR DE DIRECTION DE PROPAGATION AUX EXTREMITES DU
!     FOND DE FISSURE
!
!     ENTREES:
!        NOMA   : NOM DU MAILLAGE
!        CNXINV : CONNECTIVITE INVERSE
!        INOEXT : INDICE DU NOEUD EXTREMITE DU FOND
!        INOSEG : INDICE DU NOEUD APPARTENANT AU MEME SEGMENT DU FOND
!                 QUE INOEXT
!        NBNOFF : NOMBRE DE NOEUDS AU FOND DE FISSURE
!        JBORL  : ADRESSE DU VECTEUR PERMETTANT DE SAVOIR SI LE VECTEUR
!                  DE DIRECTION DE PROPAGATION A DEJA ETE RECALCULE OU
!                  NON AUX POINTS EXTREMITES DU FOND (POUR SAVOIR SI ON
!                  DOIT REMPLACER LA VALEUR EXISTANTE OU LA LUI AJOUTER)
!        JDIROL : ADRESSE DES VECTEURS DIRECTIONS DE PROPAGATION
!                  INITIAUX (CAD SANS MODIFICATION DES VECTEURS AUX
!                  POINTS EXTREMITES DU FOND)
!        JNVDIR : ADRESSE DU VECTEUR CONTENANT 0 OU 1 AUX POINTS
!                  EXTREMITES DU FOND:
!                  0: LE PRODUIT SCALAIRE ENTRE LA NORMALE A LA FACE DE
!                     BORD ET LE VDIR INITIAL ESI INFERIEUR A 0
!                  1: LE PRODUIT SCALAIRE EST SUPERIEUR OU EGAL A 0
!        ISEG   : INDICE DU SEGMENT DU FOND
!
!     ENTREE/SORTIE:
!        JBASNO : BASE LOCALE AUX NOEUDS DU FOND
!
!-----------------------------------------------------------------------
!
    integer :: ibid, ifa, ima, ino, iret, itypma
    integer :: jconx1, jconx2, jcoor, jma, jmanoe
    integer :: nbf, nbfacb, nbno, ndime, nmaext, nmanoe, nuno
    integer :: nunoa, nunob, nunoc, numpt
    integer :: ibid3(12, 3), inobor(2), fa(6, 4)
    real(kind=8) :: coorg(3), vectn(12), norme, vect(3), proj
    character(len=8) :: k8b, typma
    logical :: fabord, nofac
!     -----------------------------------------------------------------
!
    call jemarq()
    numpt=1
    if (iseg .ne. 1) numpt=nbnoff
!
!     RECUPERATION DES INFORMATIONS RELATIVES AU MAILLAGE
!
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    call jeveuo(noma//'.TYPMAIL', 'L', jma)
!
!     RECUPERATION DES MAILLES CONTENANT LE NOEUD EXTREMITE DU FOND
    call jelira(jexnum(cnxinv, inoext), 'LONMAX', nmanoe, k8b)
    call jeveuo(jexnum(cnxinv, inoext), 'L', jmanoe)
!
!     BOUCLE SUR LE NOMBRE DE MAILLES CONNECTEES AU NOEUD EXTREMITE
    do 1000 ima = 1, nmanoe
        nmaext=zi(jmanoe-1+(ima-1)+1)
!
        itypma=zi(jma-1+nmaext)
        call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
!
        call dismoi('F', 'DIM_TOPO', typma, 'TYPE_MAILLE', ndime,&
                    k8b, iret)
!
!       ON NE PREND QUE LES MAILLES EN 3D
        if (ndime .ne. 3) goto 1000
!       CALCUL DU CENTRE DE GRAVITE DE LA MAILLE
        call cengra(noma, nmaext, coorg)
        call confac(typma, ibid3, ibid, fa, nbf)
        nbfacb=0
        inobor(1)=0
        inobor(2)=0
!       BOUCLE SUR LE NOMBRE DE FACES DE LA MAILLE
        do 1100 ifa = 1, nbf
            nbno = 4
            if (fa(ifa,4) .eq. 0) nbno = 3
            nofac=.false.
!         BOUCLE SUR LE NOMBRE DE NOEUDS DE LA FACE
            do 1110 ino = 1, nbno
                nuno = zi(jconx1-1+zi(jconx2+nmaext-1)+fa(ifa,ino)-1)
                if (nuno .eq. inoseg) goto 1100
                if (nuno .eq. inoext) nofac=.true.
!         FIN BOUCLE SUR LES NOEUDS
1110          continue
!
            if (nofac) then
                nunoa = zi(jconx1-1+zi(jconx2+nmaext-1)+fa(ifa,1)-1)
                nunob = zi(jconx1-1+zi(jconx2+nmaext-1)+fa(ifa,2)-1)
                nunoc = zi(jconx1-1+zi(jconx2+nmaext-1)+fa(ifa,3)-1)
!
!           ON VERIFIE SI LA FACE COURANTE EST UNE FACE DE BORD
                call xfabor(noma, cnxinv, nunoa, nunob, nunoc,&
                            fabord)
                if (fabord) then
                    call xnorme(numpt, inobor, vectn, nbfacb, nunoa,&
                                nunob, nunoc, jcoor, coorg)
!             ON VERIFIE QUE LA NORMALE A LA FACE N'EST PAS
!             COLINEAIRE AU VECTEUR NORMAL AU PLAN DE FISSURE
                    vect(1)=vectn(1+3*(nbfacb-1))
                    vect(2)=vectn(2+3*(nbfacb-1))
                    vect(3)=vectn(3+3*(nbfacb-1))
                    call normev(vect, norme)
!
                    proj=vect(1)*zr(jbasno-1+6*(numpt-1)+1) +vect(2)*&
                    zr(jbasno-1+6*(numpt-1)+2) +vect(3)*zr(jbasno-1+6*&
                    (numpt-1)+3)
!
                    if (abs(proj) .ge. 0.95d0) then
                        nbfacb = nbfacb-1
                        goto 1100
                    endif
!
                endif
            endif
!       FIN BOUCLE SUR LES FACES
1100      continue
        if (nbfacb .ne. 0) then
            call xextre(inobor, vectn, nbfacb, jbasno, jborl,&
                        jdirol, jnvdir)
        endif
!     FIN BOUCLE SUR LES MAILLES
1000  end do
!
    call normev(zr(jbasno-1+6*(numpt-1)+4), norme)
!
    call jedema()
end subroutine
