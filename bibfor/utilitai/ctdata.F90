subroutine ctdata(mesnoe, mesmai, nkcha, tych, toucmp,&
                  nkcmp, nkvari, nbcmp, ndim, chpgs, noma,&
                  nbno, nbma, nbval, tsca)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cesvar.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlim1.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/megeom.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/varinonu.h"
#include "asterfort/wkvect.h"

    integer :: nbcmp, ndim, nbno, nbma, nbval
    character(len=1) :: tsca
    character(len=4) :: tych
    character(len=8) :: noma
    character(len=24) :: mesnoe, mesmai, nkcha, nkvari, nkcmp
    character(len=19) :: chpgs
    aster_logical :: toucmp
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ----- OPERATEUR CREA_TABLE , MOT-CLE FACTEUR RESU   --------------
!
!        BUT : RECUPERER LES DONNEES UTILES POUR CONSTRUIRE LA TABLE
!              (COMPOSANTES,NOEUDS,MAILLES,...)
!
!        IN     : NKCHA  (K24) : OBJET DES NOMS DE CHAMP
!                 NBVAL (I)    : NOMBRE DE VALEURS D'ACCES
!        IN/OUT : MESNOE (K24) : OBJET DES NOMS DE NOEUD
!                 MESMAI (K24) : OBJET DES NOMS DE MAILLE
!                 NKCMP  (K24) : OBJET DES NOMS DE COMPOSANTES  (NOM_CMP)
!                 NKVARI (K24) : OBJET DES NOMS DE VAR. INTERNES (NOM_VARI)
!                 NCHSPG (K24) : NOM DU CHAM_ELEM_S DES COORDONNES DES
!                                POINTS DE GAUSS (REMPLI SI TYCH='ELGA')
!        OUT    : TYCH   (K4)  : TYPE DE CHAMP (=NOEU,ELXX,CART)
!                 TOUCMP (L)   : INDIQUE SI TOUT_CMP EST RENSEIGNE
!                 NBCMP  (I)   : NOMBRE DE COMPOSANTES LORSQUE
!                                NOM_CMP EST RENSEIGNE, 0 SINON
!                 NDIM   (I)   : DIMENSION GEOMETRIQUE (=2 OU 3)
!                 NOMA   (K8)  : NOM DU MAILLAGE
!                 NBNO   (I)   : NOMBRE DE NOEUDS UTILISATEUR
!                 NBMA   (I)   : NOMBRE DE MAILLES UTILISATEUR
!                 TSCA  (K1)  : TYPE DE LA GRANDEUR (REEL)
!
! ----------------------------------------------------------------------
    integer :: jkcha, i, ibid, iret, jlno, jcmp, n1, jlma, n2, n3, nchi, n0, n4
    integer :: n5, igrel,jcmp16
    integer, pointer :: repe(:) => null()
    character(len=8) :: nomo, nomgd, noca
    character(len=8) :: typmcl(4), lpain(6), lpaout(1),sdresu=' '
    character(len=16) :: motcle(4),nocham
    character(len=19) :: ligrel, ligrmo
    character(len=24) :: chgeom, lchin(6), lchout(1)
    aster_logical :: exicar
!     ------------------------------------------------------------------
!
    call jemarq()
!
!
!  --- 1. DETERMINATION DU TYPE DE CHAMP
!
    call jeveuo(nkcha, 'L', jkcha)
    tych=' '
    ligrel = '&&CTDATA.LIGREL'
    nomo=' '
    tsca=' '
    exicar=.false.
    call getvid('RESU', 'RESULTAT', iocc=1, scal=sdresu, nbret=n0)
    call getvid('RESU', 'CHAM_GD', iocc=1, nbval=0, nbret=n4)
    do i = 1, nbval
        if (zk24(jkcha+i-1)(1:18) .ne. '&&CHAMP_INEXISTANT') then
            call dismoi('TYPE_CHAMP', zk24(jkcha+i-1)(1:19), 'CHAMP', repk=tych)
            call dismoi('NOM_MAILLA', zk24(jkcha+i-1)(1:19), 'CHAMP', repk=noma)
            call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
            call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
            call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)
            call dismoi('NOM_GD', zk24(jkcha+i-1)(1:19), 'CHAMP', repk=nomgd)
            call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
            if (tsca .ne. 'R') then
                call utmess('F', 'TABLE0_42')
            endif
            if (tych(1:2) .eq. 'EL') then
                call dismoi('NOM_MODELE', zk24(jkcha+i-1)(1:19), 'CHAMP', repk=nomo)
                call dismoi('NOM_LIGREL', nomo, 'MODELE', repk=ligrmo)
                call jeveuo(ligrmo//'.REPE', 'L', vi=repe)
            endif
            if (tych .eq. 'ELGA') then
!               CARACTERISTIQUES POUR LES CAS DES ELEMENTS A SOUS POINTS
                if (n0 .ne. 0) then
                    call dismoi('CARA_ELEM', zk24(jkcha+i-1)(1:8), 'RESULTAT', repk=noca,&
                                arret='C', ier=iret)
                    if (iret .eq. 0) exicar=.true.
                else if (n4.ne.0) then
                    call getvid('RESU', 'CARA_ELEM', iocc=1, scal=noca, nbret=n5)
                    if (n5 .ne. 0) exicar=.true.
                endif
!               DIMENSION MODELE POUR IMPRESSION COOR POINT GAUSS
                call dismoi('DIM_GEOM', nomo, 'MODELE', repi=ibid)
                ndim=ibid
                if (ibid .ge. 100) then
                    ibid = ibid - 100
                    ndim=1
                endif
                if (ibid .ge. 20) then
                    ibid = ibid - 20
                    ndim=2
                endif
                if (ibid .eq. 3) then
                    ndim=3
                endif
            endif
            goto 61
        endif
    end do
 61 continue
!
!  --- 2. RECUPERATION DES NOEUDS,MAILLES
!
    if (tych .eq. 'NOEU') then
!
        motcle(1) = 'NOEUD'
        motcle(2) = 'GROUP_NO'
        motcle(3) = 'MAILLE'
        motcle(4) = 'GROUP_MA'
        typmcl(1) = 'NOEUD'
        typmcl(2) = 'GROUP_NO'
        typmcl(3) = 'MAILLE'
        typmcl(4) = 'GROUP_MA'
        call getvtx('RESU', 'TOUT', iocc=1, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call wkvect(mesnoe, 'V V I', nbno, jlno)
            do i = 1, nbno
                zi(jlno+i-1)=i
            end do
        else
            call reliem(' ', noma, 'NU_NOEUD', 'RESU', 1,&
                        4, motcle, typmcl, mesnoe, nbno)
            call jeveuo(mesnoe, 'L', jlno)
        endif
        nbma=0
!
    else if (tych(1:2).eq.'EL'.or.tych.eq.'CART') then
!
!          VERIFICATIONS
        call getvtx('RESU', 'NOEUD', iocc=1, nbval=0, nbret=n1)
        call getvtx('RESU', 'GROUP_NO', iocc=1, nbval=0, nbret=n2)
        n3=-n1-n2
        if (n3 .ne. 0) then
            call utmess('F', 'TABLE0_41')
        endif
!
        motcle(1) = 'MAILLE'
        motcle(2) = 'GROUP_MA'
        typmcl(1) = 'MAILLE'
        typmcl(2) = 'GROUP_MA'
        call getvtx('RESU', 'TOUT', iocc=1, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call wkvect(mesmai, 'V V I', nbma, jlma)
            if (tych.eq.'CART') then
                do i = 1, nbma
                    zi(jlma+i-1)=i
                end do
            else
!               -- on ne garde que les mailles du modele :
                do i = 1, nbma
                    igrel = repe(1+2*(i-1))
                    if (igrel.gt.0) zi(jlma+i-1)=i
                end do
            endif
        else
            call reliem(' ', noma, 'NU_MAILLE', 'RESU', 1,&
                        2, motcle, typmcl, mesmai, nbma)
        endif
        nbno=0
!
        if (tych .eq. 'ELGA') then

!           -- calcul de ligrel :
!           ---------------------
            call jeveuo(mesmai, 'L', jlma)
            call jelira(mesmai, 'LONMAX', nbma)
            call exlim1(zi(jlma), nbma, nomo, 'V', ligrel)
!
            call megeom(nomo, chgeom)
            lchin(1)=chgeom(1:19)
            lpain(1)='PGEOMER'
            nchi=1
            if (exicar) then
                nchi=6
                lchin(2)=noca//'.CARORIEN'
                lpain(2)='PCAORIE'
                lchin(3)=noca//'.CAFIBR'
                lpain(3)='PFIBRES'
                lchin(4)=noca//'.CANBSP'
                lpain(4)='PNBSP_I'
                lchin(5)=noca//'.CARCOQUE'
                lpain(5)='PCACOQU'
                lchin(6)=noca//'.CARGEOPO'
                lpain(6)='PCAGEPO'
                lchout(1)='&&CTDATA.PGCOOR'
                lpaout(1)='PCOORPG'
                call cesvar(noca, ' ', ligrel, lchout(1))
            else
                lchout(1)='&&CTDATA.PGCOOR'
                lpaout(1)='PCOORPG'
            endif
!
            call calcul('S', 'COOR_ELGA', ligrel, nchi, lchin,&
                        lpain, 1, lchout, lpaout, 'V',&
                        'OUI')
            call celces(lchout(1), 'V', chpgs)
            call detrsd('LIGREL', ligrel)
!
        endif
!
    endif
!
!  --- 3. RECUPERATION DES COMPOSANTES
!
    call getvtx('RESU', 'TOUT_CMP', iocc=1, nbval=0, nbret=n1)
    if (n1.ne.0) then
        nbcmp=0
        toucmp=.true.
        call wkvect(nkcmp, 'V V K8', 1, jcmp)
        zk8(jcmp)=' '
    else
        toucmp=.false.
        call getvtx('RESU', 'NOM_CMP', iocc=1, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            nbcmp=-n1
            call wkvect(nkcmp, 'V V K8', nbcmp, jcmp)
            call getvtx('RESU', 'NOM_CMP', iocc=1, nbval=nbcmp, vect=zk8(jcmp),&
                        nbret=n1)
        else
            call getvtx('RESU', 'NOM_VARI', iocc=1, nbval=0, nbret=n1)
            nbcmp=-n1
            ASSERT(nbcmp.gt.0)
            call wkvect(nkcmp, 'V V K8', nbcmp, jcmp)
            call wkvect(nkvari, 'V V K16', nbcmp, jcmp16)
            call getvtx('RESU', 'NOM_VARI', iocc=1, nbval=nbcmp, vect=zk16(jcmp16),&
                         nbret=n1)
            if (sdresu.eq.' ') then
                call utmess('F', 'EXTRACTION_24')
            endif
            call getvtx('RESU', 'NOM_CHAM', iocc=1, scal=nocham, nbret=n1)
            if (nocham(1:7).ne.'VARI_EL') then
                call utmess('F', 'EXTRACTION_25',sk=nocham)
            endif
            ASSERT(nbma.gt.0)
            call varinonu(' ', sdresu, nbma, zi(jlma), nbcmp, zk16(jcmp16), zk8(jcmp))
        endif
    endif
!
    call jedema()
!
end subroutine
