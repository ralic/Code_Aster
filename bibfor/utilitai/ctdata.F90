subroutine ctdata(mesnoe, mesmai, nkcha, tych, toucmp,&
                  nkcmp, nbcmp, ndim, chpgs, noma,&
                  nbno, nbma, nbval, tsca)
    implicit none
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cesvar.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/megeom.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: nbcmp, ndim, nbno, nbma, nbval
    character(len=1) :: tsca
    character(len=4) :: tych
    character(len=8) :: noma
    character(len=24) :: mesnoe, mesmai, nkcha, nkcmp
    character(len=19) :: chpgs
    logical :: toucmp
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
!     ----- OPERATEUR CREA_TABLE , MOT-CLE FACTEUR RESU   --------------
!
!        BUT : RECUPERER LES DONNEES UTILES POUR CONSTRUIRE LA TABLE
!              (COMPOSANTES,NOEUDS,MAILLES,...)
!
!        IN     : NKCHA  (K24) : OBJET DES NOMS DE CHAMP
!                 NBVAL (I)    : NOMBRE DE VALEURS D'ACCES
!        IN/OUT : MESNOE (K24) : OBJET DES NOMS DE NOEUD
!                 MESMAI (K24) : OBJET DES NOMS DE MAILLE
!                 NKCMP  (K24) : OBJET DES NOMS DE COMPOSANTES
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
    character(len=8) :: k8b
    integer :: jkcha, i, ibid, iret, jlno, jcmp, n1, jlma, n2, n3, nchi, n0, n4
    integer :: n5
    character(len=8) :: nomo, nomgd, noca
    character(len=8) :: typmcl(4), lpain(6), lpaout(1)
    character(len=16) :: motcle(4)
    character(len=19) :: ligrel
    character(len=24) :: chgeom, lchin(6), lchout(1)
    logical :: exicar
!     ------------------------------------------------------------------
!
    call jemarq()
!
!
!  --- 1. DETERMINATION DU TYPE DE CHAMP
!
    call jeveuo(nkcha, 'L', jkcha)
    tych=' '
    ligrel = ' '
    nomo=' '
    tsca=' '
    exicar=.false.
    call getvid('RESU', 'RESULTAT', iocc=1, nbval=0, nbret=n0)
    call getvid('RESU', 'CHAM_GD', iocc=1, nbval=0, nbret=n4)
    do 60 i = 1, nbval
        if (zk24(jkcha+i-1)(1:18) .ne. '&&CHAMP_INEXISTANT') then
            call dismoi('F', 'TYPE_CHAMP', zk24(jkcha+i-1)(1:19), 'CHAMP', ibid,&
                        tych, iret)
            call dismoi('F', 'NOM_MAILLA', zk24(jkcha+i-1)(1:19), 'CHAMP', ibid,&
                        noma, iret)
            call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbno,&
                        k8b, iret)
            call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                        k8b, iret)
            call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndim,&
                        k8b, iret)
            call dismoi('F', 'NOM_GD', zk24(jkcha+i-1)(1:19), 'CHAMP', ibid,&
                        nomgd, iret)
            call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                        tsca, ibid)
            if (tsca .ne. 'R') call u2mess('F', 'TABLE0_42')
            if (tych(1:2) .eq. 'EL') then
                call dismoi('F', 'NOM_MODELE', zk24(jkcha+i-1)(1:19), 'CHAMP', ibid,&
                            nomo, iret)
                ligrel=nomo//'.MODELE'
            endif
            if (tych .eq. 'ELGA') then
!               CARACTERISTIQUES POUR LES CAS DES ELEMENTS A SOUS POINTS
                if (n0 .ne. 0) then
                    call dismoi('C', 'CARA_ELEM', zk24(jkcha+i-1)(1:8), 'RESULTAT', ibid,&
                                noca, iret)
                    if (iret .eq. 0) exicar=.true.
                else if (n4.ne.0) then
                    call getvid('RESU', 'CARA_ELEM', iocc=1, scal=noca, nbret=n5)
                    if (n5 .ne. 0) exicar=.true.
                endif
!               DIMENSION MODELE POUR IMPRESSION COOR POINT GAUSS
                call dismoi('F', 'DIM_GEOM', nomo, 'MODELE', ibid,&
                            k8b, iret)
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
60  end do
61  continue
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
            do 70 i = 1, nbno
                zi(jlno+i-1)=i
70          continue
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
        if (n3 .ne. 0) call u2mess('F', 'TABLE0_41')
!
        motcle(1) = 'MAILLE'
        motcle(2) = 'GROUP_MA'
        typmcl(1) = 'MAILLE'
        typmcl(2) = 'GROUP_MA'
        call getvtx('RESU', 'TOUT', iocc=1, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call wkvect(mesmai, 'V V I', nbma, jlma)
            do 80 i = 1, nbma
                zi(jlma+i-1)=i
80          continue
        else
            call reliem(' ', noma, 'NU_MAILLE', 'RESU', 1,&
                        2, motcle, typmcl, mesmai, nbma)
            call jeveuo(mesmai, 'L', jlma)
        endif
        nbno=0
!
        if (tych .eq. 'ELGA') then
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
!
        endif
!
    endif
!
!  --- 3. RECUPERATION DES COMPOSANTES
!
    call getvtx('RESU', 'NOM_CMP', iocc=1, nbval=0, nbret=n1)
    if (n1 .ne. 0) then
        nbcmp=-n1
        toucmp=.false.
        call wkvect(nkcmp, 'V V K8', nbcmp, jcmp)
        call getvtx('RESU', 'NOM_CMP', iocc=1, nbval=nbcmp, vect=zk8(jcmp),&
                    nbret=n1)
    else
        nbcmp=0
        toucmp=.true.
        call wkvect(nkcmp, 'V V K8', 1, jcmp)
        zk8(jcmp)=' '
    endif
!
    call jedema()
!
end subroutine
