subroutine ctnotb(nbno, mesnoe, noma, nbval, nkcha,&
                  nkcmp, toucmp, nbcmp, typac, ndim,&
                  nrval, resu, nomtb, nsymb, nival,&
                  niord)
    implicit   none
#include "jeveux.h"
!
#include "asterc/indik8.h"
#include "asterfort/cnocns.h"
#include "asterfort/indiis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/tbajli.h"
#include "asterfort/wkvect.h"
    integer :: nbcmp, nbno, ndim, nbval
    character(len=8) :: typac, noma, resu, nomtb
    character(len=16) :: nsymb
    character(len=24) :: nkcha, nkcmp, mesnoe, nival, nrval, niord
    logical :: toucmp
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
!     ----- OPERATEUR CREA_TABLE , MOT-CLE FACTEUR RESU   --------------
!
!        BUT : REMPLISSAGE DE LA TABLE POUR UN CHAM_NO
!
!        IN     : NKCHA (K24)  : OBJET DES NOMS DE CHAMP
!                 RESU  (K8)   : NOM DU RESULTAT (SI RESULTAT,SINON ' ')
!                 NKCMP  (K24) : OBJET DES NOMS DE COMPOSANTES
!                 TOUCMP (L)   : INDIQUE SI TOUT_CMP EST RENSEIGNE
!                 NBCMP (I)    : NOMBRE DE COMPOSANTES LORSQUE
!                                NOM_CMP EST RENSEIGNE, 0 SINON
!                 TYPAC (K8)   : ACCES (ORDRE,MODE,FREQ,INST)
!                 NBVAL (I)    : NOMBRE DE VALEURS D'ACCES
!                 NOMA   (K8)  : NOM DU MAILLAGE
!                 MESNOE (K24) : OBJET DES NOMS DE NOEUD
!                 NRVAL (K16)  : OBJET DES VALEURS D'ACCES (REELS)
!                 NIVAL (K16)  : OBJET DES VALEURS D'ACCES (ENTIERS)
!                 NIORD (K16)  : NOM D'OBJET DES NUMEROS D'ORDRE
!                 NSYMB (K16)  : NOM SYMBOLIQUE DU CHAMP
!                 NBNO  (I)    : NOMBRE DE NOEUDS UTILISATEURS
!
!        IN/OUT : NOMTB (K24)  : OBJET TABLE
!
! ----------------------------------------------------------------------
!
    integer :: jcmp, jkcha, jlno, jrval, jival, jniord, jcoor, i, jcnsv, nbnox
    integer :: jcnsl, jcnsd, jcnsc, nbcmpx, n, jval, jkval, ino, indno
    integer :: kcp, icmp, indcmp, ni, nr, nk, ji, jr, jk, kk, nbpara
    integer :: jparak, j, ibid
    complex(kind=8) :: cbid
    character(len=8) :: kno
    character(len=19) :: chamns
!     ------------------------------------------------------------------
!
    call jemarq()
!
! --- 0. INITIALISATIONS
    chamns = '&&CTNOTB.CNS       '
    call jeveuo(nkcmp, 'L', jcmp)
    call jeveuo(nkcha, 'L', jkcha)
    call jeveuo(mesnoe, 'L', jlno)
    call jeveuo(nrval, 'L', jrval)
    call jeveuo(nival, 'L', jival)
    call jeveuo(niord, 'L', jniord)
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
!     TABLEAU DES VALEURS ENTIERES DE LA TABLE: ZI(JI)
!     TABLEAU DES VALEURS REELES DE LA TABLE: ZR(JR)
!     TABLEAU DES VALEURS CARACTERES DE LA TABLE: ZK16(JK)
!     POUR DES RAISONS DE PERF, CES TABLEAUX ONT ETE SORTIS DE
!     LA BOUCLE, D'OU DES DIMENSIONS EN DUR (NOMBRE SUFFISANT)
    call wkvect('&&CTNOTB.TABLE_VALR', 'V V R', 50, jr)
    call wkvect('&&CTNOTB.TABLE_VALI', 'V V I', 50, ji)
    call wkvect('&&CTNOTB.TABLE_VALK', 'V V K16', 50, jk)
!
    do 100 i = 1, nbval
!
        if (zk24(jkcha+i-1)(1:18) .ne. '&&CHAMP_INEXISTANT') then
!
!
!            -- PASSAGE CHAM_NO => CHAM_NO_S
            call cnocns(zk24(jkcha+i-1), 'V', chamns)
            call jeveuo(chamns//'.CNSV', 'L', jcnsv)
            call jeveuo(chamns//'.CNSL', 'L', jcnsl)
            call jeveuo(chamns//'.CNSD', 'L', jcnsd)
            call jeveuo(chamns//'.CNSC', 'L', jcnsc)
!
!             NOMBRE DE NOEUDS MAX DU CHAMP: NBNOX
            nbnox=zi(jcnsd)
!             NOMBRE DE COMPOSANTES MAX DU CHAMP : NBCMPX
            nbcmpx=zi(jcnsd+1)
!
!             NOMBRE DE COMPOSANTES DESIREES : N
            if (toucmp) then
                n=nbcmpx
            else
                n=nbcmp
            endif
!
!             TABLEAU DES VALEURS DES COMPOSANTES DESIREES: ZR(JVAL)
            call jedetr('&&CTNOTB.VAL_CMP')
            call wkvect('&&CTNOTB.VAL_CMP', 'V V R', n, jval)
!
!             TABLEAU DES NOMS DE COMPOSANTES DESIREES : ZK8(JKVAL)
            call jedetr('&&CTNOTB.NOM_CMP')
            call wkvect('&&CTNOTB.NOM_CMP', 'V V K8', n, jkval)
!
!             -- ON PARCOURT LES NOEUDS MAX,
            do 110 ino = 1, nbnox
!
!               - SI LE NOEUD FAIT PARTIE DES NOEUDS DESIRES,
!               ON POURSUIT, SINON ON VA AU NOEUD SUIVANT:
                indno=indiis(zi(jlno),ino,1,nbno)
                if (indno .eq. 0) goto 110
                kcp=0
!
!              - ON PARCOURT LES COMPOSANTES:
                do 120 icmp = 1, nbcmpx
                    if (.not.toucmp) then
!                    -SI LA COMPOSANTE FAIT PARTIE DES COMPOSANTES
!                     DESIREES, ON POURSUIT, SINON ON VA A LA
!                     COMPOSANTE SUIVANTE
                        indcmp=indik8(zk8(jcmp),zk8(jcnsc+icmp-1),&
                        1,nbcmp)
                        if (indcmp .eq. 0) goto 120
                    endif
!                  - SI LE CHAMP A UNE VALEUR, ON POURSUIT ET ON
!                  STOCKE LE NOM ET LA VALEUR DE COMPOSANTE :
                    if (.not.zl(jcnsl+nbcmpx*(ino-1)+icmp-1)) goto 120
                    kcp=kcp+1
                    zr(jval+kcp-1)=zr(jcnsv+nbcmpx*(ino-1)+icmp-1)
                    zk8(jkval+kcp-1)=zk8(jcnsc+icmp-1)
120              continue
!
!               SOIT NI LE NOMBRE DE VALEURS ENTIERES DE LA TABLE
!               SOIT NR LE NOMBRE DE VALEURS REELES DE LA TABLE
!               SOIT NK LE NOMBRE DE VALEURS CARACTERES DE LA TABLE
!
                nr=ndim+kcp
                ni=1
                nk=3
                if (resu .ne. ' ') then
                    if (typac .eq. 'FREQ' .or. typac .eq. 'INST') then
                        nr=nr+1
                    else if (typac.eq.'MODE') then
                        ni=ni+1
                    endif
                else
                    ni=0
                    nk=2
                endif
!
!
!               ON REMPLIT LES TABLEAUX ZI(JI),ZR(JR),ZK16(JK)
                kk=0
                if (typac .eq. 'FREQ' .or. typac .eq. 'INST') then
                    zr(jr+kk)=zr(jrval+i-1)
                    kk=kk+1
                endif
                do 121 j = 1, ndim
                    zr(jr+kk)=zr(jcoor+3*(ino-1)+j-1)
                    kk=kk+1
121              continue
                do 122 j = 1, kcp
                    zr(jr+kk)=zr(jval+j-1)
                    kk=kk+1
122              continue
!
                kk=0
                if (resu .eq. ' ') then
                    zk16(jk+kk)=zk24(jkcha+i-1)(1:16)
                    kk=kk+1
                else
                    zk16(jk+kk)=resu
                    kk=kk+1
                    zk16(jk+kk)=nsymb
                    kk=kk+1
                    zi(ji)=zi(jniord+i-1)
                    if (typac .eq. 'MODE') zi(ji+1)=zi(jival+i-1)
                endif
                call jenuno(jexnum(noma//'.NOMNOE', ino), kno)
                zk16(jk+kk)=kno
!
!
!               TABLEAU DES NOMS DE PARAMETRES DE LA TABLE: ZK16(JPARAK)
                nbpara=nr+ni+nk
                call jedetr('&&CTNOTB.TABLE_PARAK')
                call wkvect('&&CTNOTB.TABLE_PARAK', 'V V K16', nbpara, jparak)
!
!               ON REMPLIT ZK16(JPARAK)
                kk=0
                if (resu .eq. ' ') then
                    zk16(jparak+kk)='CHAM_GD'
                    kk=kk+1
                else
                    zk16(jparak+kk)='RESULTAT'
                    kk=kk+1
                    zk16(jparak+kk)='NOM_CHAM'
                    kk=kk+1
                    if (typac .ne. 'ORDRE') then
                        zk16(jparak+kk)=typac
                        kk=kk+1
                    endif
                    zk16(jparak+kk)='NUME_ORDRE'
                    kk=kk+1
                endif
                zk16(jparak+kk)='NOEUD'
                kk=kk+1
                zk16(jparak+kk)='COOR_X'
                kk=kk+1
                if (ndim .ge. 2) then
                    zk16(jparak+kk)='COOR_Y'
                    kk=kk+1
                endif
                if (ndim .eq. 3) then
                    zk16(jparak+kk)='COOR_Z'
                    kk=kk+1
                endif
                do 123 j = 1, kcp
                    zk16(jparak+kk)=zk8(jkval+j-1)
                    kk=kk+1
123              continue
!
!
!               ON AJOUTE LA LIGNE A LA TABLE
                if (resu .eq. ' ') then
                    call tbajli(nomtb, nbpara, zk16(jparak), [ibid], zr(jr),&
                                [cbid], zk16(jk), 0)
                else
                    call tbajli(nomtb, nbpara, zk16(jparak), zi(ji), zr( jr),&
                                [cbid], zk16(jk), 0)
                endif
!
110          continue
!
        endif
!
100  end do
!
    call jedetr('&&CTNOTB.TABLE_VALR')
    call jedetr('&&CTNOTB.TABLE_VALI')
    call jedetr('&&CTNOTB.TABLE_VALK')
!
!
    call jedema()
!
end subroutine
