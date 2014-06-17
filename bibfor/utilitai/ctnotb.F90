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
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
    integer :: jcmp, jkcha, jlno, jrval, jival, jniord,  i,  nbnox
    integer :: jcnsl,   nbcmpx, n,   ino, indno
    integer :: kcp, icmp, indcmp, ni, nr, nk,    kk, nbpara
    integer ::  j, ibid
    complex(kind=8) :: cbid
    character(len=8) :: kno
    character(len=19) :: chamns
    character(len=8), pointer :: nom_cmp(:) => null()
    character(len=16), pointer :: table_parak(:) => null()
    integer, pointer :: table_vali(:) => null()
    character(len=16), pointer :: table_valk(:) => null()
    real(kind=8), pointer :: table_valr(:) => null()
    real(kind=8), pointer :: val_cmp(:) => null()
    integer, pointer :: cnsd(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    real(kind=8), pointer :: cnsv(:) => null()
    character(len=8), pointer :: cnsc(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
!
! --- 0. INITIALISATIONS
    ibid=0
    cbid=(0.d0,0.d0)
    chamns = '&&CTNOTB.CNS       '
    call jeveuo(nkcmp, 'L', jcmp)
    call jeveuo(nkcha, 'L', jkcha)
    call jeveuo(mesnoe, 'L', jlno)
    call jeveuo(nrval, 'L', jrval)
    call jeveuo(nival, 'L', jival)
    call jeveuo(niord, 'L', jniord)
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
!
!     TABLEAU DES VALEURS ENTIERES DE LA TABLE: ZI(JI)
!     TABLEAU DES VALEURS REELES DE LA TABLE: ZR(JR)
!     TABLEAU DES VALEURS CARACTERES DE LA TABLE: ZK16(JK)
!     POUR DES RAISONS DE PERF, CES TABLEAUX ONT ETE SORTIS DE
!     LA BOUCLE, D'OU DES DIMENSIONS EN DUR (NOMBRE SUFFISANT)
    AS_ALLOCATE(vr=table_valr, size=50)
    AS_ALLOCATE(vi=table_vali, size=50)
    AS_ALLOCATE(vk16=table_valk, size=50)
!
    do 100 i = 1, nbval
!
        if (zk24(jkcha+i-1)(1:18) .ne. '&&CHAMP_INEXISTANT') then
!
!
!            -- PASSAGE CHAM_NO => CHAM_NO_S
            call cnocns(zk24(jkcha+i-1), 'V', chamns)
            call jeveuo(chamns//'.CNSV', 'L', vr=cnsv)
            call jeveuo(chamns//'.CNSL', 'L', jcnsl)
            call jeveuo(chamns//'.CNSD', 'L', vi=cnsd)
            call jeveuo(chamns//'.CNSC', 'L', vk8=cnsc)
!
!             NOMBRE DE NOEUDS MAX DU CHAMP: NBNOX
            nbnox=cnsd(1)
!             NOMBRE DE COMPOSANTES MAX DU CHAMP : NBCMPX
            nbcmpx=cnsd(2)
!
!             NOMBRE DE COMPOSANTES DESIREES : N
            if (toucmp) then
                n=nbcmpx
            else
                n=nbcmp
            endif
!
!             TABLEAU DES VALEURS DES COMPOSANTES DESIREES: ZR(JVAL)
            AS_ALLOCATE(vr=val_cmp, size=n)
!
!             TABLEAU DES NOMS DE COMPOSANTES DESIREES : ZK8(JKVAL)
            AS_ALLOCATE(vk8=nom_cmp, size=n)
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
                        indcmp=indik8(zk8(jcmp),cnsc(icmp),&
                        1,nbcmp)
                        if (indcmp .eq. 0) goto 120
                    endif
!                  - SI LE CHAMP A UNE VALEUR, ON POURSUIT ET ON
!                  STOCKE LE NOM ET LA VALEUR DE COMPOSANTE :
                    if (.not.zl(jcnsl+nbcmpx*(ino-1)+icmp-1)) goto 120
                    kcp=kcp+1
                    val_cmp(kcp)=cnsv(1+nbcmpx*(ino-1)+icmp-1)
                    nom_cmp(kcp)=cnsc(icmp)
120             continue
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
                    table_valr(kk+1)=zr(jrval+i-1)
                    kk=kk+1
                endif
                do 121 j = 1, ndim
                    table_valr(kk+1)=vale(1+3*(ino-1)+j-1)
                    kk=kk+1
121              continue
                do 122 j = 1, kcp
                    table_valr(kk+1)=val_cmp(j)
                    kk=kk+1
122              continue
!
                kk=0
                if (resu .eq. ' ') then
                    table_valk(kk+1)=zk24(jkcha+i-1)(1:16)
                    kk=kk+1
                else
                    table_valk(kk+1)=resu
                    kk=kk+1
                    table_valk(kk+1)=nsymb
                    kk=kk+1
                    table_vali(1)=zi(jniord+i-1)
                    if (typac .eq. 'MODE') table_vali(1+1)=zi(jival+i-1)
                endif
                call jenuno(jexnum(noma//'.NOMNOE', ino), kno)
                table_valk(kk+1)=kno
!
!
!               TABLEAU DES NOMS DE PARAMETRES DE LA TABLE: ZK16(JPARAK)
                nbpara=nr+ni+nk
                AS_ALLOCATE(vk16=table_parak, size=nbpara)
!
!               ON REMPLIT ZK16(JPARAK)
                kk=0
                if (resu .eq. ' ') then
                    table_parak(kk+1)='CHAM_GD'
                    kk=kk+1
                else
                    table_parak(kk+1)='RESULTAT'
                    kk=kk+1
                    table_parak(kk+1)='NOM_CHAM'
                    kk=kk+1
                    if (typac .ne. 'ORDRE') then
                        table_parak(kk+1)=typac
                        kk=kk+1
                    endif
                    table_parak(kk+1)='NUME_ORDRE'
                    kk=kk+1
                endif
                table_parak(kk+1)='NOEUD'
                kk=kk+1
                table_parak(kk+1)='COOR_X'
                kk=kk+1
                if (ndim .ge. 2) then
                    table_parak(kk+1)='COOR_Y'
                    kk=kk+1
                endif
                if (ndim .eq. 3) then
                    table_parak(kk+1)='COOR_Z'
                    kk=kk+1
                endif
                do 123 j = 1, kcp
                    table_parak(kk+1)=nom_cmp(j)
                    kk=kk+1
123              continue
!
!
!               ON AJOUTE LA LIGNE A LA TABLE
                if (resu .eq. ' ') then
                    call tbajli(nomtb, nbpara, table_parak, [ibid], table_valr,&
                                [cbid], table_valk, 0)
                else
                    call tbajli(nomtb, nbpara, table_parak, table_vali, table_valr,&
                                [cbid], table_valk, 0)
                endif
                AS_DEALLOCATE(vk16=table_parak)
!
110         continue
            AS_DEALLOCATE(vr=val_cmp)
            AS_DEALLOCATE(vk8=nom_cmp)
!
        endif
!
100  end do
!
    AS_DEALLOCATE(vr=table_valr)
    AS_DEALLOCATE(vi=table_vali)
    AS_DEALLOCATE(vk16=table_valk)
!
!
    call jedema()
!
end subroutine
