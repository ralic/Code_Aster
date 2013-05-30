subroutine cteltb(nbma, mesmai, noma, nbval, nkcha,&
                  nkcmp, toucmp, nbcmp, typac, ndim,&
                  nrval, resu, nomtb, nsymb, chpgs,&
                  tych, nival, niord)
    implicit   none
    include 'jeveux.h'
!
    include 'asterc/indik8.h'
    include 'asterfort/assert.h'
    include 'asterfort/carces.h'
    include 'asterfort/celces.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/indiis.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/wkvect.h'
    integer :: nbcmp, ndim, nbval, nbma
    character(len=4) :: tych
    character(len=8) :: typac, noma, resu, nomtb
    character(len=16) :: nsymb
    character(len=19) :: chpgs
    character(len=24) :: nkcha, nkcmp, mesmai, nival, nrval, niord
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
!        BUT : REMPLISSAGE DE LA TABLE POUR UN CHAM_ELEM OU UNE CARTE
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
!                 MESMAI (K24) : OBJET DES NOMS DE MAILLE
!                 NRVAL (K16)  : OBJET DES VALEURS D'ACCES (REELS)
!                 NIVAL (K16)  : OBJET DES VALEURS D'ACCES (ENTIERS)
!                 NIORD (K16)  : NOM D'OBJET DES NUMEROS D'ORDRE
!                 NSYMB (K16)  : NOM SYMBOLIQUE DU CHAMP
!                 TYCH  (K4)   : TYPE DE CHAMP (ELNO,ELEM,ELGA,CART)
!                 CHPGS (K19)  : CHAMP DES COORD DES POINTS DE GAUSS
!                 NBMA  (I)    : NOMBRE DE MAILLES UTILISATEUR
!
!        IN/OUT : NOMTB (K24)  : OBJET TABLE
!
! ----------------------------------------------------------------------
!
    integer :: jcmp, jkcha, jlma, jrval, jival, jniord, jcoor, jconx1, jconx2
    integer :: jcpgv, jcpgl, jcpgd, i, j, jcesv, jcesl, jcesd, jcesc, nbmax
    integer :: nbcmpx
    integer :: n, jval, jkval, ima, ipt, ispt, icmp, indma, nbpt, kk
    integer :: nbcmpt, nbspt, inot, kcp, indcmp, iad, ni, nk, nr, ji, jr, jk
    integer :: nbpara, jparak, iret
    character(len=8) :: kma, kno
    complex(kind=8) :: cbid
    character(len=19) :: chames
!
!
!
!
!     ------------------------------------------------------------------
!
    call jemarq()
!
! --- 0. INITIALISATIONS
!      -----------------
    chames = '&&CTELTB.CES       '
    call jeveuo(nkcmp, 'L', jcmp)
    call jeveuo(nkcha, 'L', jkcha)
    call jeveuo(mesmai, 'L', jlma)
    call jeveuo(nrval, 'L', jrval)
    call jeveuo(nival, 'L', jival)
    call jeveuo(niord, 'L', jniord)
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    if (tych .eq. 'ELGA') then
        call jeveuo(chpgs//'.CESV', 'L', jcpgv)
        call jeveuo(chpgs//'.CESL', 'L', jcpgl)
        call jeveuo(chpgs//'.CESD', 'L', jcpgd)
    endif
!     TABLEAU D'ENTIERS DE LA TABLE: ZI(JI)
!     TABLEAU DE REELS DE LA TABLE: ZR(JR)
!     TABLEAU DE CARACTERES DE LA TABLE: ZK16(JK)
!     POUR DES RAISONS DE PERF, CES TABLEAUX ONT ETE SORTIS DE
!     LA BOUCLE, D'OU DES DIMENSIONS EN DUR (NOMBRE SUFFISANT)
    call wkvect('&&CTELTB.TABLE_VALR', 'V V R', 250, jr)
    call wkvect('&&CTELTB.TABLE_VALI', 'V V I', 250, ji)
    call wkvect('&&CTELTB.TABLE_VALK', 'V V K16', 250, jk)
!
! --- 1. LECTURE DES CHAMPS ET REMPLISSAGE DE LA TABLE
!      -----------------------------------------------
!
    do 100 i = 1, nbval
!     -- JE NE COMPRENDS PAS LA BOUCLE I=1,NBVAL (J. PELLET)
!
        if (zk24(jkcha+i-1)(1:18) .ne. '&&CHAMP_INEXISTANT') then
!
!
!            -- PASSAGE CHAMP => CHAM_ELEM_S
            if (tych(1:2) .eq. 'EL') then
                call celces(zk24(jkcha+i-1), 'V', chames)
            else if (tych.eq.'CART') then
                call carces(zk24(jkcha+i-1), 'ELEM', ' ', 'V', chames,&
                            ' ', iret)
                call assert(iret.eq.0)
            else
                call assert(.false.)
            endif
            call jeveuo(chames//'.CESV', 'L', jcesv)
            call jeveuo(chames//'.CESL', 'L', jcesl)
            call jeveuo(chames//'.CESD', 'L', jcesd)
            call jeveuo(chames//'.CESC', 'L', jcesc)
!
!             NOMBRE DE MAILLES MAX DU CHAMP : NBMAX
            nbmax=zi(jcesd)
!
!             NOMBRE DE COMPOSANTES MAX DU CHAMP : NBCMPX
            nbcmpx=zi(jcesd+1)
!
!             NOMBRE DE COMPOSANTES DESIREES : N
            if (toucmp) then
                n=nbcmpx
            else
                n=nbcmp
            endif
!
!             TABLEAU DES VALEURS DES COMPOSANTES DESIREES: ZR(JVAL)
            call jedetr('&&CTELTB.VAL_CMP')
            call wkvect('&&CTELTB.VAL_CMP', 'V V R', n, jval)
!
!             TABLEAU DES NOMS DE COMPOSANTES DESIREES : ZK8(JKVAL)
            call jedetr('&&CTELTB.NOM_CMP')
            call wkvect('&&CTELTB.NOM_CMP', 'V V K8', n, jkval)
!
!            -- ON PARCOURT LES MAILLES
            do 210 ima = 1, nbmax
!
!               - SI LA MAILLE FAIT PARTIE DES MAILLES DESIREES,
!               ON POURSUIT, SINON ON VA A LA MAILLE SUIVANTE:
                indma=indiis(zi(jlma),ima,1,nbma)
                if (indma .eq. 0) goto 210
!
!               NOMBRE DE POINTS DE LA MAILLE IMA : NBPT
                nbpt=zi(jcesd+5+4*(ima-1))
!
!               NOMBRE DE COMPOSANTES PORTEES PAR LES POINTS
!               DE LA MAILLE IMA
                nbcmpt=zi(jcesd+5+4*(ima-1)+2)
!
!               NOMBRE DE SOUS-POINTS PORTES PAR LES POINTS
                nbspt=zi(jcesd+5+4*(ima-1)+1)
!
!               -- ON PARCOURT LES POINTS DE LA MAILLE IMA
                do 220 ipt = 1, nbpt
!
!                 NUMERO DU POINT (DU MAILLAGE GLOBAL): INOT
                    inot = zi(jconx1-1+zi(jconx2-1+ima)+ipt-1)
!
!                 -- ON PARCOURT LES SOUS-POINTS DE LA MAILLE IMA
                    do 225 ispt = 1, nbspt
                        kcp=0
!
!                   -- ON PARCOURT LES COMPOSANTES PORTEES
!                   PAR LE POINT IPT
                        do 230 icmp = 1, nbcmpt
!
                            if (.not.toucmp) then
!                        -SI LA COMPOSANTE FAIT PARTIE DES
!                         COMPOSANTES DESIREES, ON POURSUIT,
!                         SINON ON VA A LA COMPOSANTE SUIVANTE
                                indcmp=indik8(zk8(jcmp),zk8(jcesc+&
                                icmp-1), 1,nbcmp)
                                if (indcmp .eq. 0) goto 230
                            endif
!
!                      VALEUR DE LA COMPOSANTE ICMP AU POINT IPT DE
!                      LA MAILLE IMA: ZR(JCESV+IAD-1)
                            call cesexi('C', jcesd, jcesl, ima, ipt,&
                                        ispt, icmp, iad)
                            if (iad .gt. 0) then
                                kcp=kcp+1
                                zr(jval+kcp-1)=zr(jcesv+iad-1)
                                zk8(jkval+kcp-1)=zk8(jcesc+icmp-1)
                            endif
!
230                      continue
                        if (kcp .eq. 0) goto 225
!                   -- POUR NE PAS DEBORDER DES OBJETS (L=250):
                        call assert(kcp.le.200)
!
!                   SOIT NI LE NOMBRE DE ENTIERS DE LA TABLE
!                   SOIT NR LE NOMBRE DE REELS DE LA TABLE
!                   SOIT NK LE NOMBRE DE CARACTERES DE LA TABLE
                        ni=1
                        nk=3
                        nr=kcp
                        if (tych .eq. 'ELNO' .or. tych .eq. 'ELGA') nr=nr+ ndim
!
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
                        if (tych .eq. 'ELNO') then
!                      -- noeud + sous_point
                            nk=nk+1
                            ni=ni+1
                        else if (tych.eq.'ELGA') then
!                      -- point + sous_point
                            ni=ni+2
                        endif
!
!                   ON REMPLIT LES TABLEAUX ZI(JI),ZR(JR),ZK16(JK)
                        kk=0
                        if (typac .eq. 'FREQ' .or. typac .eq. 'INST') then
                            zr(jr+kk)=zr(jrval+i-1)
                            kk=kk+1
                        endif
                        if (tych .eq. 'ELNO') then
                            do 240 j = 1, ndim
                                zr(jr+kk)=zr(jcoor+3*(inot-1)+j-1)
                                kk=kk+1
240                          continue
                        else if (tych.eq.'ELGA') then
                            do 241 j = 1, ndim
                                call cesexi('C', jcpgd, jcpgl, ima, ipt,&
                                            ispt, j, iad)
                                if (iad .gt. 0) then
                                    zr(jr+kk)=zr(jcpgv+iad-1)
                                    kk=kk+1
                                endif
241                          continue
                        endif
                        do 250 j = 1, kcp
                            zr(jr+kk)=zr(jval+j-1)
                            kk=kk+1
250                      continue
!
                        kk=0
                        if (resu .ne. ' ') then
                            zi(ji+kk)=zi(jniord+i-1)
                            kk=kk+1
                        endif
                        if (typac .eq. 'MODE') then
                            zi(ji+kk)=zi(jival+i-1)
                            kk=kk+1
                        endif
                        if (tych .eq. 'ELGA') then
                            zi(ji+kk)=ipt
                            kk=kk+1
                        endif
                        if (tych(1:2) .eq. 'EL') then
                            zi(ji+kk)=ispt
                            kk=kk+1
                        endif
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
                        endif
                        call jenuno(jexnum(noma//'.NOMMAI', ima), kma)
                        zk16(jk+kk)=kma
                        kk=kk+1
                        if (tych .eq. 'ELNO') then
                            call jenuno(jexnum(noma//'.NOMNOE', inot), kno)
                            zk16(jk+kk)=kno
                            kk=kk+1
                        endif
!
!                   TABLEAU DES NOMS DE PARAMETRES DE LA TABLE
                        nbpara=nr+ni+nk
                        call jedetr('&&CTELTB.TABLE_PARAK')
                        call wkvect('&&CTELTB.TABLE_PARAK', 'V V K16', nbpara, jparak)
!
                        kk=0
                        if (resu .eq. ' ') then
                            zk16(jparak+kk)='CHAM_GD'
                            kk=kk+1
                        else
                            zk16(jparak+kk)='RESULTAT'
                            kk=kk+1
                            zk16(jparak+kk)='NOM_CHAM'
                            kk=kk+1
                        endif
!
                        if (resu .ne. ' ') then
                            if (typac .ne. 'ORDRE') then
                                zk16(jparak+kk)=typac
                                kk=kk+1
                            endif
                            zk16(jparak+kk)='NUME_ORDRE'
                            kk=kk+1
                        endif
                        zk16(jparak+kk)='MAILLE'
                        kk=kk+1
                        if (tych .eq. 'ELNO') then
                            zk16(jparak+kk)='NOEUD'
                            kk=kk+1
                        else if (tych.eq.'ELGA') then
                            zk16(jparak+kk)='POINT'
                            kk=kk+1
                        endif
                        if (tych(1:2) .eq. 'EL') then
                            zk16(jparak+kk)='SOUS_POINT'
                            kk=kk+1
                        endif
!
!                   -- COORDONNEES :
                        if (tych .eq. 'ELNO' .or. tych .eq. 'ELGA') then
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
                        endif
!
!                   -- COMPOSANTES :
                        do 260 j = 1, kcp
                            zk16(jparak+kk)=zk8(jkval+j-1)
                            kk=kk+1
260                      continue
!
!                       ON AJOUTE LA LIGNE A LA TABLE
                        call tbajli(nomtb, nbpara, zk16(jparak), zi(ji), zr(jr),&
                                    cbid, zk16(jk), 0)
!
225                  continue
!
220              continue
!
210          continue
!
        endif
!
100  end do
!
    call jedetr('&&CTELTB.TABLE_VALR')
    call jedetr('&&CTELTB.TABLE_VALI')
    call jedetr('&&CTELTB.TABLE_VALK')
!
    call jedema()
!
end subroutine
