subroutine celcel(transf, cel1, base, cel2)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/celces.h'
    include 'asterfort/cescel.h'
    include 'asterfort/cescre.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/copisd.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: transf, cel1, base, cel2
! ----------------------------------------------------------------------
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ----------------------------------------------------------------------
!  BUT : TRANSFORMER UN CHAM_ELEM (CEL1) EN UN CHAM_ELEM (CEL2) QUI
!        A DE NOUVELLES PROPRIETES. PAR EXEMPLE POUR POUVOIR ETRE
!        IMPRIME, POST-TRAITE, ...
!
!  IN        TRANSF K*  : NOM DE LA TRANSFORMATION :
!
!  'NBVARI_CST' : SI LES ELEMENTS DU CHAM_ELEM (VARI_R) N'ONT
!                   PAS LE MEME NOMBRE DE DE CMPS, ON LES ALIGNE
!                   TOUS SUR LE NOMBRE MAX (SITUATION AVANT 10/99)
!
!  'PAS_DE_SP'  : SI DES ELEMENTS DU CHAM_ELEM ONT DES SOUS-POINTS
!                 ON MET LE MODE LOCAL DE LEUR GREL A 0.
!                 C'EST COMME S'ILS AVAIENT ETE EFFACES.
!
!
!  IN/JXIN   CEL1   K19 : CHAM_ELEM A TRANSFORMER
!  IN        BASE   K1  : /'V'/'G'/ : BASE DE CREATION DE CEL2
!  IN/JXOUT  CEL2   K19 : CHAM_ELEM APRES TRANSFORMATION
! ----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: ima, ipt, ispt, icmp, nbma, ibid, ierd
    integer :: jcesd1, jcesl1, jcesv1, jcesc1, jcesk1
    integer :: jcesd2, jcesl2, jcesv2, jceld2, nbgrel, igrel, debugr, nbel
    integer :: nbpt, nbspt, ncmp2, iad1, iad2, imolo, nbspmx, iel, nbsp
    integer :: ncmpg, nbvamx, jnbpt, jnbspt, jcelk1, nncp, ico
    character(len=19) :: ces1, ces2, ligrel, cel11, cel22
    character(len=16) :: optini, nompar
    character(len=8) :: ma, nomgd, typces, kbid
    character(len=24) :: valk(3)
! -DEB------------------------------------------------------------------
    call jemarq()
!
    call dismoi('F', 'NOM_GD', cel1, 'CHAMP', ibid,&
                nomgd, ierd)
!
!
    if (transf .eq. 'NBVARI_CST') then
!     =================================
!
        if (nomgd .ne. 'VARI_R') then
!         -- IL N'Y A RIEN A FAIRE : NBVARI EST CST !
            call copisd('CHAMP_GD', base, cel1, cel2)
            goto 80
        endif
!
!       1- ON TRANSFORME CEL1 EN CHAM_ELEM_S : CES1
!       -------------------------------------------
        ces1 = '&&CELCEL.CES1'
        call celces(cel1, 'V', ces1)
        call jeveuo(ces1//'.CESD', 'L', jcesd1)
        call jeveuo(ces1//'.CESL', 'L', jcesl1)
        call jeveuo(ces1//'.CESV', 'L', jcesv1)
        call jeveuo(ces1//'.CESC', 'L', jcesc1)
        call jeveuo(ces1//'.CESK', 'L', jcesk1)
!
!       2- ON ALLOUE UN CHAM_ELEM_S PLUS GROS: CES2
!       -------------------------------------------
        ces2 = '&&CELCEL.CES2'
        ma = zk8(jcesk1-1+1)
        typces = zk8(jcesk1-1+3)
        nbma = zi(jcesd1-1+1)
        ncmpg = zi(jcesd1-1+2)
        nbvamx = zi(jcesd1-1+5)
        call assert(ncmpg.eq.nbvamx)
!
!       2.1 : CALCUL DE 2 VECTEURS CONTENANT LE NOMBRE DE
!             POINTS DE SOUS-POINTS DES MAILLES
!       ---------------------------------------------------
        call wkvect('&&CELCEL.NBPT', 'V V I', nbma, jnbpt)
        call wkvect('&&CELCEL.NBSPT', 'V V I', nbma, jnbspt)
        do 10,ima = 1,nbma
        zi(jnbpt-1+ima) = zi(jcesd1-1+5+4* (ima-1)+1)
        zi(jnbspt-1+ima) = zi(jcesd1-1+5+4* (ima-1)+2)
10      continue
!
!       2.2 : ALLOCATION DE CES2 :
!       ---------------------------------------------------
        call cescre('V', ces2, typces, ma, nomgd,&
                    -nbvamx, kbid, zi(jnbpt), zi(jnbspt), -nbvamx)
        call jeveuo(ces2//'.CESD', 'L', jcesd2)
        call jeveuo(ces2//'.CESL', 'E', jcesl2)
        call jeveuo(ces2//'.CESV', 'E', jcesv2)
!
!
!
!
!       3- ON RECOPIE LES VALEURS DE CES1 DANS CES2 :
!       ---------------------------------------------
        do 50,ima = 1,nbma
        nbpt = zi(jcesd1-1+5+4* (ima-1)+1)
        nbspt = zi(jcesd1-1+5+4* (ima-1)+2)
!
        ncmp2 = zi(jcesd2-1+5+4* (ima-1)+3)
!
        do 40,ipt = 1,nbpt
        do 30,ispt = 1,nbspt
        do 20,icmp = 1,ncmp2
        call cesexi('C', jcesd1, jcesl1, ima, ipt,&
                    ispt, icmp, iad1)
        call cesexi('C', jcesd2, jcesl2, ima, ipt,&
                    ispt, icmp, iad2)
        call assert(iad2.lt.0)
        zl(jcesl2-1-iad2) = .true.
        if (iad1 .gt. 0) then
            zr(jcesv2-1-iad2) = zr(jcesv1-1+iad1)
!
        else
            zr(jcesv2-1-iad2) = 0.d0
        endif
20      continue
30      continue
40      continue
50      continue
!
!
!
!       4- ON TRANSFORME CES2 EN CHAM_ELEM : CEL2
!       -------------------------------------------
        cel11 = cel1
        call jeveuo(cel11//'.CELK', 'L', jcelk1)
        ligrel = zk24(jcelk1-1+1)
        optini = zk24(jcelk1-1+2)
        nompar = zk24(jcelk1-1+6)
        call cescel(ces2, ligrel, optini, nompar, 'NON',&
                    nncp, base, cel2, 'F', ibid)
!
!
!       5- MENAGE :
!       -------------------------------------------
        call jedetr('&&CELCEL.NBPT')
        call jedetr('&&CELCEL.NBSPT')
        call detrsd('CHAM_ELEM_S', ces1)
        call detrsd('CHAM_ELEM_S', ces2)
!
!
    else if (transf.eq.'PAS_DE_SP') then
!     =====================================
!
        call copisd('CHAMP_GD', base, cel1, cel2)
        cel22 = cel2
        call jeveuo(cel22//'.CELD', 'E', jceld2)
        nbgrel = zi(jceld2-1+2)
!
!       -- ON MET A ZERO LE MODE LOCAL DES GRELS QUI ONT DES
!          SOUS-POINTS :
        ico=0
        do 70,igrel = 1,nbgrel
        debugr = zi(jceld2-1+4+igrel)
        nbel = zi(jceld2-1+debugr+1)
        imolo = zi(jceld2-1+debugr+2)
        if (imolo .gt. 0) then
            nbspmx = 0
            do 60,iel = 1,nbel
            nbsp = zi(jceld2-1+debugr+4+4* (iel-1)+1)
            nbspmx = max(nbspmx,nbsp)
60          continue
            if (nbspmx .gt. 1) then
                zi(jceld2-1+debugr+2) = 0
            else
                ico=ico+1
            endif
        endif
70      continue
        if (ico .eq. 0) then
            valk(1)=cel1
            valk(2)=nomgd
            call u2mesk('F', 'CALCULEL2_40', 2, valk)
        endif
!
!
    else
!       CAS RESTANT A PROGRAMMER ...
        call assert(.false.)
    endif
!
80  continue
    call jedema()
end subroutine
