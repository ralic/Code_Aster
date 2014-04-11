subroutine arlchi(iocc,mail,nomo,nom1,nom2,mailar, &
                  typmai,nbchel,chames,jma1,jma2, &
                  tabcor,proj)

! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================


    implicit none

#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/exisd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/carces.h"
#include "asterfort/cesred.h"
#include "asterfort/jexatr.h"
#include "asterfort/codent.h"
#include "asterfort/cescre.h"
#include "asterfort/arlcnn.h"
#include "asterfort/arlcos.h"
#include "asterfort/cesexi.h"
#include "asterfort/rcvale.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/assert.h"
#include "asterfort/jedetr.h"
#include "asterc/r8maem.h"
#include "asterfort/jedema.h"

    logical           :: proj
    character(len=8)  :: mailar,mail,nomo
    character(len=10) :: nom1,nom2
    character(len=16) :: typmai
    integer           :: nbchel,jma1,jma2,iocc
    character(len=19) :: chames(nbchel)
    character(len=24) :: tabcor

! ----------------------------------------------------------------------

! ROUTINE ARLEQUIN

! CREATION DES CHAM_ELEM_S POUR APPEL A CALCUL - ARLQ_MATR

! ----------------------------------------------------------------------


    integer ::       nbnomx
    parameter    (nbnomx=27)
    integer ::       iad,noc,iop
    character(len=19) ::  ngrm1,ngrm2
    integer ::       jgrp1,jgrp2,nbma1,nbma2
    integer ::       jdime,jcoor,jcumu,jconx
    integer ::       nbma,ndim,nbnoc1,nbnoc2
    integer ::       nummc1,nummc2
    integer ::       ncmpi,ncmpf,ncmpr,ncmpc
    integer ::       ima,icmp
    integer ::       jtypmm,jtypm,jtyp,iret
    character(len=8) ::   nomfam,elref1,elref2
    character(len=19) ::  ctfami,ctinfo
    character(len=19) ::  ctref1,ctcoo1
    character(len=19) ::  ctref2,ctcoo2,carte1,cesmat,carte,carsd,carsd1
    character(len=6) ::   ch2
    real(kind=8) ::  cno1(3*nbnomx),cno2(3*2),r8max,cnoeud,inf,sup
    character(len=6) ::  nompro
    parameter   (nompro='ARLCHI')

    character(len=8) ::  vfami,vinfo(26),ntmc1,ntmc2,materi,mater,carael
    character(len=8) ::  vref1,vcoo1(3*nbnomx),vref2,vcoo2(3*nbnomx),kbid,k8b
    character(len=16) :: nocmp(15),nomcmp(3),nomte,option
    integer ::      jfamv,jfamd,jfaml,jinfv,jinfd,jinfl
    integer ::      jref1v,jref1d,jref1l,jcoo1v,jcoo1d,jcoo1l
    integer ::      jref2v,jref2d,jref2l,jcoo2v,jcoo2d,jcoo2l
    integer ::      cxno1(nbnomx),cxno2(2)
    integer ::      jadf,jadi,jadc,jadr,jtabco
    integer ::      icodre(3),iado,iadc
    integer ::      jcesv1,jcesd1,jcesl1,jcesv2,jcesd2,jcesl2
    integer ::      jtyel,ityel,ipt,nbpt,decal,i,j,l,jcesv3,jcesd3,jcesl3
    real(kind=8) ::  cpara(15),corie(3),para(3),rbid(1)

    character(len=16) ::  motfac


! ----------------------------------------------------------------------

    call jemarq()
    r8max = r8maem()

! --- INFO SUR LE PSEUDO-MAILLAGE

    call jeveuo(mailar(1:8)//'.DIME','L',jdime)
    call jeveuo(nomo(1:8)//'.MAILLE','L',jtyel)
    nbma = zi(jdime - 1 + 3)
    ndim = 3
    call jeveuo(tabcor,'L',jtabco)

! --- LECTURE DONNEES GROUPE DE MAILLES

    ngrm1 = nom1(1:10)//'.GROUPEMA'
    ngrm2 = nom2(1:10)//'.GROUPEMA'
    call jeveuo(ngrm1(1:19),'L',jgrp1)
    call jeveuo(ngrm2(1:19),'L',jgrp2)
    call jelira(ngrm1,'LONMAX',nbma1,kbid)
    call jelira(ngrm2,'LONMAX',nbma2,kbid)

! --- INITIALISATIONS

    ctfami = chames(1)
    ctinfo = chames(2)
    ctref1 = chames(3)
    ctcoo1 = chames(4)
    ctref2 = chames(5)
    ctcoo2 = chames(6)
    motfac = 'LIAISON_ELEM'
    call getvtx(motfac,'OPTION', iocc=iocc, scal=option, nbret=iop)
    if (option .eq. '3D_POU_ARLEQUIN') then
       call getvid(motfac,'CHAM_MATER',iocc=iocc,scal=materi,nbret=noc)
       cesmat = '&&'//nompro//'.CESMAT'
       carte1 = materi//'.CHAMP_MAT '
       call carces(carte1,'ELEM',' ','V',cesmat,'A',iret)
       call cesred(cesmat,nbma2,zi(jtabco+nbma1),0,kbid,'V',cesmat)
       call jeveuo(cesmat//'.CESD', 'L', jcesd1)
       call jeveuo(cesmat//'.CESV', 'L', jcesv1)
       call jeveuo(cesmat//'.CESL', 'L', jcesl1)
       call getvid(motfac,'CARA_ELEM',iocc=iocc,scal=carael,nbret=noc)
       ncmpi = 15
       carsd = '&&'//nompro//'.CARGENPO'
       carte = carael//'.CARGENPO  '
       nocmp(1)  = 'A1      '
       nocmp(2)  = 'IY1     '
       nocmp(3)  = 'IZ1     '
       nocmp(4)  = 'AY1     '
       nocmp(5)  = 'AZ1     '
       nocmp(6)  = 'EY1     '
       nocmp(7)  = 'EZ1     '
       nocmp(8)  = 'A2      '
       nocmp(9)  = 'IY2     '
       nocmp(10) = 'IZ2     '
       nocmp(11) = 'AY2     '
       nocmp(12) = 'AZ2     '
       nocmp(13) = 'EY2     '
       nocmp(14) = 'EZ2     '
       nocmp(15) = 'JX1     '
       call carces(carte ,'ELEM',' ','V',carsd ,'A',iret)
       call cesred(carsd,nbma2,zi(jtabco+nbma1),ncmpi,nocmp,'V',carsd)
       call jeveuo(carsd//'.CESD', 'L', jcesd2)
       call jeveuo(carsd//'.CESV', 'L', jcesv2)
       call jeveuo(carsd//'.CESL', 'L', jcesl2)
       carsd1 = '&&'//nompro//'.CARORIEN'
       call exisd('CARTE', carael//'.CARORIEN', iret)
       if (iret .ne. 0) then
           call carces(carael//'.CARORIEN  ' ,'ELEM',' ','V',carsd1 ,'A',iret)
           nomcmp(1)  = 'ALPHA   '
           nomcmp(2)  = 'BETA    '
           nomcmp(3)  = 'GAMMA   '
           call cesred(carsd1,nbma2,zi(jtabco+nbma1),3,nomcmp,'V',carsd1)
           call jeveuo(carsd1//'.CESD', 'L', jcesd3)
           call jeveuo(carsd1//'.CESV', 'L', jcesv3)
           call jeveuo(carsd1//'.CESL', 'L', jcesl3)
       endif
    endif    

! --- LECTURE SD CONTENANT NOM DES TYPES ELEMENTS (&&CATA.NOMTM)

    call jeveuo(typmai,'L',jtypmm)
    call jeveuo(mailar(1:8)//'.TYPMAIL        ','L',jtypm)
    call jeveuo(mail(1:8)//'.TYPMAIL        ','L',jtyp)

! --- LECTURE DONNEES MAILLAGE

    call jeveuo(mail(1:8)//'.COORDO    .VALE','L',jcoor)
    call jeveuo(mail(1:8)//'.CONNEX         ','L',jconx)
    call jeveuo(jexatr(mail(1:8)//'.CONNEX         ','LONCUM'),'L',jcumu)

! --- REMPLISSAGE COMPOSANTES DES CHAM_ELEM_S

    ncmpf = 1
    vfami = 'Z1'
    ncmpi = 26
    do 10 icmp = 1,ncmpi
        call codent(icmp,'G',ch2)
        vinfo(icmp) = 'X'//ch2
    10 end do

    call cescre('V',ctfami,'ELEM',mailar,'NEUT_K8',ncmpf,vfami, &
                [-1],[-1],[-ncmpf])
    call cescre('V',ctinfo,'ELEM',mailar,'NEUT_R',ncmpi,vinfo,  &
                [-1],[-1],[-ncmpi])

! --- REMPLISSAGE COMPOSANTES DES CHAM_ELEM_S

    ncmpr = 1
    vref1 = 'Z1'
    vref2 = 'Z1'
    ncmpc = 3*nbnomx
    do 13 icmp = 1,ncmpc
        call codent(icmp,'G',ch2)
        vcoo1(icmp) = 'X'//ch2
        vcoo2(icmp) = 'X'//ch2
    13 end do

    call cescre('V',ctref1,'ELEM',mailar,'NEUT_K8',ncmpr,vref1, &
                [-1],[-1],[-ncmpr])
    call cescre('V',ctcoo1,'ELEM',mailar,'N120_R',ncmpc,vcoo1,  &
                [-1],[-1],[-ncmpc])
    call cescre('V',ctref2,'ELEM',mailar,'NEUT_K8',ncmpr,vref2, &
                [-1],[-1],[-ncmpr])
    call cescre('V',ctcoo2,'ELEM',mailar,'N120_R',ncmpc,vcoo2,  &
                [-1],[-1],[-ncmpc])

! --- ACCES AUX CHAM_ELEM_S - VALEURS

    call jeveuo(ctfami//'.CESV','E',jfamv)
    call jeveuo(ctfami//'.CESD','E',jfamd)
    call jeveuo(ctfami//'.CESL','E',jfaml)

    call jeveuo(ctinfo//'.CESV','E',jinfv)
    call jeveuo(ctinfo//'.CESD','E',jinfd)
    call jeveuo(ctinfo//'.CESL','E',jinfl)

    call jeveuo(ctref1//'.CESV','E',jref1v)
    call jeveuo(ctref1//'.CESD','E',jref1d)
    call jeveuo(ctref1//'.CESL','E',jref1l)

    call jeveuo(ctcoo1//'.CESV','E',jcoo1v)
    call jeveuo(ctcoo1//'.CESD','E',jcoo1d)
    call jeveuo(ctcoo1//'.CESL','E',jcoo1l)

    call jeveuo(ctref2//'.CESV','E',jref2v)
    call jeveuo(ctref2//'.CESD','E',jref2d)
    call jeveuo(ctref2//'.CESL','E',jref2l)

    call jeveuo(ctcoo2//'.CESV','E',jcoo2v)
    call jeveuo(ctcoo2//'.CESD','E',jcoo2d)
    call jeveuo(ctcoo2//'.CESL','E',jcoo2l)

! --- REMPLISSAGE VALEURS DES CHAM_ELEM_S

    nomfam = 'ARLQ_1'
    proj = .false.
    do 90 ima = 1,nbma

! --- NUMERO DES MAILLES CONCERNEES

        nummc1 = zi(jtabco+jma1-1)
        ntmc1 = zk8(jtypmm+zi(jtypm+jma1-1)-1)
        nummc2 = zi(jtabco+nbma1+jma2-1)
        ntmc2 = zk8(jtypmm+zi(jtypm+nbma1+jma2-1)-1)

! --- REMPLISSAGE CTFAMI

        call cesexi('S', jfamd,jfaml,ima,1,1,1,jadf)
        if (jadf < 0) then
            zk8(jfamv-1-jadf) = nomfam
            zl(jfaml-1-jadf) = .true.
        endif

! --- COORDONNEES SOLIDES DE LA MAILLE 1 DU COUPLE

        call arlcnn(nummc1,zi(jconx),zi(jcumu),nbnoc1,cxno1)
        call arlcos(nummc1,zi(jconx),zi(jcumu),zr(jcoor),ndim,cno1)

        if (ntmc1 == 'SEG2') then
            elref1 = 'SE2'
        elseif (ntmc1 == 'HEXA20') then
            elref1 = 'H20'
        elseif (ntmc1 == 'HEXA8') then
            elref1 = 'HE8'
        elseif (ntmc1 == 'PENTA15') then
            elref1 = 'P15'
        elseif (ntmc1 == 'PENTA6') then
            elref1 = 'PE6'
        elseif (ntmc1 == 'TETRA10') then
            elref1 = 'T10'
        elseif (ntmc1 == 'TETRA4') then
            elref1 = 'TE4'
        endif

! --- COORDONNEES SOLIDES DE LA MAILLE 2 DU COUPLE

        call arlcnn(nummc2,zi(jconx),zi(jcumu),nbnoc2,cxno2)
        call arlcos(nummc2,zi(jconx),zi(jcumu),zr(jcoor),ndim,cno2)

        if (ntmc2 == 'SEG2') then
            elref2 = 'SE2'
        elseif (ntmc2 == 'HEXA20') then
            elref2 = 'H20'
        elseif (ntmc2 == 'HEXA8') then
            elref2 = 'HE8'
        elseif (ntmc2 == 'PENTA15') then
            elref2 = 'P15'
        elseif (ntmc2 == 'PENTA6') then
            elref2 = 'PE6'
        elseif (ntmc2 == 'TETRA10') then
            elref2 = 'T10'
        elseif (ntmc2 == 'TETRA4') then
            elref2 = 'TE4'
        endif

! --- PROJECTION DU BARYCENTRE DE LA MAILLE 3D SUR LA MAILLE 1D

        cnoeud = 0.D0
        do 241 l = 1, nbnoc1
            cnoeud = cnoeud + cno1(3*l-2)
        241 end do
        cnoeud = cnoeud / nbnoc1
        inf = min(cno2(3*1-2),cno2(3*2-2))
        sup = max(cno2(3*1-2),cno2(3*2-2))
        if ((cnoeud > inf) .and. (cnoeud < sup)) then
            proj = .true.
        endif

! --- RECHERCHE DES CARACTERISTIQUES MATERIAU

        call cesexi('C',jcesd1,jcesl1,nummc2,1,1,1,iad)
        if (iad > 0) then
            mater = zk8(jcesv1-1+iad)
        end if
        call rcvale(mater,'ELAS',0,k8b,rbid(1), &
                    1,'E       ',para(1),icodre,1)
        call rcvale(mater,'ELAS',0,k8b,rbid(1), &
                    1,'RHO     ',para(2),icodre,1)
        call rcvale(mater,'ELAS',0,k8b,rbid(1), &
                    1,'NU      ',para(3),icodre,1)

! --- RECHERCHE DES CARACTERISTIQUES POUTRE

        ityel = zi(jtyel-1+nummc2)
        call jenuno(jexnum('&CATA.TE.NOMTE',ityel),nomte)
        nbpt = 1
        ncmpi = 15
        if (nomte(6:12) == 'POU_D_T') then
            do 110 ipt = 1, nbpt
                do 120 icmp = 1, ncmpi
                    if (ipt == 1) then
                        decal = 0
                    else
                        decal = 15
                    endif
                    call cesexi('S',jcesd2,jcesl2,nummc2,ipt,1, &
                                icmp+decal,iadc)
                    ASSERT(iadc.gt.0)
                    cpara(icmp) = zr(jcesv2-1+iadc)
                120 end do
                do 130 icmp = 1, 3
                    call cesexi('S', jcesd3, jcesl3, nummc2,ipt,&
                                1,icmp,iado)
                    ASSERT(iad.gt.0)
                    corie(icmp) = zr(jcesv3-1+iado)
                130 end do
            110 end do
        else
            do 111 ipt = 1, nbpt
                do 121 icmp = 1, ncmpi
                    cpara(icmp) = 0.D0
                    goto 777
                121 end do
            111 end do
            777 continue
        endif

! --- REMPLISSAGE CTINFO

        call cesexi('S', jinfd,jinfl,ima,1,1,1,jadi)
        if (jadi < 0) then
            zr(jinfv-1-jadi) = ndim
            zl(jinfl-1-jadi) = .true.
        endif

        call cesexi('S', jinfd,jinfl,ima,1,1,2,jadi)
        if (jadi < 0) then
            zr(jinfv-1-jadi) = nbnoc1
            zl(jinfl-1-jadi) = .true.
        endif

        call cesexi('S', jinfd,jinfl,ima,1,1,3,jadi)
        if (jadi < 0) then
            zr(jinfv-1-jadi) = nbnoc2
            zl(jinfl-1-jadi) = .true.
        endif

        call cesexi('S', jinfd,jinfl,ima,1,1,4,jadi)
        if (jadi < 0) then
            zr(jinfv-1-jadi) = nummc1
            zl(jinfl-1-jadi) = .true.
        endif
        call cesexi('S', jinfd,jinfl,ima,1,1,5,jadi)
        if (jadi < 0) then
            zr(jinfv-1-jadi) = nummc2
            zl(jinfl-1-jadi) = .true.
        endif
        do 991 i = 1, 3
            call cesexi('S', jinfd,jinfl,ima,1,1,5+i,jadi)
            if (jadi < 0) then
                zr(jinfv-1-jadi) = para(i)
                zl(jinfl-1-jadi) = .true.
            endif
        991 end do
        do 992 j = 1, ncmpi
            call cesexi('S', jinfd,jinfl,ima,1,1,8+j,jadi)
            if (jadi < 0) then
                zr(jinfv-1-jadi) = cpara(j)
                zl(jinfl-1-jadi) = .true.
            endif
        992 end do
        do 993 i = 1, 3
            call cesexi('S', jinfd,jinfl,ima,1,1,23+i,jadi)
            if (jadi < 0) then
                zr(jinfv-1-jadi) = corie(i)
                zl(jinfl-1-jadi) = .true.
            endif
        993 end do

! --- REMPLISSAGE CTCOO1/CTCOO2

        do 500 icmp = 1,ncmpc
            call cesexi('S', jcoo1d,jcoo1l,ima,1,1,icmp,jadc)
            if (jadc < 0) then
                if (icmp <= (ndim*nbnoc1)) then
                    zr(jcoo1v-1-jadc)=cno1(icmp)
                    zl(jcoo1l-1-jadc)=.true.
                elseif (icmp > (ndim*nbnoc1) .and. &
                    icmp <= (ndim*nbnoc1)+nbnoc1) then
                    zr(jcoo1v-1-jadc)=cxno1(icmp-(ndim*nbnoc1))
                    zl(jcoo1l-1-jadc)=.true.
                else
                    zr(jcoo1v-1-jadc) = r8max
                    zl(jcoo1l-1-jadc) = .true.
                endif
            endif

            call cesexi('S', jcoo2d,jcoo2l,ima,1,1,icmp,jadc)
            if (jadc < 0) then
                if (icmp <= (ndim*nbnoc2)) then
                    zr(jcoo2v-1-jadc)=cno2(icmp)
                    zl(jcoo2l-1-jadc)=.true.
                elseif (icmp > (ndim*nbnoc2) .and. &
                    icmp <= (ndim*nbnoc2)+nbnoc2) then
                    zr(jcoo2v-1-jadc)=cxno2(icmp-(ndim*nbnoc2))
                    zl(jcoo2l-1-jadc)=.true.
                else
                    zr(jcoo2v-1-jadc) = r8max
                    zl(jcoo2l-1-jadc) = .true.
                endif
            endif
        500 end do

! --- REMPLISSAGE CTREF1/CTREF2

        call cesexi('S', jref1d,jref1l,ima,1,1,1,jadr)
        if (jadr < 0) then
            zk8(jref1v-1-jadr) = elref1
            zl(jref1l-1-jadr) = .true.
        endif

        call cesexi('S', jref2d,jref2l,ima,1,1,1,jadr)
        if (jadr < 0) then
            zk8(jref2v-1-jadr) = elref2
            zl(jref2l-1-jadr) = .true.
        endif

    90 end do

! --- MENAGE

    call jedetr('&&'//nompro//'.CESMAT')
    call jedetr('&&'//nompro//'.CARGENPO')
    call jedetr('&&'//nompro//'.CARORIEN')

    call jedema()
end subroutine
