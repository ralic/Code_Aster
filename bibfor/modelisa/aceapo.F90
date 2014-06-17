subroutine aceapo(noma, nomo, lmax, npoutr, nbocc,&
                  mclf, nbepo, ntyele, ivr, ifm,&
                  jdlm)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/acedat.h"
#include "asterfort/affdef.h"
#include "asterfort/affgen.h"
#include "asterfort/affpou.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/coecis.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nocart.h"
#include "asterfort/tecart.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: lmax, npoutr, nbocc, nbepo, ifm, jdlm
    integer :: ntyele(*), ivr(*)
    character(len=8) :: noma, nomo
    character(len=*) :: mclf
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT POUTRE
! ----------------------------------------------------------------------
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : NOMO   : NOM DU MODELE
! IN  : LMAX   : NOMBRE MAX DE MAILLE OU GROUPE DE MAILLE
! IN  : NPOUTR : NOMBRE DE POUTRE DU MODELE
! IN  : NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE POUTRE
! IN  : NBEPO  : NOMBRE D'ELEMENT DE TYPE POUTRE
! IN  : NTYELE : TABLEAU DES TYPES D'ELEMENTS
! IN  : IVR    : TABLEAU DES INDICES DE VERIFICATION
! IN  : JDLM   : ADRESSE DES MAILLES
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    character(len=6) :: kioc
    character(len=8) :: k8b, nomu, fcx, nomsec
    character(len=16) :: k16b, sec, concep, cmd, varsec
    character(len=19) :: cartpo, cartge, cartpf, tabcar, napcis, foncis
    character(len=24) :: tmpnpo, tmpvpo, tmpgen, tmpnge, tmpvge, typca, nommai
    character(len=24) :: tmpnpf, tmpvpf, tmpgef, modmai, mlggma, mlgnma
    character(len=24) :: vmessk(2)
    integer :: iarg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, idw, ier, iisec, iivar, ioc, isec
    integer :: itabl,   ivar, ivect, ixma, j
    integer ::   jdcge, jdcpo, jdcpof, jdge, jdgef
    integer :: jdgm,  jdme, jdvge, jdvpo, jdvpof
    integer :: jj,      k
    integer :: nbcar, nbcolo, nblign, nbmagr, nbmail, nbo, nbval
    integer :: ncar, ncarac, ndim, nfcx, ng, nm, nnosec
    integer :: npoaff, nsec, nsecpo, ntab, ntypse, nummai, nutyel
    integer :: nval, nvsec
    real(kind=8) :: epy1, hy1
    character(len=8), pointer :: cara(:) => null()
    character(len=8), pointer :: carpou(:) => null()
    character(len=8), pointer :: exppou(:) => null()
    integer, pointer :: ncp(:) => null()
    character(len=24), pointer :: poutre(:) => null()
    integer, pointer :: tab_para(:) => null()
    character(len=8), pointer :: tabpou(:) => null()
    character(len=16), pointer :: typ_sect(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    character(len=24), pointer :: tblp(:) => null()
    integer, pointer :: tbnp(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    call getres(nomu, concep, cmd)
!
    AS_ALLOCATE(vi=tab_para, size=10)
    call acedat('POUTRE', 0, tab_para, k16b, k8b,&
                k8b, k8b)
    nsecpo = tab_para(1)
    ntypse = tab_para(1+1)
    nbo = tab_para(1+2)
    nbcar = tab_para(1+3)
    nbval = tab_para(1+4)
!
    AS_ALLOCATE(vi=ncp, size=ntypse)
    do 2 i = 1, ntypse
        ncp(i) = tab_para(1+4+i)
 2  end do
    ndim = ncp(1) * ( nsecpo + 1 )
    AS_ALLOCATE(vk16=typ_sect, size=ntypse)
    AS_ALLOCATE(vk8=exppou, size=nbo)
    AS_ALLOCATE(vk8=tabpou, size=nbo)
    AS_ALLOCATE(vk8=carpou, size=ndim*ntypse)
    call acedat('POUTRE', 1, tab_para, typ_sect, exppou,&
                tabpou,carpou)
    AS_ALLOCATE(vk8=cara, size=nbcar)
    AS_ALLOCATE(vr=vale, size=nbval)
!
    modmai = nomo//'.MAILLE'
    mlgnma = noma//'.NOMMAI'
    mlggma = noma//'.GROUPEMA'
    ier = 0
    call jelira(mlgnma, 'NOMMAX', nbmail)
    call jeexin(modmai, ixma)
    if (ixma .ne. 0) call jeveuo(modmai, 'L', jdme)
!
! --- CONSTRUCTION DES CARTES
    tmpgen = nomu//'.POUTRE'
    cartpo = nomu//'.CARGENPO'
    cartge = nomu//'.CARGEOPO'
    tmpnpo = cartpo//'.NCMP'
    tmpvpo = cartpo//'.VALV'
    tmpnge = cartge//'.NCMP'
    tmpvge = cartge//'.VALV'
!
    tmpgef = nomu//'.VENT'
    cartpf = nomu//'.CVENTCXF'
    tmpnpf = cartpf//'.NCMP'
    tmpvpf = cartpf//'.VALV'
!
! --- CREATION D UN OBJET TAMPON (SURDIMENSIONNE A NBO*NPOUTR)  :
    call jecrec(tmpgen, 'V V R', 'NO', 'CONTIG', 'CONSTANT',&
                npoutr)
    call jeecra(tmpgen, 'LONMAX', nbo)
    call jecrec(tmpgef, 'V V K8', 'NO', 'CONTIG', 'CONSTANT',&
                npoutr)
    call jeecra(tmpgef, 'LONMAX', 1)
    AS_ALLOCATE(vk24=poutre, size=lmax)
!
! --- LECTURE ET STOCKAGE DES DONNEES  DANS L OBJET TAMPON
    do 10 ioc = 1, nbocc
        call codent(ioc, 'G', kioc)
        call getvem(noma, 'GROUP_MA', 'POUTRE', 'GROUP_MA', ioc,&
                    iarg, lmax, poutre, ng)
        call getvem(noma, 'MAILLE', 'POUTRE', 'MAILLE', ioc,&
                    iarg, lmax, poutre, nm)
        call getvtx('POUTRE', 'SECTION', iocc=ioc, scal=sec, nbret=nsec)
        call getvtx('POUTRE', 'VARI_SECT', iocc=ioc, scal=varsec, nbret=nvsec)
!
!
        call getvid('POUTRE', 'TABLE_CARA', iocc=ioc, scal=tabcar, nbret=ntab)
        if (ntab .eq. 1) then
            call getvtx('POUTRE', 'NOM_SEC', iocc=ioc, scal=nomsec, nbret=nnosec)
            ASSERT(nnosec.eq.1)
!
            call jeveuo(tabcar//'.TBNP', 'L', vi=tbnp)
!            NOMBRE DE COLONNES
            nbcolo = tbnp(1)
!            ON RECHERCHE NOMSEC DANS LA 1ER COLONNE
            call jeveuo(tabcar//'.TBLP', 'L', vk24=tblp)
            typca=tblp(2)
            if (typca(1:2) .ne. 'K8' .and. typca(1:3) .ne. 'K24') then
                call utmess('F', 'MODELISA8_17', sk=tabcar)
            endif
            call jeveuo(tblp(3), 'L', itabl)
            nblign = tbnp(2)
            if (typca .eq. 'K8') then
                do 95 i = 1, nblign
                    if (zk8(itabl-1+i) .eq. nomsec) then
                        iisec=i
                        goto 97
                    endif
95              continue
            else
                do 94 i = 1, nblign
                    if (zk24(itabl-1+i)(1:8) .eq. nomsec) then
                        iisec=i
                        goto 97
                    endif
94              continue
            endif
            vmessk(1)=tabcar
            vmessk(2)=nomsec
            call utmess('F', 'MODELISA8_18', nk=2, valk=vmessk)
97          continue
            jj=0
            do 96 i = 1, nbcolo-1
                if (tblp(1+4*i+1) .ne. 'R') goto 96
                do 102 k = 1, nbo
                    if (tblp(1+4*i) .eq. exppou(k)) goto 103
102              continue
                goto 96
103              continue
                jj=jj+1
                cara(jj) = tblp(1+4*i)(1:8)
                call jeveuo(tblp(1+4*i+2), 'L', ivect)
                vale(jj)=zr(ivect-1+iisec)
96          continue
            ncarac=jj
        else
            call getvtx('POUTRE', 'CARA', iocc=ioc, nbval=nbcar, vect=cara,&
                        nbret=ncar)
            call getvr8('POUTRE', 'VALE', iocc=ioc, nbval=nbval, vect=vale,&
                        nbret=nval)
            ASSERT(ncar.gt.0)
            ncarac=ncar
        endif
!
        fcx = '.'
        call getvid('POUTRE', 'FCX', iocc=ioc, scal=fcx, nbret=nfcx)
!
        ivar = 2
!
! ----  TYPE DE SECTION ET DE VARIATION DE SECTION POUR CETTE OCCURENCE
!        TEST DE ZK8(JCARA) SEUL > VERIFICATION D HOMOGENEITE DEJA FAITE
!
        if (varsec(1:4) .eq. 'AFFI') then
            ivar = 1
            hy1 = 0.d0
            epy1 = 0.d0
            do 120 k = 1, nbcar
                if (cara(k)(1:3) .eq. 'HY ') then
                    hy1 = vale(k)
                    cara(k) = 'HY1'
                endif
                if (cara(k)(1:4) .eq. 'EPY ') then
                    epy1 = vale(k)
                    cara(k) = 'EPY1'
                endif
120          continue
            ncar = ncar + 1
            cara(ncar) = 'HY2'
            vale(ncar) = hy1
            if (epy1 .ne. 0.d0) then
                ncar = ncar + 1
                cara(ncar) = 'EPY2'
                vale(ncar) = epy1
            endif
            ncarac = ncar
        endif
!
!
        if (ntab .eq. 0) then
            do 20 i = 1, ntypse
                if (sec .eq. typ_sect(i)) then
                    isec = i - 1
                    do 22 j = 1, ncp(i)
                        if (cara(1) .eq. carpou(1+j+ndim*(i-1)-1)) then
                            ivar = 0
                            goto 24
                        endif
22                  continue
                endif
20          continue
        else
!         SI ON A DONNE TABLE_CARA LA SECTION EST CONSTANTE
            ivar = 0
            isec = 0
        endif
24      continue
        iivar = ivar
!
! ---    "GROUP_MA" = TOUTES LES MAILLES POSSIBLES DE LA LISTE DES
!                                                    GROUPES DE MAILLES
        if (ng .gt. 0) then
            do 40 i = 1, ng
                call jeveuo(jexnom(mlggma, poutre(i)), 'L', jdgm)
                call jelira(jexnom(mlggma, poutre(i)), 'LONUTI', nbmagr)
                do 42 j = 1, nbmagr
                    nummai = zi(jdgm+j-1)
                    call jenuno(jexnum(mlgnma, nummai), nommai)
                    nutyel = zi(jdme+nummai-1)
                    do 44 k = 1, nbepo
                        if (nutyel .eq. ntyele(k)) then
                            if (k .eq. 4) iivar = 10
                            call affpou(tmpgen, tmpgef, fcx, nommai, isec,&
                                        iivar, cara, ncarac, vale, tabpou,&
                                        exppou, nbo, kioc, ier)
                            iivar = ivar
                            goto 42
                        endif
44                  continue
                    vmessk(1) = mclf
                    vmessk(2) = nommai
                    call utmess('F', 'MODELISA_8', nk=2, valk=vmessk)
42              continue
40          continue
        endif
!
! ---    "MAILLE" = TOUTES LES MAILLES POSSIBLES DE LA LISTE DE MAILLES
        if (nm .gt. 0) then
            do 50 i = 1, nm
                nommai = poutre(i)
                call jenonu(jexnom(mlgnma, nommai), nummai)
                nutyel = zi(jdme+nummai-1)
                do 52 j = 1, nbepo
                    if (nutyel .eq. ntyele(j)) then
                        if (j .eq. 4) iivar = 10
                        call affpou(tmpgen, tmpgef, fcx, nommai, isec,&
                                    iivar, cara, ncarac, vale, tabpou,&
                                    exppou, nbo, kioc, ier)
                        iivar = ivar
                        goto 50
                    endif
52              continue
                vmessk(1) = mclf
                vmessk(2) = nommai
                call utmess('F', 'MODELISA_8', nk=2, valk=vmessk)
50          continue
        endif
!
10  end do
    if (ier .ne. 0) then
        call utmess('F', 'MODELISA_14')
    endif
!
    call jelira(tmpgen, 'NUTIOC', npoaff)
!
! --- VERIFICATION DES OBLIGATIONS ET AFFECTATION DES DEFAUTS
    do 60 i = 1, npoaff
        call jenuno(jexnum(tmpgen, i), nommai)
        call jenonu(jexnom(mlgnma, nommai), nummai)
        nutyel = zi(jdme+nummai-1)
        call affdef(tmpgen, nommai, nutyel, ntyele, tabpou,&
                    ier)
60  end do
    if (ier .ne. 0) then
        call utmess('F', 'MODELISA_15')
    endif
!
! --- CALCUL DES DONNEES GENERALES A PARTIR DES DONNEES GEOMETRIQUES
!                            (CERC+RECT) ET AFFECTATIONS DANS LE TAMPON
!
! --- UTILISATION DE LA NAPPE POUR LES SECTIONS RECTANGULAIRES
! --- ET DE LA FONCTION POUR LES SECTIONS CIRCULAIRES
! --- AFIN DINTERPOLER LES COEFFICIENTS DE CISAILLEMENT
!
    call coecis(napcis, foncis)
!
    do 62 i = 1, npoaff
        call jenuno(jexnum(tmpgen, i), nommai)
        call jenonu(jexnom(mlgnma, nommai), nummai)
        nutyel = zi(jdme+nummai-1)
        call affgen(tmpgen, nommai, nutyel, ntyele, napcis,&
                    foncis)
62  end do
!
! --- IMPRESSION DES VALEURS AFFECTEES DANS LE TAMPON SI DEMANDE
    if (ivr(3) .eq. 1) then
!
! ---    IMPRESSION DES DONNEES GENERALES
        write(ifm,2000)
        do 64 i = 1, npoaff
            call jenuno(jexnum(tmpgen, i), nommai)
            call jeveuo(jexnum(tmpgen, i), 'L', jdge)
            ivar = nint(zr(jdge+22))
            isec = nint(zr(jdge+35))
            write(ifm,2001)nommai,(zr(jdge+j-1),j=1,22), (zr(jdge+j-1)&
            ,j=37,44),ivar,isec
            call jenuno(jexnum(tmpgef, i), nommai)
            call jeveuo(jexnum(tmpgef, i), 'L', jdgef)
            write(ifm,*)'CX : ',zk8(jdgef)
64      continue
!
! ---    IMPRESSION DES DONNEES GEOMETRIQUES
        idw = 0
        do 66 i = 1, npoaff
            call jenuno(jexnum(tmpgen, i), nommai)
            call jeveuo(jexnum(tmpgen, i), 'L', jdge)
            isec = nint(zr(jdge+35))
            if (isec .gt. 0) then
                if (idw .eq. 0) then
                    write(ifm,2002)
                    idw = 1
                endif
                write(ifm,2003)nommai,(zr(jdge+j-1),j=24,35),isec
            endif
66      continue
    endif
!
    2000  format(/,3x,'<SECTION> ',&
     &      'VALEURS DE TYPE GENERALE AFFECTEES AUX POUTRES',//,3x,&
     &      'MAILLE   ',&
     &      'A1  ',8x,'IY1  ',7x,'IZ1  ',7x,'AY1  ',7x,'AZ1  ',/,&
     &  12x,'EY1 ',8x,'EZ1  ',7x,'JX1  ',7x,'RY1  ',7x,'RZ1  ',/,&
     &  12x,'RT1 ',8x,'A2   ',7x,'IY2  ',7x,'IZ2  ',7x,'AY2  ',/,&
     &  12x,'AZ2 ',8x,'EY2  ',7x,'EZ2  ',7x,'JX2  ',7x,'RY2  ',/,&
     &  12x,'RZ2 ',8x,'RT2  ',7x,'AI1  ',7x,'AI2  ',7x,'JG1  ',/,&
     &  12x,'JG2 ',8x,'IYR21',7x,'IYR22',7x,'IZR21',7x,'IZR22',/,&
     &  12x,'TVAR',8x,'TSEC ')
    2001  format(/,1p,3x,a8,1x,5(1pd12.5,1x),5(/,12x,5(1pd12.5,1x)),&
     &         /,12x,i6,6x,i6)
    2002  format(/,3x,'<SECTION> ',&
     &  'VALEURS DE TYPE GEOMETRIQUE AFFECTEES AUX POUTRES',//,3x,&
     &  'MAILLE   HY1         HZ1         EPY1        EPZ1'&
     &  ,/,12x,         'HY2         HZ2         EPY2        EPZ2'&
     &  ,/,12x,         'R1          EP1         R2          EP2',9x&
     &  ,'TSEC')
    2003  format(/,1p,3x,a8,1x,4(1pd12.5,1x),2(/,12x,4(1pd12.5,1x)),i6)
!
! --- ALLOCATION DES CARTES
    call alcart('G', cartpo, noma, 'CAGNPO')
    call alcart('G', cartge, noma, 'CAGEPO')
    call jeveuo(tmpnpo, 'E', jdcpo)
    call jeveuo(tmpvpo, 'E', jdvpo)
    call jeveuo(tmpnge, 'E', jdcge)
    call jeveuo(tmpvge, 'E', jdvge)
    call jeveuo(tmpnpf, 'E', jdcpof)
    call jeveuo(tmpvpf, 'E', jdvpof)
!
! --- AFFECTATIONS DES DONNEES GENERALES
    do 68 i = 1, 23
        zk8(jdcpo+i-1) = tabpou(i)
68  end do
    do 69 i = 24, 31
        zk8(jdcpo+i-1) = tabpou(1+i+13-1)
69  end do
!     POUR LA CARTE DE VENT ==> FCXP
    zk8(jdcpof) = 'FCXP'
!
    do 70 i = 1, npoaff
        call jenuno(jexnum(tmpgen, i), nommai)
        call jenonu(jexnom(mlgnma, nommai), nummai)
        zi(jdlm+nummai-1) = -1
        call jeveuo(jexnum(tmpgen, i), 'L', jdge)
        do 72 j = 1, 23
            zr(jdvpo+j-1) = zr(jdge+j-1)
72      continue
        do 73 j = 24, 31
            zr(jdvpo+j-1) = zr(jdge+j+13-1)
73      continue
        call jeveuo(jexnum(tmpgef, i), 'L', jdgef)
        zk8(jdvpof) = zk8(jdgef)
        call nocart(cartpo, 3, 31, mode='NOM', nma=1,&
                    limano=[nommai])
        call nocart(cartpf, 3, 1, mode='NOM', nma=1,&
                    limano=[nommai])
70  end do
!
! --- AFFECTATIONS DONNEES GEOMETRIQUES (ON AFFECTE TOUTES LES CMPS)
    do 74 i = 1, 13
        zk8(jdcge+i-1) = tabpou(1+i+23-1)
74  end do
    do 76 j = 1, npoaff
        call jenuno(jexnum(tmpgen, j), nommai)
        call jeveuo(jexnum(tmpgen, j), 'L', jdge)
        isec = nint(zr(jdge+35))
        if (isec .eq. 0) then
! ---       GENERALE
            do 78 i = 1, 13
                zr (jdvge+i-1) = 0.d0
78          continue
            call nocart(cartge, 3, 13, mode='NOM', nma=1,&
                        limano=[nommai])
        else
! ---       RECTANGLE OU CERCLE
            do 80 i = 1, 13
                zr (jdvge+i-1) = zr(jdge+i+22)
80          continue
            call nocart(cartge, 3, 13, mode='NOM', nma=1,&
                        limano=[nommai])
        endif
76  end do
!
! --- COMPACTAGE DES CARTES (MAIS ON NE CHERCHE PAS DE REMANENCE) :
    call tecart(cartpo)
    call tecart(cartge)
!
! --- NETTOYAGE
    AS_DEALLOCATE(vi=tab_para)
    AS_DEALLOCATE(vi=ncp)
    AS_DEALLOCATE(vk16=typ_sect)
    AS_DEALLOCATE(vk8=exppou)
    AS_DEALLOCATE(vk8=tabpou)
    AS_DEALLOCATE(vk8=carpou)
    AS_DEALLOCATE(vk8=cara)
    AS_DEALLOCATE(vr=vale)
    AS_DEALLOCATE(vk24=poutre)
    call jedetr(tmpgen)
    call jedetr(tmpgef)
    call jedetr(tmpnpo)
    call jedetr(tmpvpo)
    call jedetr(tmpnge)
    call jedetr(tmpvge)
!
    call jedema()
end subroutine
