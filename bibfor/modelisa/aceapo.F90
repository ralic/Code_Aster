subroutine aceapo(noma, nomo, lmax, npoutr, nbocc,&
                  mclf, nbepo, ntyele, ivr, ifm,&
                  jdlm)
    implicit none
#include "jeveux.h"
!
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/acedat.h"
#include "asterfort/affdef.h"
#include "asterfort/affgen.h"
#include "asterfort/affpou.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/coecis.h"
#include "asterfort/getvem.h"
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
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
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
    character(len=1) :: k1bid
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
    integer :: itabl, itblp, itbnp, ivar, ivect, ixma, j
    integer :: jcar, jcara, jdcge, jdcpo, jdcpof, jdge, jdgef
    integer :: jdgm, jdls, jdme, jdvge, jdvpo, jdvpof, jexp
    integer :: jj, jpara, jsect, jtab, jtype, jvale, k
    integer :: nbcar, nbcolo, nblign, nbmagr, nbmail, nbo, nbval
    integer :: ncar, ncarac, ndim, nfcx, ng, nm, nnosec
    integer :: npoaff, nsec, nsecpo, ntab, ntypse, nummai, nutyel
    integer :: nval, nvsec
    real(kind=8) :: epy1, hy1
!-----------------------------------------------------------------------
    call jemarq()
    call getres(nomu, concep, cmd)
!
    call wkvect('&&ACEAPO.TAB_PARA', 'V V I', 10, jpara)
    call acedat('POUTRE', 0, zi(jpara), k16b, k8b,&
                k8b, k8b)
    nsecpo = zi(jpara )
    ntypse = zi(jpara+1)
    nbo = zi(jpara+2)
    nbcar = zi(jpara+3)
    nbval = zi(jpara+4)
!
    call wkvect('&&ACEAPO.NCP', 'V V I', ntypse, jtype)
    do 2 i = 1, ntypse
        zi(jtype+i-1) = zi(jpara+4+i)
 2  end do
    ndim = zi(jtype) * ( nsecpo + 1 )
    call wkvect('&&ACEAPO.TYP_SECT', 'V V K16', ntypse, jsect)
    call wkvect('&&ACEAPO.EXPPOU', 'V V K8 ', nbo, jexp)
    call wkvect('&&ACEAPO.TABPOU', 'V V K8 ', nbo, jtab)
    call wkvect('&&ACEAPO.CARPOU', 'V V K8 ', ndim*ntypse, jcar)
    call acedat('POUTRE', 1, zi(jpara), zk16(jsect), zk8(jexp),&
                zk8(jtab), zk8(jcar))
    call wkvect('&&ACEAPO.CARA', 'V V K8', nbcar, jcara)
    call wkvect('&&ACEAPO.VALE', 'V V R8', nbval, jvale)
!
    modmai = nomo//'.MAILLE'
    mlgnma = noma//'.NOMMAI'
    mlggma = noma//'.GROUPEMA'
    ier = 0
    call jelira(mlgnma, 'NOMMAX', nbmail, k1bid)
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
    call jeecra(tmpgen, 'LONMAX', nbo, ' ')
    call jecrec(tmpgef, 'V V K8', 'NO', 'CONTIG', 'CONSTANT',&
                npoutr)
    call jeecra(tmpgef, 'LONMAX', 1, ' ')
    call wkvect('&&ACEAPO.POUTRE', 'V V K24', lmax, jdls)
!
! --- LECTURE ET STOCKAGE DES DONNEES  DANS L OBJET TAMPON
    do 10 ioc = 1, nbocc
        call codent(ioc, 'G', kioc)
        call getvem(noma, 'GROUP_MA', 'POUTRE', 'GROUP_MA', ioc,&
                    iarg, lmax, zk24(jdls), ng)
        call getvem(noma, 'MAILLE', 'POUTRE', 'MAILLE', ioc,&
                    iarg, lmax, zk24(jdls), nm)
        call getvtx('POUTRE', 'SECTION', ioc, iarg, 1,&
                    sec, nsec)
        call getvtx('POUTRE', 'VARI_SECT', ioc, iarg, 1,&
                    varsec, nvsec)
!
!
        call getvid('POUTRE', 'TABLE_CARA', ioc, iarg, 1,&
                    tabcar, ntab)
        if (ntab .eq. 1) then
            call getvtx('POUTRE', 'NOM_SEC', ioc, iarg, 1,&
                        nomsec, nnosec)
            ASSERT(nnosec.eq.1)
!
            call jeveuo(tabcar//'.TBNP', 'L', itbnp)
!            NOMBRE DE COLONNES
            nbcolo = zi(itbnp)
!            ON RECHERCHE NOMSEC DANS LA 1ER COLONNE
            call jeveuo(tabcar//'.TBLP', 'L', itblp)
            typca=zk24(itblp+1)
            if (typca(1:2) .ne. 'K8' .and. typca(1:3) .ne. 'K24') then
                call u2mesk('F', 'MODELISA8_17', 1, tabcar)
            endif
            call jeveuo(zk24(itblp+2), 'L', itabl)
            nblign = zi(itbnp+1)
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
            call u2mesk('F', 'MODELISA8_18', 2, vmessk)
97          continue
            jj=0
            do 96 i = 1, nbcolo-1
                if (zk24(itblp+4*i+1) .ne. 'R') goto 96
                do 102 k = 1, nbo
                    if (zk24(itblp+4*i) .eq. zk8(jexp-1+k)) goto 103
102              continue
                goto 96
103              continue
                jj=jj+1
                zk8(jcara-1+jj) = zk24(itblp+4*i)(1:8)
                call jeveuo(zk24(itblp+4*i+2), 'L', ivect)
                zr(jvale-1+jj)=zr(ivect-1+iisec)
96          continue
            ncarac=jj
        else
            call getvtx('POUTRE', 'CARA', ioc, iarg, nbcar,&
                        zk8(jcara), ncar)
            call getvr8('POUTRE', 'VALE', ioc, iarg, nbval,&
                        zr(jvale), nval)
            ASSERT(ncar.gt.0)
            ncarac=ncar
        endif
!
        fcx = '.'
        call getvid('POUTRE', 'FCX', ioc, iarg, 1,&
                    fcx, nfcx)
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
                if (zk8(jcara-1+k)(1:3) .eq. 'HY ') then
                    hy1 = zr(jvale-1+k)
                    zk8(jcara-1+k) = 'HY1'
                endif
                if (zk8(jcara-1+k)(1:4) .eq. 'EPY ') then
                    epy1 = zr(jvale-1+k)
                    zk8(jcara-1+k) = 'EPY1'
                endif
120          continue
            ncar = ncar + 1
            zk8(jcara-1+ncar) = 'HY2'
            zr(jvale-1+ncar) = hy1
            if (epy1 .ne. 0.d0) then
                ncar = ncar + 1
                zk8(jcara-1+ncar) = 'EPY2'
                zr(jvale-1+ncar) = epy1
            endif
            ncarac = ncar
        endif
!
!
        if (ntab .eq. 0) then
            do 20 i = 1, ntypse
                if (sec .eq. zk16(jsect+i-1)) then
                    isec = i - 1
                    do 22 j = 1, zi(jtype+i-1)
                        if (zk8(jcara) .eq. zk8(jcar+j+ndim*(i-1)-1)) then
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
                call jeveuo(jexnom(mlggma, zk24(jdls+i-1)), 'L', jdgm)
                call jelira(jexnom(mlggma, zk24(jdls+i-1)), 'LONUTI', nbmagr, k1bid)
                do 42 j = 1, nbmagr
                    nummai = zi(jdgm+j-1)
                    call jenuno(jexnum(mlgnma, nummai), nommai)
                    nutyel = zi(jdme+nummai-1)
                    do 44 k = 1, nbepo
                        if (nutyel .eq. ntyele(k)) then
                            if (k .eq. 4) iivar = 10
                            call affpou(tmpgen, tmpgef, fcx, nommai, isec,&
                                        iivar, zk8(jcara), ncarac, zr(jvale), zk8(jtab),&
                                        zk8(jexp), nbo, kioc, ier)
                            iivar = ivar
                            goto 42
                        endif
44                  continue
                    vmessk(1) = mclf
                    vmessk(2) = nommai
                    call u2mesk('F', 'MODELISA_8', 2, vmessk)
42              continue
40          continue
        endif
!
! ---    "MAILLE" = TOUTES LES MAILLES POSSIBLES DE LA LISTE DE MAILLES
        if (nm .gt. 0) then
            do 50 i = 1, nm
                nommai = zk24(jdls+i-1)
                call jenonu(jexnom(mlgnma, nommai), nummai)
                nutyel = zi(jdme+nummai-1)
                do 52 j = 1, nbepo
                    if (nutyel .eq. ntyele(j)) then
                        if (j .eq. 4) iivar = 10
                        call affpou(tmpgen, tmpgef, fcx, nommai, isec,&
                                    iivar, zk8(jcara), ncarac, zr(jvale), zk8(jtab),&
                                    zk8(jexp), nbo, kioc, ier)
                        iivar = ivar
                        goto 50
                    endif
52              continue
                vmessk(1) = mclf
                vmessk(2) = nommai
                call u2mesk('F', 'MODELISA_8', 2, vmessk)
50          continue
        endif
!
10  end do
    if (ier .ne. 0) then
        call u2mess('F', 'MODELISA_14')
    endif
!
    call jelira(tmpgen, 'NUTIOC', npoaff, k1bid)
!
! --- VERIFICATION DES OBLIGATIONS ET AFFECTATION DES DEFAUTS
    do 60 i = 1, npoaff
        call jenuno(jexnum(tmpgen, i), nommai)
        call jenonu(jexnom(mlgnma, nommai), nummai)
        nutyel = zi(jdme+nummai-1)
        call affdef(tmpgen, nommai, nutyel, ntyele, zk8(jtab),&
                    ier)
60  end do
    if (ier .ne. 0) then
        call u2mess('F', 'MODELISA_15')
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
        zk8(jdcpo+i-1) = zk8(jtab+i-1)
68  end do
    do 69 i = 24, 31
        zk8(jdcpo+i-1) = zk8(jtab+i+13-1)
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
        call nocart(cartpo, 3, ' ', 'NOM', 1,&
                    nommai, 0, ' ', 31)
        call nocart(cartpf, 3, ' ', 'NOM', 1,&
                    nommai, 0, ' ', 1)
70  end do
!
! --- AFFECTATIONS DONNEES GEOMETRIQUES (ON AFFECTE TOUTES LES CMPS)
    do 74 i = 1, 13
        zk8(jdcge+i-1) = zk8(jtab+i+23-1)
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
            call nocart(cartge, 3, ' ', 'NOM', 1,&
                        nommai, 0, ' ', 13)
        else
! ---       RECTANGLE OU CERCLE
            do 80 i = 1, 13
                zr (jdvge+i-1) = zr(jdge+i+22)
80          continue
            call nocart(cartge, 3, ' ', 'NOM', 1,&
                        nommai, 0, ' ', 13)
        endif
76  end do
!
! --- COMPACTAGE DES CARTES (MAIS ON NE CHERCHE PAS DE REMANENCE) :
    call tecart(cartpo)
    call tecart(cartge)
!
! --- NETTOYAGE
    call jedetr('&&ACEAPO.TAB_PARA')
    call jedetr('&&ACEAPO.NCP')
    call jedetr('&&ACEAPO.TYP_SECT')
    call jedetr('&&ACEAPO.EXPPOU')
    call jedetr('&&ACEAPO.TABPOU')
    call jedetr('&&ACEAPO.CARPOU')
    call jedetr('&&ACEAPO.CARA')
    call jedetr('&&ACEAPO.VALE')
    call jedetr('&&ACEAPO.POUTRE')
    call jedetr(tmpgen)
    call jedetr(tmpgef)
    call jedetr(tmpnpo)
    call jedetr(tmpvpo)
    call jedetr(tmpnge)
    call jedetr(tmpvge)
!
    call jedema()
end subroutine
