subroutine aceaor(noma, nomo, lmax, nbepo, nbedi,&
                  nbtel, ntyele, nomele, ivr, ifm,&
                  nbocc)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/r8miem.h"
#include "asterc/r8rddg.h"
#include "asterfort/aceatu.h"
#include "asterfort/affori.h"
#include "asterfort/alcart.h"
#include "asterfort/angvx.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nocart.h"
#include "asterfort/utmess.h"
#include "asterfort/vdiff.h"
#include "asterfort/wkvect.h"
!
    integer :: lmax, nbepo, nbedi, ntyele(*), ivr(*), nbocc(*)
    character(len=8) :: noma, nomo
    character(len=16) :: nomele(*)
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     AFFECTATION DES CARACTERISTIQUES POUR LES ORIENTATIONS
! ----------------------------------------------------------------------
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : NOMO   : NOM DU MODELE
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ibid, ier, ifm, ioc, ixma, ixno
    integer :: ixnw, j, jad, jdcmpo, jdco, jdgm, jdgn
    integer :: jdls, jdme, jdne, jdno, jdnw, jdor, jdtm
    integer :: jdvlvo, k, nbcar, nbmagr, nbmail, nbmtot, nbmtrd
    integer :: nbnogr, nbtel, nbval, ncar, nco, ng, nj
    integer :: nm, nn, no1, no2, nocaor, ntpoi, ntseg
    integer :: ntseg3, ntseg4, nummai, numnoe, numtrd, nutyel, nutyma
    integer :: nval
!-----------------------------------------------------------------------
    parameter    ( nbcar = 100 , nbval = 1000 , nco = 4 )
    real(kind=8) :: val(nbval), x1(3), x2(3), x3(3)
    real(kind=8) :: tsm, rddg
    real(kind=8) :: alpha, beta, gamma
    character(len=8) :: nomu, car(nbcar), carori(nco)
    character(len=4) :: exituy
    character(len=16) :: concep, cmd, nunoel
    character(len=19) :: cartor
    character(len=24) :: tmpnor, tmpvor, tmpori
    character(len=24) :: mlgnma, mlgnno, mlgtma, mlggno, mlggma, mlgcoo, mlgcnx
    character(len=24) :: modnoe, modnem, modmai, nomnoe, nommai
    integer :: iarg
!     ------------------------------------------------------------------
    data carori  /'VECT_Y ','VECT_X_Y','ANGL_NAU','ANGL_VRI'/
!     ------------------------------------------------------------------
!
    call jemarq()
    tsm = r8miem()
    rddg = r8rddg()
    ier = 0
    call getres(nomu, concep, cmd)
    tmpori = nomu//'.ORIENTATION'
!
! --- CONSTRUCTION DES CARTES
    cartor = nomu//'.CARORIEN'
    tmpnor = cartor//'.NCMP'
    tmpvor = cartor//'.VALV'
!
! --- RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MODELE
    modmai = nomo//'.MAILLE'
    modnoe = nomo//'.NOEUD'
    modnem = nomo//'.MODELE    .NEMA'
!
! --- RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MAILLAGE ASSOCIE
    mlgnma = noma//'.NOMMAI'
    mlgnno = noma//'.NOMNOE'
    mlgtma = noma//'.TYPMAIL'
    mlgcnx = noma//'.CONNEX'
    mlggno = noma//'.GROUPENO'
    mlggma = noma//'.GROUPEMA'
    mlgcoo = noma//'.COORDO    .VALE'
!
    call jelira(mlgnma, 'NOMMAX', nbmail)
    call jeexin(modmai, ixma)
    call jeexin(modnem, ixnw)
    call jeexin(modnoe, ixno)
    nbmtrd = 0
    if (ixma .ne. 0) call jeveuo(modmai, 'L', jdme)
    if (ixno .ne. 0) call jeveuo(modnoe, 'L', jdne)
    if (ixnw .ne. 0) then
        call jelira(modnem, 'NMAXOC', nbmtrd)
        call jeveuo(modnem, 'L', jdnw)
    endif
    nbmtot = nbmail + nbmtrd
!
! --- RECUPERATION DES ADRESSES JEVEUX UTILES
    call jeveuo(mlgtma, 'L', jdtm)
    call jeveuo(mlgcoo, 'L', jdco)
!
! --- RECUPERATION DES NUMEROS DES TYPES MAILLES POI1/SEG2
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1'), ntpoi)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2'), ntseg)
!
! ================ MODIF ===============
!
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG3'), ntseg3)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG4'), ntseg4)
!
! ================ MODIF ===============
!
    call wkvect('&&TMPORIEN', 'V V K24', lmax, jdls)
    call wkvect(tmpori, 'V V R', nbmtot*3, jdor)
!
! --- INITIALISATION DES VALEURS DES ANGLES NAUTIQUES PAR DEFAUT
!                      SUR TOUTES LES MAILLES (REPERE LOCAL PAR DEFAUT)
    do 10 i = 1, nbmtot*3
        zr(jdor+i-1) = 0.d0
10  end do
!
    do 20 nummai = 1, nbmail
        nutyma = zi(jdtm+nummai-1)
        jad = jdor + (nummai*3) - 3
!
        if (nutyma .eq. ntseg) then
!
            call jeveuo(jexnum(mlgcnx, nummai), 'L', jdno)
            no1 = zi(jdno)
            no2 = zi(jdno+1)
            do 22 i = 1, 3
                x1(i) = zr(jdco+(no1-1)*3+i-1)
                x2(i) = zr(jdco+(no2-1)*3+i-1)
22          continue
            call vdiff(3, x2, x1, x3)
            if (abs(x3(1)) .gt. tsm .or. abs(x3(2)) .gt. tsm .or. abs(x3(3)) .gt. tsm) then
                call angvx(x3, alpha, beta)
                zr(jad) = zr(jad) + alpha
                zr(jad+1) = zr(jad+1) + beta
            endif
        endif
20  end do
!
! --- AFFECTATION DES VALEURS LUES DANS L OBJET TAMPON :
!     --------------------------------------------------
    if (nbocc(4) .ne. 0) then
        do 30 ioc = 1, nbocc(4)
            call getvem(noma, 'GROUP_MA', 'ORIENTATION', 'GROUP_MA', ioc,&
                        iarg, lmax, zk24(jdls), ng)
            call getvem(noma, 'MAILLE', 'ORIENTATION', 'MAILLE', ioc,&
                        iarg, lmax, zk24(jdls), nm)
            call getvem(noma, 'GROUP_NO', 'ORIENTATION', 'GROUP_NO', ioc,&
                        iarg, lmax, zk24(jdls), nj)
            call getvem(noma, 'NOEUD', 'ORIENTATION', 'NOEUD', ioc,&
                        iarg, lmax, zk24(jdls), nn)
            call getvtx('ORIENTATION', 'CARA', iocc=ioc, nbval=nbcar, vect=car,&
                        nbret=ncar)
            call getvr8('ORIENTATION', 'VALE', iocc=ioc, nbval=nbval, vect=val,&
                        nbret=nval)
!
! ---       "GROUP_MA" = TOUTES LES MAILLES POSSIBLES DE LA LISTE DES
!                                                    GROUPES DE MAILLES
            if (ng .gt. 0) then
                do 36 i = 1, ng
                    call jeveuo(jexnom(mlggma, zk24(jdls+i-1)), 'L', jdgm)
                    call jelira(jexnom(mlggma, zk24(jdls+i-1)), 'LONUTI', nbmagr)
                    do 38 j = 1, nbmagr
                        nummai = zi(jdgm+j-1)
                        call jenuno(jexnum(mlgnma, nummai), nommai)
                        call jeveuo(jexnum(mlgcnx, nummai), 'L', jdno)
                        nutyma = zi(jdtm+nummai-1)
                        jad = jdor + (nummai*3) - 3
                        if ((nutyma.ne.ntseg3) .and. ( nutyma.ne.ntseg4)) then
                            call affori('MAILLE', nommai, car(1), val, jad,&
                                        jdno, jdco, ivr, nutyma, ntseg,&
                                        carori, nco, ier)
                        endif
38                  continue
36              continue
            endif
!
! ---    "MAILLE" = TOUTES LES MAILLES POSSIBLES DE LA LISTE DE MAILLES
            if (nm .gt. 0) then
                do 40 i = 1, nm
                    nommai = zk24(jdls+i-1)
                    call jenonu(jexnom(mlgnma, nommai), nummai)
                    call jeveuo(jexnum(mlgcnx, nummai), 'L', jdno)
                    nutyma = zi(jdtm+nummai-1)
                    jad = jdor + (nummai*3) - 3
                    if ((nutyma.ne.ntseg3) .and. (nutyma.ne.ntseg4)) then
                        call affori('MAILLE', nommai, car(1), val, jad,&
                                    jdno, jdco, ivr, nutyma, ntseg,&
                                    carori, nco, ier)
                    endif
40              continue
            endif
!
! ---       SI DES MAILLES TARDIVES EXISTENT POUR CE MODELE :
            if (ixnw .ne. 0) then
! ----         "GROUP_NO" = TOUTES LES MAILLES TARDIVES POSSIBLES DE LA
!                                            LISTE DE GROUPES DE NOEUDS
                if (nj .gt. 0) then
                    do 42 i = 1, nj
                        call jeveuo(jexnom(mlggno, zk24(jdls+i-1)), 'L', jdgn)
                        call jelira(jexnom(mlggno, zk24(jdls+i-1)), 'LONUTI', nbnogr)
                        do 44 j = 1, nbnogr
                            numnoe = zi(jdgn+j-1)
                            do 46 k = 1, nbmtrd
                                if (zi(jdnw+k*2-2) .eq. numnoe) numtrd= k+nbmail
46                          continue
                            call jenuno(jexnum(mlgnno, numnoe), nomnoe)
                            jad = jdor + (numtrd*3) - 3
                            call affori('NOEUD', nomnoe, car(1), val, jad,&
                                        jdno, jdco, ivr, ntpoi, ntseg,&
                                        carori, nco, ier)
44                      continue
42                  continue
                endif
! ---          "NOEUD" = TOUTES LES MAILLES TARDIVES POSSIBLES DE LA
!                                                       LISTE DE NOEUDS
                if (nn .gt. 0) then
                    do 48 i = 1, nn
                        nomnoe = zk24(jdls+i-1)
                        call jenonu(jexnom(mlgnno, nomnoe), numnoe)
                        do 50 k = 1, nbmtrd
                            if (zi(jdnw+k*2-2) .eq. numnoe) numtrd=k+ nbmail
50                      continue
                        jad = jdor + (numtrd*3) - 3
                        call affori('NOEUD', nomnoe, car(1), val, jad,&
                                    jdno, jdco, ivr, ntpoi, ntseg,&
                                    carori, nco, ier)
48                  continue
                endif
            endif
30      continue
    endif
!
    if (ier .ne. 0) then
        call utmess('F', 'MODELISA_12')
    endif
!
! --- IMPRESSION DES VALEURS DES ORIENTATIONS SI DEMANDE
    nocaor = 0
    if (ivr(3) .eq. 1) write(ifm,1000)
    do 60 nummai = 1, nbmail
        nutyel = zi(jdme+nummai-1)
        do 62 j = 1, nbtel
            if (nutyel .eq. ntyele(j)) then
                nocaor = nocaor + 1
                if (ivr(3) .eq. 1) then
                    call jenuno(jexnum(mlgnma, nummai), nommai)
                    jad = jdor + nummai*3 - 3
                    alpha = rddg * zr(jad)
                    beta = rddg * zr(jad+1)
                    gamma = rddg * zr(jad+2)
                    write(ifm,1010)nommai,nomele(j),alpha,beta,gamma
                endif
                goto 60
            endif
62      continue
60  end do
!++++++
    if (ixnw .ne. 0) then
        if (ivr(3) .eq. 1) write(ifm,1020)
        do 64 i = 1, nbmtrd
            numnoe = zi(jdnw+i*2-2)
            nutyel = zi(jdne+numnoe-1)
            do 66 j = nbepo+1, nbepo + nbedi
                if (nutyel .eq. ntyele(j)) then
                    nocaor = nocaor + 1
                    if (ivr(3) .eq. 1) then
                        call jenuno(jexnum(mlgnno, zi(jdnw+i*2-2)), nomnoe)
                        jad = jdor + (nbmail+i)*3 - 3
                        alpha = rddg * zr(jad)
                        beta = rddg * zr(jad+1)
                        gamma = rddg * zr(jad+2)
                        write(ifm,1010)nomnoe,nomele(j),alpha,beta,&
                        gamma
                    endif
                    goto 64
                endif
66          continue
64      continue
    endif
!
    1000 format(/,3x,'<ANGL> ORIENTATIONS SUR LES MAILLES DE TYPE POUTRE,'&
     &  ,' BARRE OU DISCRET',//,3x,&
     &  'NOM      TYPE             ALPHA         BETA',&
     &  '          GAMMA')
    1010 format(3x,a8,1x,a16,1x,3(1pd13.6,2x))
    1020 format(/,3x,'<ANGL> ORIENTATIONS SUR LES NOEUDS DE TYPE ',&
     &  'DISCRET',//,3x,'NOM      TYPE             ALPHA         BETA',&
     &  '          GAMMA')
!
! --- AFFECTATION DES VALEURS DU TAMPON DANS LA CARTE ORIENTATION :
!     -------------------------------------------------------------
    if (nocaor .gt. 0) then
        call alcart('G', cartor, noma, 'CAORIE')
        call jeveuo(tmpnor, 'E', jdcmpo)
        call jeveuo(tmpvor, 'E', jdvlvo)
        zk8(jdcmpo) = 'ALPHA'
        zk8(jdcmpo+1) = 'BETA'
        zk8(jdcmpo+2) = 'GAMMA'
!
! ---    AFFECTATION DES MAILLES DU MAILLAGE (POUTRE, BARRE OU DISCRET)
        do 68 nummai = 1, nbmail
            nutyel = zi(jdme+nummai-1)
            do 70 j = 1, nbtel
                if (nutyel .eq. ntyele(j)) then
! ---    RECUPERATION DES NUMEROS DES NOMS DES ELEMENTS
                    call jenuno(jexnum('&CATA.TE.NOMTE', nutyel), nunoel)
                    if ((nunoel.ne.'MET3SEG3') .and. ( nunoel.ne.'MET3SEG4') .and.&
                        (nunoel.ne.'MET6SEG3')) then
                        zr(jdvlvo) = zr(jdor+nummai*3-3)
                        zr(jdvlvo+1) = zr(jdor+nummai*3-2)
                        zr(jdvlvo+2) = zr(jdor+nummai*3-1)
                        call nocart(cartor, 3, 3, mode='NUM', nma=1,&
                                    limanu=[nummai])
                        goto 68
                    endif
                endif
70          continue
68      continue
!
! ---    AFFECTATION DES MAILLES TARDIVES DU MODELE (TYPE DISCRET)
        do 72 i = 1, nbmtrd
            numnoe = zi(jdnw+i*2-2)
            nutyel = zi(jdne+numnoe-1)
            do 74 j = nbepo+1, nbepo + nbedi
                if (nutyel .eq. ntyele(j)) then
                    zr(jdvlvo) = zr(jdor+(i+nbmail)*3-3)
                    zr(jdvlvo+1) = zr(jdor+(i+nbmail)*3-2)
                    zr(jdvlvo+2) = zr(jdor+(i+nbmail)*3-1)
                    call nocart(cartor, -3, 3, ligrel=nomo//'.MODELE    ', nma=1,&
                                limanu=[-i])
                    goto 72
                endif
74          continue
72      continue
    endif
!
!JMP AFFECTATION DES ELEMENTS TUYAUX
!
    call dismoi('F', 'EXI_TUYAU', nomo, 'MODELE', ibid,&
                exituy, ibid)
    if (exituy .eq. 'OUI') then
        call aceatu(noma, nomo, nbepo, ntyele, ivr,&
                    ifm, nbocc)
    endif
    call jedetr('&&TMPORIEN')
    call jedetr(tmpnor)
    call jedetr(tmpvor)
    call jedetr(tmpori)
!
    call jedema()
end subroutine
