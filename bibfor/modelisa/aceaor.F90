subroutine aceaor(noma, nomo, lmax, nbepo, nbedi,&
                  nbtel, ntyele, nomele, ivr, ifm,&
                  nbocc)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
    integer :: lmax, nbepo, nbedi, ntyele(*), ivr(*), nbocc(*)
    character(len=8) :: noma, nomo
    character(len=16) :: nomele(*)
!
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     AFFECTATION DES CARACTERISTIQUES POUR LES ORIENTATIONS
! ----------------------------------------------------------------------
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : NOMO   : NOM DU MODELE
! ----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/r8rddg.h"
#include "asterfort/assert.h"
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
#include "blas/ddot.h"
!
! --------------------------------------------------------------------------------------------------
    integer :: ii, ifm, ioc, ixma, ixno, iarg
    integer :: ixnw, jj, jad, jin, jdcmpo, jdco, jdgm, jdgn, nbid
    integer :: jdls, jdme, jdne, jdno, jdnw, jdori, jdtm, jinit
    integer :: jdvlvo, kk, nbmagr, nbmail, nbmtot, nbmtrd
    integer :: nbnogr, nbtel, nbval, ncar, ng, nj
    integer :: nm, nn, no1, no2, nocaor, ntpoi, ntseg
    integer :: ntseg3, ntseg4, nummai, numnoe, numtrd, nutyel, nutyma, nbalarme
    integer :: nval
    parameter  ( nbval = 6 )
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: val(nbval), x1(3), x2(3), x3(3), longseg, longseuil
    real(kind=8) :: rddg, alpha, beta, gamma
    character(len=4) :: exituy
    character(len=8) :: nomu
    character(len=10) :: oricara
    character(len=16) :: concep, cmd, nunoel
    character(len=19) :: cartor
    character(len=24) :: tmpnor, tmpvor, tmpori,tmpini
    character(len=24) :: mlgnma, mlgnno, mlgtma, mlggno, mlggma, mlgcoo, mlgcnx
    character(len=24) :: modnoe, modnem, modmai, nomnoe, nommai
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    rddg = r8rddg()
    call getres(nomu, concep, cmd)
    tmpori = nomu//'.ORIENTATION'
    tmpini = nomu//'.ORIENTINIT'
! --------------------------------------------------------------------------------------------------
!   CONSTRUCTION DES CARTES
    cartor = nomu//'.CARORIEN'
    tmpnor = cartor//'.NCMP'
    tmpvor = cartor//'.VALV'
!
!   RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MODELE
    modmai = nomo//'.MAILLE'
    modnoe = nomo//'.NOEUD'
    modnem = nomo//'.MODELE    .NEMA'
!
!   RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MAILLAGE ASSOCIE
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
! --------------------------------------------------------------------------------------------------
!   recuperation des adresses jeveux utiles
    call jeveuo(mlgtma, 'L', jdtm)
    call jeveuo(mlgcoo, 'L', jdco)
!
!   recuperation des numeros des types mailles poi1/seg2
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1'), ntpoi)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2'), ntseg)
!
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG3'), ntseg3)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG4'), ntseg4)
!
    call wkvect('&&TMPORIEN', 'V V K24', lmax, jdls)
    call wkvect(tmpori, 'V V R', nbmtot*3, jdori)
    call wkvect(tmpini, 'V V I', nbmtot*3, jinit)
!
!   Initialisation des angles nautiques sur toutes les mailles non nulles. Repère local par défaut
    do ii = 1, nbmtot*3
        zr(jdori+ii-1) = 0.d0
        zi(jinit+ii-1) = 0
    enddo
!
    do nummai = 1, nbmail
        nutyma = zi(jdtm+nummai-1)
        jad = jdori + (nummai-1)*3
        jin = jinit + (nummai-1)*3
        if (nutyma .eq. ntseg) then
            call jeveuo(jexnum(mlgcnx, nummai), 'L', jdno)
            no1 = zi(jdno)
            no2 = zi(jdno+1)
            do ii = 1, 3
                x1(ii) = zr(jdco+(no1-1)*3+ii-1)
                x2(ii) = zr(jdco+(no2-1)*3+ii-1)
            enddo
            call vdiff(3, x2, x1, x3)
            longseg = sqrt( ddot(3,x3,1,x3,1) )
            if ( longseg .gt. 0.0d0 ) then
                call angvx(x3, alpha, beta)
                zr(jad)   = zr(jad)   + alpha
                zr(jad+1) = zr(jad+1) + beta
                zi(jin  ) = 1
                zi(jin+1) = 1
            endif
        endif
    enddo
!
! --------------------------------------------------------------------------------------------------
!   Affectation des valeurs lues dans l'objet tampon
    if (nbocc(4) .ne. 0) then
        nbalarme = 0
        do ioc = 1, nbocc(4)
!           Pour les MAILLES
            call getvem(noma, 'GROUP_MA', 'ORIENTATION', 'GROUP_MA', ioc, &
                        iarg, lmax, zk24(jdls), ng)
            call getvem(noma, 'MAILLE', 'ORIENTATION', 'MAILLE', ioc, &
                        iarg, lmax, zk24(jdls), nm)
!           Seuil correspondant à la longueur nulle pour une maille :
!               si seglong .LT. longseuil ==> maille de taille nulle
            call getvr8('ORIENTATION', 'PRECISION', iocc=ioc, scal=longseuil, nbret=nbid)
            if ( nbid .ne. 1 ) longseuil = -1.0d0
!           Pour les NOEUDS
            call getvem(noma, 'GROUP_NO', 'ORIENTATION', 'GROUP_NO', ioc, &
                        iarg, lmax, zk24(jdls), nj)
            call getvem(noma, 'NOEUD', 'ORIENTATION', 'NOEUD', ioc, &
                        iarg, lmax, zk24(jdls), nn)
            call getvtx('ORIENTATION', 'CARA', iocc=ioc, scal=oricara, nbret=ncar)
            call getvr8('ORIENTATION', 'VALE', iocc=ioc, nbval=nbval, vect=val, nbret=nval)
!           GROUP_MA = toutes les mailles possibles de la liste des groupes de mailles
            if (ng .gt. 0) then
                do ii = 1, ng
                    call jeveuo(jexnom(mlggma, zk24(jdls+ii-1)), 'L', jdgm)
                    call jelira(jexnom(mlggma, zk24(jdls+ii-1)), 'LONUTI', nbmagr)
                    do jj = 1, nbmagr
                        nummai = zi(jdgm+jj-1)
                        call jenuno(jexnum(mlgnma, nummai), nommai)
                        call jeveuo(jexnum(mlgcnx, nummai), 'L', jdno)
                        nutyma = zi(jdtm+nummai-1)
                        jad = jdori + (nummai-1)*3
                        jin = jinit + (nummai-1)*3
                        if ((nutyma.ne.ntseg3) .and. ( nutyma.ne.ntseg4)) then
                            call affori('MAILLE', nommai, oricara, val, jad, jin, &
                                        jdno, jdco, nutyma, ntseg, &
                                        lseuil=longseuil, nbseuil=nbalarme)
                        endif
                    enddo
                enddo
            endif
!           MAILLE = toutes les mailles possibles de la liste de mailles
            if (nm .gt. 0) then
                do ii = 1, nm
                    nommai = zk24(jdls+ii-1)
                    call jenonu(jexnom(mlgnma, nommai), nummai)
                    call jeveuo(jexnum(mlgcnx, nummai), 'L', jdno)
                    nutyma = zi(jdtm+nummai-1)
                    jad = jdori + (nummai-1)*3
                    jin = jinit + (nummai-1)*3
                    if ((nutyma.ne.ntseg3) .and. (nutyma.ne.ntseg4)) then
                        call affori('MAILLE', nommai, oricara, val, jad, jin, &
                                    jdno, jdco, nutyma, ntseg, &
                                    lseuil=longseuil, nbseuil=nbalarme)
                    endif
                enddo
            endif
!           Si des mailles tardives existent pour ce modele :
            if (ixnw .ne. 0) then
!               GROUP_NO = toutes les mailles tardives possibles de la liste de groupes de noeuds
                if (nj .gt. 0) then
                    do ii = 1, nj
                        call jeveuo(jexnom(mlggno, zk24(jdls+ii-1)), 'L', jdgn)
                        call jelira(jexnom(mlggno, zk24(jdls+ii-1)), 'LONUTI', nbnogr)
                        do jj = 1, nbnogr
                            numnoe = zi(jdgn+jj-1)
                            numtrd = 0
                            do kk = 1, nbmtrd
                                if (zi(jdnw+kk*2-2) .eq. numnoe) numtrd = kk + nbmail
                            enddo
                            ASSERT( numtrd.ne.0 )
                            call jenuno(jexnum(mlgnno, numnoe), nomnoe)
                            jad = jdori + (numtrd-1)*3
                            jin = jinit + (numtrd-1)*3
                            call affori('NOEUD', nomnoe, oricara, val, jad, jin,&
                                        jdno, jdco, ntpoi, ntseg)
                        enddo
                    enddo
                endif
!               NOEUD = toutes les mailles tardives possibles de la liste de noeuds
                if (nn .gt. 0) then
                    do ii = 1, nn
                        nomnoe = zk24(jdls+ii-1)
                        call jenonu(jexnom(mlgnno, nomnoe), numnoe)
                        numtrd = 0
                        do kk = 1, nbmtrd
                            if (zi(jdnw+kk*2-2) .eq. numnoe) numtrd = kk + nbmail
                        enddo
                        ASSERT( numtrd.ne.0 )
                        jad = jdori + (numtrd-1)*3
                        jin = jinit + (numtrd-1)*3
                        call affori('NOEUD', nomnoe, oricara, val, jad, jin,&
                                    jdno, jdco, ntpoi, ntseg)
                    enddo
                endif
            endif
        enddo
        if (nbalarme.gt.0) call utmess('A','MODELISA_95',si=nbalarme,sr=longseuil)
    endif
!
! --------------------------------------------------------------------------------------------------
!   Impression des valeurs des orientations si demande
    nocaor = 0
    if (ivr(3) .eq. 1) write(ifm,1000)
    cnum1: do nummai = 1, nbmail
        nutyel = zi(jdme+nummai-1)
        do jj = 1, nbtel
            if (nutyel .eq. ntyele(jj)) then
                nocaor = nocaor + 1
                if (ivr(3) .eq. 1) then
                    call jenuno(jexnum(mlgnma, nummai), nommai)
                    jad = jdori + (nummai-1)*3
                    alpha = rddg * zr(jad)
                    beta  = rddg * zr(jad+1)
                    gamma = rddg * zr(jad+2)
                    write(ifm,1010)nommai,nomele(jj),alpha,beta,gamma
                endif
                cycle cnum1
            endif
        enddo
    enddo cnum1
!
    if (ixnw .ne. 0) then
        if (ivr(3) .eq. 1) write(ifm,1020)
        cii1: do ii = 1, nbmtrd
            numnoe = zi(jdnw+ii*2-2)
            nutyel = zi(jdne+numnoe-1)
            do jj = nbepo+1, nbepo + nbedi
                if (nutyel .eq. ntyele(jj)) then
                    nocaor = nocaor + 1
                    if (ivr(3) .eq. 1) then
                        call jenuno(jexnum(mlgnno, zi(jdnw+ii*2-2)), nomnoe)
                        jad = jdori + (nbmail+ii-1)*3
                        alpha = rddg * zr(jad)
                        beta  = rddg * zr(jad+1)
                        gamma = rddg * zr(jad+2)
                        write(ifm,1010) nomnoe,nomele(jj),alpha,beta,gamma
                    endif
                    cycle cii1
                endif
            enddo
        enddo cii1
    endif
!
    1000 format(/,3x,'<ANGL> ORIENTATIONS SUR LES MAILLES DE TYPE POUTRE BARRE OU DISCRET',//,3x, &
            'NOM      TYPE             ALPHA         BETA          GAMMA')
    1010 format(3x,a8,1x,a16,1x,3(1pd13.6,2x))
    1020 format(/,3x,'<ANGL> ORIENTATIONS SUR LES NOEUDS DE TYPE DISCRET',//,3x, &
            'NOM      TYPE             ALPHA         BETA          GAMMA')
!
! --------------------------------------------------------------------------------------------------
!   Affectation des valeurs du tampon dans la carte orientation :
    if (nocaor .gt. 0) then
        call alcart('G', cartor, noma, 'CAORIE')
        call jeveuo(tmpnor, 'E', jdcmpo)
        call jeveuo(tmpvor, 'E', jdvlvo)
        zk8(jdcmpo)   = 'ALPHA'
        zk8(jdcmpo+1) = 'BETA'
        zk8(jdcmpo+2) = 'GAMMA'
!
!       Affectation des mailles du maillage (poutre, barre ou discret)
        cnum2: do nummai = 1, nbmail
            nutyel = zi(jdme+nummai-1)
            do jj = 1, nbtel
                if (nutyel .eq. ntyele(jj)) then
!                   Recuperation des numeros des noms des elements
                    call jenuno(jexnum('&CATA.TE.NOMTE', nutyel), nunoel)
                    if ((nunoel.ne.'MET3SEG3') .and. ( nunoel.ne.'MET3SEG4') .and.&
                        (nunoel.ne.'MET6SEG3')) then
                        zr(jdvlvo)   = zr(jdori+nummai*3-3)
                        zr(jdvlvo+1) = zr(jdori+nummai*3-2)
                        zr(jdvlvo+2) = zr(jdori+nummai*3-1)
                        call nocart(cartor, 3, 3, mode='NUM', nma=1, limanu=[nummai])
                        cycle cnum2
                    endif
                endif
            enddo
        enddo cnum2
!       Affectation des mailles tardives du modele (type discret)
        cii2: do ii = 1, nbmtrd
            numnoe = zi(jdnw+ii*2-2)
            nutyel = zi(jdne+numnoe-1)
            do jj = nbepo+1, nbepo + nbedi
                if (nutyel .eq. ntyele(jj)) then
                    zr(jdvlvo)   = zr(jdori+(ii+nbmail)*3-3)
                    zr(jdvlvo+1) = zr(jdori+(ii+nbmail)*3-2)
                    zr(jdvlvo+2) = zr(jdori+(ii+nbmail)*3-1)
                    call nocart(cartor, -3, 3, ligrel=nomo//'.MODELE    ', nma=1, limanu=[-ii])
                    cycle cii2
                endif
            enddo
        enddo cii2
    endif
!
! --------------------------------------------------------------------------------------------------
!   Affectation des elements tuyaux
    call dismoi('EXI_TUYAU', nomo, 'MODELE', repk=exituy)
    if (exituy .eq. 'OUI') then
        call aceatu(noma, nomo, nbepo, ntyele, ivr, ifm, nbocc)
    endif
! --------------------------------------------------------------------------------------------------
!
    call jedetr('&&TMPORIEN')
    call jedetr(tmpnor)
    call jedetr(tmpvor)
    call jedetr(tmpori)
    call jedetr(tmpini)
!
    call jedema()
end subroutine
