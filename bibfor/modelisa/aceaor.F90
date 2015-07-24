subroutine aceaor(noma, nomo, lmax, nbepo, nbtel, ntyele, nomele, ivr, ifm, nbocc)
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
    use cara_elem_module
    implicit none
    integer :: lmax, nbepo, ntyele(*), ivr(*), nbocc(*)
    character(len=8) :: noma, nomo
    character(len=16) :: nomele(*)
!
! --------------------------------------------------------------------------------------------------
!
!     AFFE_CARA_ELEM
!     AFFECTATION DES CARACTERISTIQUES POUR LES ORIENTATIONS
!
! --------------------------------------------------------------------------------------------------
!
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : NOMO   : NOM DU MODELE
!
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
#include "jeveux.h"
#include "asterc/getres.h"
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
#include "blas/ddot.h"
!
! --------------------------------------------------------------------------------------------------
    integer :: ii, ifm, ioc, ixma, ixno, iarg
    integer :: jj, jad, jin, jdcmpo, jdco, jdgm, nbid
    integer :: jdls, jdme, jdne, jdno, jdori, jdtm, jinit
    integer :: jdvlvo, nbmagr, nbmail
    integer :: nbtel, nbval, ncar, ng
    integer :: nm, no1, no2, nocaor, ntpoi, ntseg, ntseg3, ntseg4
    integer :: nummai, nutyel, nutyma, nbalarme
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
    character(len=24) :: modnoe, modmai, nommai
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
    call jeexin(modnoe, ixno)
    if (ixma .ne. 0) call jeveuo(modmai, 'L', jdme)
    if (ixno .ne. 0) call jeveuo(modnoe, 'L', jdne)
! --------------------------------------------------------------------------------------------------
!   Récupération des adresses jeveux utiles
    call jeveuo(mlgtma, 'L', jdtm)
    call jeveuo(mlgcoo, 'L', jdco)
!
!   Récupération des numéros des types mailles poi1/seg2
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1'), ntpoi)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2'), ntseg)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG3'), ntseg3)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG4'), ntseg4)
!
!
    call wkvect('&&TMPORIEN', 'V V K24', lmax, jdls)
    call wkvect(tmpori, 'V V R', nbmail*3, jdori)
    call wkvect(tmpini, 'V V I', nbmail*3, jinit)
!
!   Initialisation des angles nautiques sur toutes les mailles non nulles. Repère local par défaut
    do ii = 1, nbmail*3
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
    if (nbocc(ACE_ORIENTATION) .ne. 0) then
        nbalarme = 0
        do ioc = 1, nbocc(ACE_ORIENTATION)
!           Pour les MAILLES
            call getvem(noma,'GROUP_MA','ORIENTATION','GROUP_MA',ioc,iarg,lmax,zk24(jdls),ng)
            call getvem(noma,'MAILLE',  'ORIENTATION','MAILLE',  ioc,iarg,lmax,zk24(jdls),nm)
!           Seuil correspondant à la longueur nulle pour une maille :
!               si seglong .LT. longseuil ==> maille de taille nulle
            call getvr8('ORIENTATION', 'PRECISION', iocc=ioc, scal=longseuil, nbret=nbid)
            if ( nbid .ne. 1 ) longseuil = -1.0d0
!           Pour les NOEUDS
            call getvtx('ORIENTATION', 'CARA', iocc=ioc, scal=oricara, nbret=ncar)
            call getvr8('ORIENTATION', 'VALE', iocc=ioc, nbval=nbval, vect=val, nbret=nval)
!           Dans le catalogue, si oricara == GENE_TUYAU c'est GROUP_NO ou NOEUD ==> Tuyaux
            if (ng .gt. 0) then
!               GROUP_MA = toutes les mailles possibles de la liste des groupes de mailles
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
            else if (nm .gt. 0) then
!               MAILLE = toutes les mailles possibles de la liste de mailles
                do ii = 1, nm
                    nommai = zk24(jdls+ii-1)
                    call jenonu(jexnom(mlgnma, nommai), nummai)
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
            endif
        enddo
        if (nbalarme.gt.0) call utmess('A','MODELISA_95',si=nbalarme,sr=longseuil)
    endif
!
! --------------------------------------------------------------------------------------------------
!   Impression des valeurs des orientations si demandé
    nocaor = 0
    if (ivr(3) .eq. 1) write(ifm,100)
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
                    write(ifm,110)nommai,nomele(jj),alpha,beta,gamma
                endif
                cycle cnum1
            endif
        enddo
    enddo cnum1
!
100 format(/,3x,'<ANGL> ORIENTATIONS SUR LES MAILLES DE TYPE POUTRE BARRE OU DISCRET',//,3x, &
                'NOM      TYPE             ALPHA         BETA          GAMMA')
110 format(3x,a8,1x,a16,1x,3(1pd13.6,2x))
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
