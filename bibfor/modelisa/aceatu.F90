subroutine aceatu(noma, nomo, nbepo, ntyele, ivr,&
                  ifm, nbocc)
    implicit none
#include "jeveux.h"
!
#include "asterc/getres.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/aceat2.h"
#include "asterfort/aceat3.h"
#include "asterfort/acemmt.h"
#include "asterfort/getvem.h"
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
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: nbepo, ntyele(*), nbocc(*), ivr(3), ifm
    character(len=8) :: noma, nomo
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
!     AFFECTATION DES CARACTERISTIQUES GEOMETRIQUES POUR LES TUYAUX
!     PAR CREATION D'UNE CARTE : NOMU//'.CAORTU' CONTENANT :
!        PGL1, PGL2, PGL3 : MATRICES DE PASSAGE REPERE LOCAL / GLOBAL
!        ICOUDE           : =0 SI ELEMENT DROIT =1 SI COUDE
!        DN1N2            : DISTANCE ENTRE LES DEUX SOMMETS
!        RCOURB           : RAYON DE COURBURE DU COUDE
!        ANGCOU           : ANGLE DU COUDE
!        ANGZZK           : ANGLE OMEGA ENTRE LA NORMALE AU PLAN DU
!                           COUDE ET LE VECTEUR ZK (GENERATRICE)
! ----------------------------------------------------------------------
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : NOMO   : NOM DU MODELE
! IN  : NBEPO  : NOMBRE DE TYPES D'ELEMENTS
! IN  : NTYELE : NUMEROS DES TYPES ELEMENTS
! IN  : IVR    : (3) = INFO 2
! IN  : IFM    : FICHIER MESSAGES
! IN  : NBOCC  : NBOCC(4) NB OCCURENCES ORIENTATION
! ----------------------------------------------------------------------
!
    integer :: iext1, iext2, ima, inn, ioc, jcozk, jdco, jdgn, jdno, jdme
    integer :: jelpar, jeltuy, jma, jnbmap, jnoex1, jnoex2, jnopar, jnozk
    integer :: nbext2, nbpart, nbtuy, ncar, ni1, ni2, nj, nj1, nj2, nn, nng
    integer :: numnoe, nutyel, nval, jsens, ixma, j
    integer :: jnotuy, nno, nbtuy4, nbext1, jzkpar, jmmt, ibid
    integer :: ier, nbmail
    real(kind=8) :: val(3), epsi
    character(len=8) :: nomu, car, crit
    character(len=16) :: concep, cmd, nunoel
    character(len=24) :: mlgnma, mlgnno, mlggno, mlgcoo, mlgcnx, modmai, nomlu
    character(len=24) :: nomnoe
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
    ier = 0
    call getres(nomu, concep, cmd)
!
! --- RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MAILLAGE ASSOCIE
!
    mlgnma = noma//'.NOMMAI'
    mlgnno = noma//'.NOMNOE'
    mlgcnx = noma//'.CONNEX'
    mlggno = noma//'.GROUPENO'
    mlgcoo = noma//'.COORDO    .VALE'
    call jelira(mlgnma, 'NOMMAX', nbmail)
    call jeveuo(mlgcoo, 'L', jdco)
!
    modmai = nomo//'.MAILLE'
    call jeexin(modmai, ixma)
    if (ixma .ne. 0) call jeveuo(modmai, 'L', jdme)
!
! --- COMPTAGE DES MET3SEG3
!
    nbtuy=0
    do 10 ima = 1, nbmail
        nutyel = zi(jdme+ima-1)
        do 12 j = 1, nbepo
            if (nutyel .eq. ntyele(j)) then
                call jenuno(jexnum('&CATA.TE.NOMTE', nutyel), nunoel)
                if ((nunoel.eq.'MET3SEG3') .or. ( nunoel.eq.'MET6SEG3') .or.&
                    (nunoel.eq.'MET3SEG4')) then
                    nbtuy=nbtuy+1
                endif
            endif
12      continue
10  end do
!
! --- STOCKAGE DES ELEMENTS MET3SEG3 ET DES NOEUDS
!
    call wkvect('&&ACEATU.NOTUY', 'V V I', nbtuy*4, jnotuy)
    call wkvect('&&ACEATU.ELTUY', 'V V I', nbtuy, jeltuy)
!
    nno=0
    nbtuy=0
    do 20 ima = 1, nbmail
        nutyel = zi(jdme+ima-1)
        do 22 j = 1, nbepo
            if (nutyel .eq. ntyele(j)) then
                call jenuno(jexnum('&CATA.TE.NOMTE', nutyel), nunoel)
                if ((nunoel.eq.'MET3SEG3') .or. ( nunoel.eq.'MET6SEG3')) then
                    nno=3
                    nbtuy=nbtuy+1
                    zi(jeltuy-1+nbtuy)=ima
                    call jeveuo(jexnum(mlgcnx, ima), 'L', jdno)
                    zi(jnotuy-1+3*nbtuy-2)=zi(jdno)
                    zi(jnotuy-1+3*nbtuy-1)=zi(jdno+1)
                    zi(jnotuy-1+3*nbtuy )=zi(jdno+2)
                endif
            endif
22      continue
20  end do
!
    nbtuy4=0
    do 30 ima = 1, nbmail
        nutyel = zi(jdme+ima-1)
        do 32 j = 1, nbepo
            if (nutyel .eq. ntyele(j)) then
                call jenuno(jexnum('&CATA.TE.NOMTE', nutyel), nunoel)
                if (nunoel .eq. 'MET3SEG4') then
                    nno=4
                    nbtuy4=nbtuy4+1
                    zi(jeltuy-1+nbtuy4)=ima
                    call jeveuo(jexnum(mlgcnx, ima), 'L', jdno)
                    zi(jnotuy-1+4*nbtuy4-3)=zi(jdno)
                    zi(jnotuy-1+4*nbtuy4-2)=zi(jdno+1)
                    zi(jnotuy-1+4*nbtuy4-1)=zi(jdno+2)
                    zi(jnotuy-1+4*nbtuy4 )=zi(jdno+3)
                endif
            endif
32      continue
30  end do
!
    if (nbtuy4 .ne. 0) then
        if (nbtuy .ne. 0) then
            call u2mess('F', 'MODELISA_27')
        else
            nbtuy = nbtuy4
        endif
    endif
!
! --- COMPTAGE DES PARTIES CONNEXES
!     HYPOTHESE : LES MAILLES SONT TOUTES ORIENTEES DANS LE MEME SENS
!
    nbext1=0
    nbext2=0
    do 40 ima = 1, nbtuy
        iext1=0
        iext2=0
        ni1 = zi(jnotuy-1+nno*(ima-1)+1)
        ni2 = zi(jnotuy-1+nno*(ima-1)+2)
        do 42 jma = 1, nbtuy
            if (jma .ne. ima) then
                nj1 = zi(jnotuy-1+nno*(jma-1)+1)
                nj2 = zi(jnotuy-1+nno*(jma-1)+2)
                if (ni1 .eq. nj2) then
                    iext1=1
                endif
                if (ni2 .eq. nj1) then
                    iext2=1
                endif
            endif
42      continue
        if (iext1 .eq. 0) then
            nbext1=nbext1+1
        endif
        if (iext2 .eq. 0) then
            nbext2=nbext2+1
        endif
40  end do
!      ASSERT(NBEXT1.EQ.NBEXT2)
    if (nbext1 .ne. nbext2) then
        call u2mess('F', 'MODELISA10_4')
    endif
    nbpart=nbext1
    if (ivr(3) .eq. 1) then
        write(ifm,*) 'NOMBRE DE PARTIES CONNEXES DE TUYAU : ',nbpart
    endif
!
! --- VERIFICATION ET STOCKAGE DES PARTIES CONNEXES
!     HYPOTHESE : LES MAILLES SONT TOUTES ORIENTEES DANS LE MEME SENS
!
    call wkvect('&&ACEATU.SENS', 'V V I', nbpart, jsens)
    call wkvect('&&ACEATU.NBMAPART', 'V V I', nbpart, jnbmap)
    call wkvect('&&ACEATU.NOEXT1', 'V V I', nbpart, jnoex1)
    call wkvect('&&ACEATU.NOEXT2', 'V V I', nbpart, jnoex2)
    call wkvect('&&ACEATU.LISMAPART', 'V V I', nbpart*nbtuy, jelpar)
    call wkvect('&&ACEATU.LISNOPART', 'V V I', nbpart*nbtuy*nno, jnopar)
    call wkvect('&&ACEATU.ZKPART', 'V V I', nbpart*nbtuy*nno, jzkpar)
    call aceat2(nbtuy, zi(jeltuy), zi(jnotuy), nbpart, zi(jnoex1),&
                zi(jnoex2), zi(jnbmap), zi(jelpar), zi(jnopar), nno)
!
!   LECTURE DE MODI_METRIQUE
!
    call wkvect('&&ACEATU.MMT', 'V V I', nbmail, jmmt)
    call acemmt(noma, zi(jmmt))
!
!     LECTURE DU MOT-CLE GENE_TUYAU
!
    inn=0
!
!     VALEURS PAR DEFAUT COHERENTES AVEC LE CATALOGUE
!
    epsi=1.d-4
    crit='RELATIF'
!
!     POUR NE PAS PASSER DES VARIABLES NON-INITIALISEES EN ARGUMENT
    jnozk = 1
    jcozk = 1
!
    if (nbocc(4) .ne. 0) then
        call wkvect('&&ACEATU.LISNOZK', 'V V I', nbocc(4), jnozk)
        call wkvect('&&ACEATU.LISCOZK', 'V V R', 3*nbocc(4), jcozk)
        do 50 ioc = 1, nbocc(4)
!
!         UN SEUL NOEUD PERMIS
!
            call getvem(noma, 'GROUP_NO', 'ORIENTATION', 'GROUP_NO', ioc,&
                        iarg, 1, nomlu, nj)
            call getvem(noma, 'NOEUD', 'ORIENTATION', 'NOEUD', ioc,&
                        iarg, 1, nomlu, nn)
            call getvtx('ORIENTATION', 'CARA', ioc, iarg, 1,&
                        car, ncar)
            call getvr8('ORIENTATION', 'VALE', ioc, iarg, 3,&
                        val, nval)
            call getvr8('ORIENTATION', 'PRECISION', ioc, iarg, 1,&
                        epsi, ibid)
            if (ibid .eq. 0) then
                epsi=1.d-4
                crit='RELATIF'
            endif
            call getvtx('ORIENTATION', 'CRITERE', ioc, iarg, 1,&
                        crit, ibid)
            if (car .eq. 'GENE_TUY') then
                if (nj .gt. 0) then
                    if (nj .eq. 1) then
                        call jeveuo(jexnom(mlggno, nomlu), 'L', jdgn)
                        call jelira(jexnom(mlggno, nomlu), 'LONUTI', nng)
                        if (nng .eq. 1) then
                            inn=inn+1
                            zi(jnozk-1+inn) = zi(jdgn)
                            zr(jcozk-1+3*inn-2)=val(1)
                            zr(jcozk-1+3*inn-1)=val(2)
                            zr(jcozk-1+3*inn )=val(3)
                        else
                            ier=1
                            goto 9998
                        endif
                    else
                        ier=1
                        goto 9998
                    endif
                endif
                if (nn .gt. 0) then
                    if (nn .eq. 1) then
                        nomnoe = nomlu
                        call jenonu(jexnom(mlgnno, nomnoe), numnoe)
                        inn=inn+1
                        zi(jnozk-1+inn) = numnoe
                        zr(jcozk-1+3*inn-2)=val(1)
                        zr(jcozk-1+3*inn-1)=val(2)
                        zr(jcozk-1+3*inn )=val(3)
                    else
                        ier=1
                        goto 9998
                    endif
                endif
            endif
50      continue
    endif
    call aceat3(noma, nomu, nbtuy, nbpart, zi(jnbmap),&
                zi(jelpar), zi(jnopar), ivr, ifm, inn,&
                zi(jnozk), zr(jcozk), zi(jsens), zr(jdco), epsi,&
                crit, nno, zi(jmmt))
!
9998  continue
    if (ier .ne. 0) then
        call u2mess('F', 'MODELISA_28')
    endif
!
!
! --- MENAGE
!
    call jedetr('&&ACEATU.NOTUY')
    call jedetr('&&ACEATU.ELTUY')
    call jedetr('&&ACEATU.SENS')
    call jedetr('&&ACEATU.NBMAPART')
    call jedetr('&&ACEATU.NOEXT1')
    call jedetr('&&ACEATU.NOEXT2')
    call jedetr('&&ACEATU.LISMAPART')
    call jedetr('&&ACEATU.LISNOPART')
    call jedetr('&&ACEATU.ZKPART')
    call jedetr('&&ACEATU.MMT')
    call jedetr('&&ACEATU.LISNOZK')
    call jedetr('&&ACEATU.LISCOZK')
!
    call jedema()
end subroutine
