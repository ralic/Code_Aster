subroutine pofaun()
    implicit none
!     ------------------------------------------------------------------
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
!     -----------------------------------------------------------------
!     COMMANDE POST_FATIGUE :
!              CHARGEMENT PUREMENT UNIAXIAL
!     -----------------------------------------------------------------
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/fgcoke.h"
#include "asterfort/fgcorr.h"
#include "asterfort/fgcota.h"
#include "asterfort/fgdoba.h"
#include "asterfort/fgdohs.h"
#include "asterfort/fgdoma.h"
#include "asterfort/fgdomm.h"
#include "asterfort/fgdowh.h"
#include "asterfort/fgordo.h"
#include "asterfort/fgpeak.h"
#include "asterfort/fgpic2.h"
#include "asterfort/fgrain.h"
#include "asterfort/fgrccm.h"
#include "asterfort/fgrmax.h"
#include "asterfort/fgtahe.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rccome.h"
#include "asterfort/rcpare.h"
#include "asterfort/rcvale.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbocc, ifonc, nbpts, i, n1, nbpapf, ifm, niv, nbp
    integer :: ivke, ivcorr, ivpoin, nbpoin, ivmax, ivmin, ivtrav
    integer :: ibid, intrav, ivpics, nbpics, nbcycl, nbpar, ivdome
    integer :: icodre(3), icodwo, ivmax1, ivmin1
    integer :: icodba, icodhs, icodma
    character(len=8) :: nomfon, result, txcum, k8b, nommat, kcorre, cara
    character(len=8) :: method, nompar, nomres(3)
    character(len=16) :: pheno, phenom, kdomm, nomcmd, methd1
    character(len=24) :: fvale
    real(kind=8) :: r8b, pseuil, rdomm, val(3), rampl
    complex(kind=8) :: cbid
    logical :: lhaigh, fateps, lke
!     --- POST_FATI_UNIAX ----------------------------------------------
    parameter (nbpapf=5)
    character(len=1) :: typppf(nbpapf)
    character(len=16) :: nomppf(nbpapf)
    data nomppf/'CYCLE','VALE_MIN','VALE_MAX','DOMMAGE','DOMM_CUMU'/
    data typppf/'I','R','R','R','R'/
!     ------------------------------------------------------------------
!
    call jemarq()
!
    r8b = 0.d0
    fateps = .false.
    lhaigh = .false.
    lke = .false.
    ivpics = 0
    ivke = 0
    ivcorr = 0
!
!     --- RECUPERATION DU NIVEAU D'IMPRESSION ---
!
    call infniv(ifm, niv)
!
    call getres(result, k8b, nomcmd)
!
!     --- RECUPERATION DE LA FONCTION CHARGEMENT ---
!
    nomfon = ' '
    call getvid('HISTOIRE', 'SIGM', iocc=1, scal=nomfon, nbret=n1)
    call getvid('HISTOIRE', 'EPSI', iocc=1, scal=nomfon, nbret=n1)
    if (n1 .ne. 0) fateps = .true.
!
    fvale = nomfon//'           .VALE'
    call jelira(fvale, 'LONMAX', nbpts)
    call jeveuo(fvale, 'L', ifonc)
    nbpts = nbpts/2
    call wkvect('&&POFAUN.FONC.POIN', 'V V R', nbpts, ivpoin)
!
!     --- IMPRESSION DE LA FONCTION ----
    if (niv .eq. 2) then
        write (ifm,'(1X,A)') 'VALEURS DE LA FONCTION CHARGEMENT:'
        do 10 i = 1, nbpts
            write (ifm,1000) zr(ifonc+i-1),zr(ifonc+nbpts+i-1)
10      continue
    endif
!
!
!     --- RECUPERATION DU COEFFICIENT D'AMPLIFICATION ---
    rampl = 1
    call getfac('COEF_MULT', nbocc)
    if (nbocc .ne. 0) then
        call getvr8('COEF_MULT', 'KT', iocc=1, scal=rampl, nbret=n1)
!        CALL FGAMPL(RAMPL,NBPTS,ZR(NBPTS+IFONC))
!
    endif
!
!     --- EXTRACTION DES PICS DE LA FONCTION DE CHARGEMENT ---
!
    call getvr8(' ', 'DELTA_OSCI', scal=pseuil, nbret=n1)
    call fgpeak(nomfon, pseuil, rampl, nbpoin, zr(ivpoin))
!
!     --- IMPRESSION DES PICS EXTRAITS DE LA FONCTION ----
    if (niv .eq. 2) then
        write (ifm,*)
        write (ifm,'(1X,A)') 'PICS EXTRAITS DE LA FONCTION CHARGEMENT'
        write (ifm,'(1X,A)') 'APRES AVOIR PRIS EN COMPTE DE KT'
        write (ifm,1010) pseuil,nbpoin
        write (ifm,*)
        write (ifm,'(4(1X,E18.6))') (zr(ivpoin+i-1),i=1,nbpoin)
    endif
!
!
!     ---RECUPERATION DE LA LOI DE COMPTAGES DE CYCLES
!
    call getvtx(' ', 'COMPTAGE', scal=methd1, nbret=n1)
    if (methd1(9:12) .ne. '_MAX') then
        method = methd1(1:8)
    else
        method = 'RFLO_MAX'
    endif
!
    call wkvect('&&POFAUN.SIGMAX', 'V V R', nbpoin+2, ivmax)
    call wkvect('&&POFAUN.SIGMIN', 'V V R', nbpoin+2, ivmin)
    call wkvect('&&POFAUN.SIGMAX1', 'V V R', nbpoin+2, ivmax1)
    call wkvect('&&POFAUN.SIGMIN1', 'V V R', nbpoin+2, ivmin1)
    call wkvect('&&POFAUN.POIN.TRAV', 'V V R', nbpoin+2, ivtrav)
    call wkvect('&&POFAUN.NUME.TRAV', 'V V I', 2* (nbpoin+2), intrav)
    if (method .eq. 'RAINFLOW') then
        call wkvect('&&POFAUN.FONC.PICS', 'V V R', nbpoin+2, ivpics)
        call fgpic2(method, zr(ivtrav), zr(ivpoin), nbpoin, zr(ivpics),&
                    nbpics)
        call fgrain(zr(ivpics), nbpics, zi(intrav), nbcycl, zr(ivmin),&
                    zr(ivmax))
    else if (method.eq.'RFLO_MAX') then
!
        call wkvect('&&POFAUN.FONC.PICS', 'V V R', nbpoin+2, ivpics)
        call fgpic2(method, zr(ivtrav), zr(ivpoin), nbpoin, zr(ivpics),&
                    nbpics)
        call fgrain(zr(ivpics), nbpics, zi(intrav), nbcycl, zr(ivmin1),&
                    zr(ivmax1))
!
        call fgrmax(nbcycl, zr(ivmin1), zr(ivmax1), zr(ivmin), zr(ivmax))
!
    else if (method.eq.'RCCM') then
        call fgordo(nbpoin, zr(ivpoin), zr(ivtrav))
        call fgrccm(nbpoin, zr(ivtrav), nbcycl, zr(ivmin), zr(ivmax))
    else if (method.eq.'NATUREL') then
        call fgcota(nbpoin, zr(ivpoin), nbcycl, zr(ivmin), zr(ivmax))
    else
        call utmess('F', 'FATIGUE1_15')
    endif
    if (nbcycl .eq. 0) then
        call utmess('F', 'FATIGUE1_16')
    endif
!
!     --- CORRECTION ELASTO-PLASTIQUE ---
!
    kcorre = ' '
    call getvtx(' ', 'CORR_KE', scal=kcorre, nbret=n1)
    call getvid(' ', 'MATER', scal=nommat, nbret=n1)
    if (kcorre .eq. 'RCCM') then
        nomres(1) = 'N_KE'
        nomres(2) = 'M_KE'
        nomres(3) = 'SM'
        nbpar = 0
        nompar = ' '
        call rcvale(nommat, 'RCCM', nbpar, nompar, [r8b],&
                    3, nomres, val, icodre, 2)
        call wkvect('&&POFAUN.KE', 'V V R', nbcycl, ivke)
        lke = .true.
        call fgcoke(nbcycl, zr(ivmin), zr(ivmax), val(1), val(2),&
                    val(3), zr(ivke))
    endif
!
!     --- CALCUL DU DOMMAGE ELEMENTAIRE ---
!
    kdomm = ' '
    call getvtx(' ', 'DOMMAGE', scal=kdomm, nbret=n1)
!
    call wkvect('&&POFAUN.DOMM.ELEM', 'V V R', nbcycl, ivdome)
!
!     --- CALCUL DU DOMMAGE ELEMENTAIRE DE WOHLER ---
!         ---------------------------------------
    if (kdomm .eq. 'WOHLER') then
!        ---CORRECTION DE HAIG (GOODMANN OU GERBER)
        kcorre = ' '
        call getvtx(' ', 'CORR_SIGM_MOYE', scal=kcorre, nbret=n1)
        if (kcorre .ne. ' ') then
            nomres(1) = 'SU'
            nbpar = 0
            nompar = ' '
            call rcvale(nommat, 'RCCM', nbpar, nompar, [r8b],&
                        1, nomres, val, icodre, 2)
            call wkvect('&&POFAUN.HAIG', 'V V R', nbcycl, ivcorr)
            lhaigh = .true.
            call fgcorr(nbcycl, zr(ivmin), zr(ivmax), kcorre, val(1),&
                        zr(ivcorr))
        endif
!
        pheno = 'FATIGUE'
        call rccome(nommat, pheno, phenom, icodre(1))
        if (icodre(1) .eq. 1) then
            call utmess('F', 'FATIGUE1_24')
        endif
        cara = 'WOHLER'
        call rcpare(nommat, pheno, cara, icodwo)
        cara = 'A_BASQUI'
        call rcpare(nommat, pheno, cara, icodba)
        cara = 'A0'
        call rcpare(nommat, pheno, cara, icodhs)
        if (icodwo .eq. 0) then
            call fgdowh(nommat, nbcycl, zr(ivmin), zr(ivmax), lke,&
                        zr(ivke), lhaigh, zr(ivcorr), zr(ivdome))
        else if (icodba.eq.0) then
            call fgdoba(nommat, nbcycl, zr(ivmin), zr(ivmax), lke,&
                        zr(ivke), lhaigh, zr(ivcorr), zr(ivdome))
        else if (icodhs.eq.0) then
            call fgdohs(nommat, nbcycl, zr(ivmin), zr(ivmax), lke,&
                        zr(ivke), lhaigh, zr(ivcorr), zr(ivdome))
        endif
!
!     --- CALCUL DU DOMMAGE ELEMENTAIRE DE MANSON_COFFIN ----
!         ----------------------------------------------
    else if (kdomm.eq.'MANSON_COFFIN') then
        if (.not.fateps) then
            call utmess('F', 'FATIGUE1_17')
        endif
        pheno = 'FATIGUE'
        call rccome(nommat, pheno, phenom, icodre(1))
        if (icodre(1) .eq. 1) then
            call utmess('F', 'FATIGUE1_24')
        endif
        cara = 'MANSON_C'
        call rcpare(nommat, pheno, cara, icodma)
        if (icodma .eq. 0) then
            call fgdoma(nommat, nbcycl, zr(ivmin), zr(ivmax), zr(ivdome))
        else
            call utmess('F', 'FATIGUE1_18')
        endif
!
!     --- CALCUL DU DOMMAGE ELEMENTAIRE DE TAHERI ---
!         ---------------------------------------
    else if (kdomm(1:6).eq.'TAHERI') then
        if (fateps) then
            call fgtahe(kdomm, nbcycl, zr(ivmin), zr(ivmax), zr(ivdome))
        else
            call utmess('F', 'FATIGUE1_19')
        endif
!
    else if (kdomm.eq.' ') then
    else
        call utmess('F', 'FATIGUE1_20')
    endif
!
!     --- CREATION DE LA TABLE ---
!
    call tbcrsd(result, 'G')
    call tbajpa(result, nbpapf, nomppf, typppf)
!
    nbp = 4
    if (kdomm .eq. ' ') nbp = 3
    do 20 i = 1, nbcycl
        val(1) = zr(ivmin+i-1)
        val(2) = zr(ivmax+i-1)
        val(3) = zr(ivdome+i-1)
        call tbajli(result, nbp, nomppf, i, val,&
                    cbid, k8b, 0)
20  end do
!
!     --- CALCUL DU DOMMAGE TOTAL ---
!
    txcum = ' '
    call getvtx(' ', 'CUMUL', scal=txcum, nbret=n1)
    if (txcum .eq. 'LINEAIRE') then
!
        call fgdomm(nbcycl, zr(ivdome), rdomm)
!
        call tbajli(result, 1, nomppf(5), ibid, rdomm,&
                    cbid, k8b, 0)
!
    endif
!
    call jedetr('&&POFAUN.FONC.POIN')
    call jedetr('&&POFAUN.SIGMAX')
    call jedetr('&&POFAUN.SIGMIN')
    call jedetr('&&POFAUN.SIGMAX1')
    call jedetr('&&POFAUN.SIGMIN1')
    call jedetr('&&POFAUN.POIN.TRAV')
    call jedetr('&&POFAUN.NUME.TRAV')
    call jedetr('&&POFAUN.DOMM.ELEM')
    if (ivpics .ne. 0) call jedetr('&&POFAUN.FONC.PICS')
    if (ivke .ne. 0) call jedetr('&&POFAUN.KE')
    if (ivcorr .ne. 0) call jedetr('&&POFAUN.HAIG')
!
    1000 format (2x,e18.6,5x,e18.6)
    1010 format (1x,'SEUIL = ',e18.6,10x,'NB DE PICS = ',i5)
!
    call jedema()
end subroutine
