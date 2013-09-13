subroutine pofape()
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/anacri.h"
#include "asterfort/avgrno.h"
#include "asterfort/dtauno.h"
#include "asterfort/fgdoba.h"
#include "asterfort/fgdohs.h"
#include "asterfort/fgdowh.h"
#include "asterfort/fmcros.h"
#include "asterfort/fmpapa.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
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
#include "asterfort/tbnuli.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!     ------------------------------------------------------------------
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
!     -----------------------------------------------------------------
!     COMMANDE POST_FATIGUE
!              CHARGEMENT PERIODIQUE
!     -----------------------------------------------------------------
!
    integer :: n1, n2, n3, n4, n5, n6, nbf, nbptot, nbpts, i, nbc, ibid, iordo
    integer :: ifonc1, ifonc, ilign, nbpar, nbpapf, j, nval, paract (30), nbeps
    integer :: ifonc2, ifonce, iordoe, ifonc3, ifoncp, iordop, nbepsp
    integer :: tdisp, nbnop, lisnoe(1), nbnot, nbordr, nnoini
    integer :: tspaq, k, jrwork, nbcmp, ordini
!
    real(kind=8) :: rbid, phmax, cissio, sphere, pcorr, val(2), vmax, vmin
    real(kind=8) :: domage, rcrit, vresu(24), resu(7), valpar(22)
    complex(kind=8) :: cbid
    logical :: lhaigh, lke, post, fordef, plcicr, lbid
    logical :: crsigm, crepst, crepse, crepsp
    integer :: icodre(2), icodwo, icodba, icodhs
    character(len=8) :: k8b, nomten(6), nomres(2), kdomm, nompar, nommat, cara
    character(len=8) :: result, nomeps(6), nomepp(6)
    character(len=16) :: nomcmd, pheno, phenom, criter, nomfor, typcha, forvie
    character(len=16) :: proaxe, nommet
    character(len=19) :: k19b
    character(len=24) :: fvale(6), etvale(6), ptvale(6)
!     --- POST_FATI_MULT -----------------------------------------------
    parameter    ( nbpapf = 37  )
    character(len=3) :: typppf(nbpapf)
    character(len=16) :: nomppf(nbpapf)
    data  nomppf / 'CRITERE' , 'VALE_CRITERE' , 'PRES_HYDRO_MAX' ,&
     &               'AMPLI_CISSION' , 'RAYON_SPHERE'   ,&
     &               'VALE_MIN' , 'VALE_MAX' , 'DOMMAGE' ,'NBRUP',&
     &               'DTAUMA', 'PHYDRM', 'NORMAX', 'NORMOY',&
     &               'EPNMAX', 'EPNMOY', 'DEPSPE', 'EPSPR1',&
     &               'SIGNM1', 'DENDIS', 'DENDIE', 'APHYDR',&
     &               'MPHYDR', 'DSIGEQ', 'SIGPR1', 'EPSNM1',&
     &               'INVA2S', 'DSITRE', 'DEPTRE', 'EPSPAC',&
     &               'RAYSPH', 'AMPCIS',&
     &               'VNM1X',  'VNM1Y', 'VNM1Z',&
     &               'VNM2X',  'VNM2Y',  'VNM2Z'  /
!
!
    data  typppf /  'K16' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R',&
     &                  'R' , 'R' , 'R' , 'R' , 'R' , 'R',  'R',&
     &                  'R' , 'R' , 'R' , 'R' , 'R' , 'R',  'R',&
     &                  'R' , 'R' , 'R' , 'R' , 'R' , 'R',  'R',&
     &                  'R',  'R',  'R' , 'R' , 'R' , 'R' , 'R' , 'R'/
!
!     ---------------------------------------------------------------
!     ----------------------------------------------------------------
!
    call jemarq()
!
    lke = .false.
    lhaigh = .false.
    nbc = 1
!
!
    call getres(result, k8b, nomcmd)
!
!     --- DETERMINATION DES CRITERES---
!
    criter = ' '
    call getvtx(' ', 'CRITERE', scal=criter, nbret=n1)
!
    typcha = ' '
    call getvtx(' ', 'TYPE_CHARGE', scal=typcha, nbret=n1)
!
    call getvid(' ', 'FORMULE_GRDEQ', scal=nomfor, nbret=nval)
    if (nval .eq. 0) then
        nomfor = '        '
    endif
!
    call getvid(' ', 'FORMULE_VIE', scal=forvie, nbret=nval)
    if (nval .eq. 0) then
        forvie = '        '
    endif
!
    kdomm = ' '
    call getvtx(' ', 'DOMMAGE', scal=kdomm, nbret=n1)
!
! ---   NOM DE LA METHODE PERMETTANT DE DETERMINER LE CERCLE CIRCONSCRIT
    call getvtx(' ', 'METHODE', scal=nommet, nbret=nval)
    if (nval .eq. 0) then
        nommet = '        '
    endif
!
! ---   PROJECTION SUR UN AXE OU SUR DEUX AXES
!     (CHARGEMENT NON_PERIODIQUE UNIQUEMENT)
    call getvtx(' ', 'PROJECTION', scal=proaxe, nbret=nval)
    if (nval .eq. 0) then
        proaxe = '        '
    endif
!
!---    ANALYSER LE CRITERE
!  INITIALISER
    crsigm = .false.
    crepst = .false.
    crepse = .false.
    crepsp = .false.
!
    call anacri(criter, nomfor, typcha, 'OUI', paract,&
                lbid, crsigm, crepst, crepse, crepsp)
!     --- RECUPERATION DE LA FONCTION CHARGEMENT ---
!
!CCCCCCCCC RECUPERER LA CONTRAINTE
    call getvid('HISTOIRE', 'SIGM_XX', iocc=1, scal=nomten(1), nbret=n1)
    call getvid('HISTOIRE', 'SIGM_YY', iocc=1, scal=nomten(2), nbret=n2)
    call getvid('HISTOIRE', 'SIGM_ZZ', iocc=1, scal=nomten(3), nbret=n3)
    call getvid('HISTOIRE', 'SIGM_XY', iocc=1, scal=nomten(4), nbret=n4)
    call getvid('HISTOIRE', 'SIGM_XZ', iocc=1, scal=nomten(5), nbret=n5)
    call getvid('HISTOIRE', 'SIGM_YZ', iocc=1, scal=nomten(6), nbret=n6)
    nbf = n1 + n2 + n3 + n4 + n5 + n6
!
    if (nbf .ne. 0) then
        fvale(1) = nomten(1)//'           .VALE'
        call jelira(fvale(1), 'LONMAX', nbpts)
    endif
!
!CCCCCCCCC RECUPERER LA DEFORMATION TOTALE
    call getvid('HISTOIRE', 'EPS_XX', iocc=1, scal=nomeps(1), nbret=n1)
    call getvid('HISTOIRE', 'EPS_YY', iocc=1, scal=nomeps(2), nbret=n2)
    call getvid('HISTOIRE', 'EPS_ZZ', iocc=1, scal=nomeps(3), nbret=n3)
    call getvid('HISTOIRE', 'EPS_XY', iocc=1, scal=nomeps(4), nbret=n4)
    call getvid('HISTOIRE', 'EPS_XZ', iocc=1, scal=nomeps(5), nbret=n5)
    call getvid('HISTOIRE', 'EPS_YZ', iocc=1, scal=nomeps(6), nbret=n6)
    nbeps = n1 + n2 + n3 + n4 + n5 + n6
!
    if (nbeps .ne. 0) then
        etvale(1) = nomeps(1)//'           .VALE'
        call jelira(etvale(1), 'LONMAX', nbpts)
    endif
!
!CCCCCCCCC RECUPERER LA DEFORMATION PLASTIQUE
    call getvid('HISTOIRE', 'EPSP_XX', iocc=1, scal=nomepp(1), nbret=n1)
    call getvid('HISTOIRE', 'EPSP_YY', iocc=1, scal=nomepp(2), nbret=n2)
    call getvid('HISTOIRE', 'EPSP_ZZ', iocc=1, scal=nomepp(3), nbret=n3)
    call getvid('HISTOIRE', 'EPSP_XY', iocc=1, scal=nomepp(4), nbret=n4)
    call getvid('HISTOIRE', 'EPSP_XZ', iocc=1, scal=nomepp(5), nbret=n5)
    call getvid('HISTOIRE', 'EPSP_YZ', iocc=1, scal=nomepp(6), nbret=n6)
    nbepsp = n1 + n2 + n3 + n4 + n5 + n6
!
    if (nbepsp .ne. 0) then
        ptvale(1) = nomepp(1)//'           .VALE'
        call jelira(ptvale(1), 'LONMAX', nbpts)
    endif
!
!C  CONTRUIRE TABLEAU CONTRAINTE
    if (nbf .eq. 0) then
        if (crsigm) then
            call u2mess('F', 'FATIGUE1_97')
        endif
        call wkvect('&&POFAPE.ORDO', 'V V R', nbpts/2*6, iordo)
    else
!
        nbptot = nbpts
        do 20 i = 2, nbf
            fvale(i) = nomten(i)//'           .VALE'
            call jelira(fvale(i), 'LONMAX', nbpts)
            if (nbpts .ne. nbptot) call u2mess('F', 'FATIGUE1_21')
20      continue
        call wkvect('&&POFAPE.ORDO', 'V V R', nbptot/2*nbf, iordo)
        call jeveuo(fvale(1), 'L', ifonc1)
        do 30 i = 2, nbf
            call jeveuo(fvale(i), 'L', ifonc)
            do 35 j = 1, nbptot/2
                if (zr(ifonc+j-1) .ne. zr(ifonc1+j-1)) then
                    call u2mess('F', 'FATIGUE1_21')
                endif
                zr(iordo+(j-1)*nbf+i-1) = zr(ifonc+nbptot/2+j-1)
35          continue
30      continue
        nbptot = nbptot / 2
        do 40 j = 1, nbptot
            zr(iordo+(j-1)*nbf) = zr(ifonc1+nbptot+j-1)
!
40      end do
!
    endif
!
!C  CONTRUIRE TABLEAU DEFORMATION TOTALE
    if (nbeps .eq. 0) then
        if (crepst) then
            call u2mess('F', 'FATIGUE1_98')
        endif
        call wkvect('&&POFAPE.ORDOE', 'V V R', nbpts/2*6, iordoe)
    else
!
        nbptot = nbpts
        do 21 i = 2, nbeps
            etvale(i) = nomeps(i)//'           .VALE'
            call jelira(etvale(i), 'LONMAX', nbpts)
            if (nbpts .ne. nbptot) call u2mess('F', 'FATIGUE1_21')
21      continue
        call wkvect('&&POFAPE.ORDOE', 'V V R', nbptot*nbeps/2, iordoe)
        call jeveuo(etvale(1), 'L', ifonc2)
        do 31 i = 2, nbeps
            call jeveuo(etvale(i), 'L', ifonce)
            do 36 j = 1, nbptot/2
                if (zr(ifonce+j-1) .ne. zr(ifonc2+j-1)) then
                    call u2mess('F', 'FATIGUE1_21')
                endif
                zr(iordoe+(j-1)*nbeps+i-1) = zr(ifonce+nbptot/2+j-1)
36          continue
31      continue
        nbptot = nbptot / 2
        do 41 j = 1, nbptot
            zr(iordoe+(j-1)*nbeps) = zr(ifonc2+nbptot+j-1)
41      continue
    endif
!
!
!C  CONTRUIRE TABLEAU DEFORMATION PLASTIQUE
!
    if (nbepsp .eq. 0) then
        if (crepsp) then
            call u2mess('F', 'FATIGUE1_99')
        endif
        call wkvect('&&POFAPE.ORDOP', 'V V R', nbpts/2*6, iordop)
    else
!
        nbptot = nbpts
        do 22 i = 2, nbepsp
            ptvale(i) = nomepp(i)//'           .VALE'
            call jelira(ptvale(i), 'LONMAX', nbpts)
            if (nbpts .ne. nbptot) call u2mess('F', 'FATIGUE1_21')
22      continue
        call wkvect('&&POFAPE.ORDOP', 'V V R', nbptot*nbepsp/2, iordop)
        call jeveuo(ptvale(1), 'L', ifonc3)
        do 32 i = 2, nbepsp
            call jeveuo(ptvale(i), 'L', ifoncp)
            do 37 j = 1, nbptot/2
                if (zr(ifoncp+j-1) .ne. zr(ifonc3+j-1)) then
                    call u2mess('F', 'FATIGUE1_21')
                endif
                zr(iordop+(j-1)*nbeps+i-1) = zr(ifoncp+nbptot/2+j-1)
37          continue
32      continue
        nbptot = nbptot / 2
        do 42 j = 1, nbptot
            zr(iordop+(j-1)*nbeps) = zr(ifonc3+nbptot+j-1)
42      continue
    endif
!
!CC  RECUPERER LE MATERIAU
!
    nommat = ' '
    call getvid(' ', 'MATER', scal=nommat, nbret=n1)
!
    if (crepse) then
        if ((nbeps + nbepsp) .eq. 0) then
            call u2mess('F', 'FATIGUE1_95')
        endif
        if ((nbeps + nbepsp) .gt. 0) then
            call u2mess('A', 'FATIGUE1_96')
        endif
    endif
!
!
!     --- CREATION DE LA TABLE ---
!
    call tbcrsd(result, 'G')
    call tbajpa(result, nbpapf, nomppf, typppf)
!
!
!
!CCCCCCCCCCCCCCCCCC
!
    call tbajli(result, 1, nomppf(1), ibid, rbid,&
                cbid, criter, 0)
    call tbnuli(result, 1, nomppf(1), ibid, rbid,&
                cbid, criter, rbid, k8b, ilign)
    if (ilign .le. 0) ilign = 0
!
!
!
    do 601 j = 1, 7
        resu(j) = 0.0d0
601  continue
!
    if (( criter .eq. 'FORMULE_CRITERE' ) .or. ( criter .eq. 'MATAKE_MODI_AV' ) .or.&
        ( criter .eq. 'DANG_VAN_MODI_AV' ) .or. ( criter .eq. 'FATESOCI_MODI_AV' ) .or.&
        ( criter .eq. 'MATAKE_MODI_AC' ) .or. ( criter .eq. 'DANG_VAN_MODI_AC' )) then
!
! ANALYSER LE CRITERE
        call anacri(criter, nomfor, typcha, 'OUI', paract,&
                    fordef, lbid, lbid, lbid, lbid)
        post = .true.
! CONS TRUIRE UN VECTEUR WORK QUI CONTIENT CONTRAINE ET DEFORMATION
        nbcmp = 6
!
        call wkvect('&&POFAPE.ORDOCD', 'V V R', nbptot*nbcmp*3, jrwork)
!
        do 60 j = 1, nbptot
            do 65 k = 1, 6
                zr(jrwork+(j-1)*nbcmp*3+k-1) = zr(iordo+(j-1)*nbcmp+k- 1)
                zr(jrwork+(j-1)*nbcmp*3 + nbcmp + k-1) = zr( iordoe+(j- 1 )*nbcmp+k-1 )
                zr(jrwork+(j-1)*nbcmp*3 + nbcmp*2 + k-1) = zr( iordop+(j-1 )*nbcmp+k-1 )
65          continue
60      continue
!
        tdisp = nbptot*nbcmp*3
        nbnot = 1
        lisnoe(1) = 1
        nbordr = nbpts/2
        nnoini = 1
        ordini = 1
        nbnop = 1
        tspaq = 18
        plcicr = .false.
!
! POUR CHARGEMENT PERIODIQUE
        if (typcha .eq. 'PERIODIQUE') then
!
            call dtauno(jrwork, lisnoe, nbnot, nbordr, ordini,&
                        nnoini, nbnop, tspaq, nommet, criter,&
                        nomfor, kdomm, forvie, k8b, k19b,&
                        nommat, post, valpar, vresu)
!
!
            if ((paract(1) .eq. 1) .or. (paract(3) .eq. 1) .or. (paract(4) .eq. 1) .or.&
                (paract(5) .eq. 1) .or. (paract(6) .eq. 1)) then
!
                plcicr = .true.
            endif
!
            if (plcicr) then
!
                call tbajli(result, 1, nomppf(10), ibid, vresu(1),&
                            cbid, k8b, ilign)
!
                do 46 i = 1, 3
                    call tbajli(result, 1, nomppf(i+31), ibid, vresu(i+1),&
                                cbid, k8b, ilign)
!
46              continue
!
                do 44 i = 1, 4
                    call tbajli(result, 1, nomppf(i+11), ibid, vresu(i+4),&
                                cbid, k8b, ilign)
!
44              continue
!
            else
! POUR LES GRANDEURS HORS DES CRITERES A PLAN CRITIQUE
                do 43 i = 1, 22
                    if (paract(i) .eq. 1) then
                        call tbajli(result, 1, nomppf(i+9), ibid, valpar(i),&
                                    cbid, k8b, ilign)
                    endif
43              continue
!
            endif
!
            call tbajli(result, 1, nomppf(2), ibid, vresu(9),&
                        cbid, k8b, ilign)
            call tbajli(result, 1, nomppf(9), ibid, vresu(10),&
                        cbid, k8b, ilign)
            call tbajli(result, 1, nomppf(8), ibid, vresu(11),&
                        cbid, k8b, ilign)
!
!
! POUR CHARGEMENT NON-PERIODIQUE
        else if (typcha .eq. 'NON_PERIODIQUE') then
!
            call avgrno(zr(jrwork), tdisp, lisnoe, nbnot, nbordr,&
                        nnoini, nbnop, tspaq, criter, nomfor,&
                        kdomm, forvie, fordef, k8b, proaxe,&
                        nommat, k19b, post, resu)
!
            call tbajli(result, 1, nomppf(32), ibid, resu(1),&
                        cbid, k8b, ilign)
            call tbajli(result, 1, nomppf(33), ibid, resu(2),&
                        cbid, k8b, ilign)
            call tbajli(result, 1, nomppf(34), ibid, resu(3),&
                        cbid, k8b, ilign)
            call tbajli(result, 1, nomppf(8), ibid, resu(4),&
                        cbid, k8b, ilign)
!
            call tbajli(result, 1, nomppf(35), ibid, resu(5),&
                        cbid, k8b, ilign)
            call tbajli(result, 1, nomppf(36), ibid, resu(6),&
                        cbid, k8b, ilign)
            call tbajli(result, 1, nomppf(37), ibid, resu(7),&
                        cbid, k8b, ilign)
!
        endif
!
        goto 50
    endif
!
!
    nomres(1) = 'D0'
    nomres(2) = 'TAU0'
    nbpar = 0
    nompar = ' '
    call rcvale(nommat, 'FATIGUE ', nbpar, nompar, [rbid],&
                2, nomres, val, icodre, 2)
!
    if (criter .eq. 'CROSSLAND') then
!          -----------------------
        call fmcros(nbf, nbptot, zr(iordo), val(1), val(2),&
                    rcrit, phmax, cissio)
!
        call tbajli(result, 1, nomppf(2), ibid, rcrit,&
                    cbid, k8b, ilign)
        call tbajli(result, 1, nomppf(3), ibid, phmax,&
                    cbid, k8b, ilign)
        call tbajli(result, 1, nomppf(4), ibid, cissio,&
                    cbid, k8b, ilign)
!
    else if (criter .eq. 'PAPADOPOULOS') then
!              --------------------------
        call fmpapa(nbf, nbptot, zr(iordo), val(1), val(2),&
                    rcrit, phmax, sphere)
!
        call tbajli(result, 1, nomppf(2), ibid, rcrit,&
                    cbid, k8b, ilign)
        call tbajli(result, 1, nomppf(3), ibid, phmax,&
                    cbid, k8b, ilign)
        call tbajli(result, 1, nomppf(5), ibid, sphere,&
                    cbid, k8b, ilign)
!
    endif
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
    if (criter .ne. 'FORMULE_CRITERE') then
!     --- CORRECTION POUR CALCUL DU DOMMAGE ----
!
        call getvr8(' ', 'COEF_CORR', scal=pcorr, nbret=n1)
        if (n1 .ne. 0) then
            vmax = 2.d0*(rcrit+val(2))*pcorr
            vmin = 0.d0
        else
            vmax = 2.d0*(rcrit+val(2))*(val(1)/val(2))
            vmin = 0.d0
        endif
        call tbajli(result, 1, nomppf(6), ibid, vmin,&
                    cbid, k8b, ilign)
        call tbajli(result, 1, nomppf(7), ibid, vmax,&
                    cbid, k8b, ilign)
!
!         --- CALCUL DU DOMMAGE ELEMENTAIRE ---
!
!
!         --- CALCUL DU DOMMAGE ELEMENTAIRE DE WOHLER ---
!             ---------------------------------------
        if (kdomm .eq. 'WOHLER') then
            pheno = 'FATIGUE'
            call rccome(nommat, pheno, phenom, icodre(1))
            if (icodre(1) .eq. 1) call u2mess('F', 'FATIGUE1_24')
            cara = 'WOHLER'
            call rcpare(nommat, pheno, cara, icodwo)
            cara = 'A_BASQUI'
            call rcpare(nommat, pheno, cara, icodba)
            cara = 'A0'
            call rcpare(nommat, pheno, cara, icodhs)
            if (icodwo .eq. 0) then
                call fgdowh(nommat, nbc, vmin, vmax, lke,&
                            rbid, lhaigh, rbid, domage)
            else if (icodba .eq. 0) then
                call fgdoba(nommat, nbc, vmin, vmax, lke,&
                            rbid, lhaigh, rbid, domage)
            else if (icodhs .eq. 0) then
                call fgdohs(nommat, nbc, vmin, vmax, lke,&
                            rbid, lhaigh, rbid, domage)
            endif
!
            call tbajli(result, 1, nomppf(8), ibid, domage,&
                        cbid, k8b, ilign)
!
        else if (kdomm .eq. ' ') then
        else
            call u2mess('F', 'FATIGUE1_20')
        endif
!
    endif
!
50  continue
!
    call jedetr('&&POFAPE.ORDO')
    call jedema()
!
end subroutine
