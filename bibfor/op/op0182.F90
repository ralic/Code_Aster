subroutine op0182()
! aslint: disable=
    implicit none
!-----------------------------------------------------------------------
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
!
!     OPERATEUR  "MODI_OBSTACLE"
!
!     DEFINITION DES VARIABLES
!        ETUBE : EPAISSEUR DU TUBE
!        RTUBE : RAYON EXTERIEURE DU TUBE
!        ROBST : RAYON INTERIEURE DE L'OBSTACLE
!        HOBST : HAUTEUR DE L'OBSTACLE
!
! ----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8pi.h"
#include "asterfort/assert.h"
#include "asterfort/calfig.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/morevu.h"
#include "asterfort/mousto.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/tbliva.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: ibid, ns, dimtub, dimobs, ifm, i, idray, idtob, idrob, idthe
    integer :: irett, jtubus, jobsus, n1, nis, nc, nco, ncr, npu, nr, niv, npo
    integer :: ireuse, iret2, nbpara, lpro
    parameter   ( nbpara = 10 )
    real(kind=8) :: arete, arete2, sect(20), voltub(20), tabr(nbpara)
    real(kind=8) :: volobs(20), rtube, robst, jeui, vust(20), vuso(20), rad, pi
    real(kind=8) :: perce, r8b, susetu, rint, etube, denc, dinst, hobst, sinit
    real(kind=8) :: suseob, svoobs, svotub
    complex(kind=8) :: c16b
    character(len=8) :: k8b, guidag, guide, obcray, typara(nbpara), k8typ
    character(len=19) :: tabpus
    character(len=16) :: concep, nomcmd, nomobs, nomob1, nopara(nbpara)
    character(len=16) :: nopar1(nbpara)
    character(len=19) :: resu
    character(len=24) :: type, tabk(nbpara), nomfon, nomf, typini, nomfg, typobc
    character(len=24) :: nomobc
!
    data nopara / 'LIEU'    , 'SECTEUR' , 'TYPE'    , 'ANGL_DEBUT',&
     &              'ANGL_FIN', 'ANGL_MAX', 'PROF_MAX', 'SURF_INIT' ,&
     &              'SURF_USE', 'FONCTION' /
    data typara / 'K8'      , 'I'         , 'K8'        , 'R'       ,&
     &              'R'       , 'R'         , 'R'         , 'R'       ,&
     &              'R'       , 'K24'       /
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call getres(resu, concep, nomcmd)
    nomfon = resu(1:8)//'_INITIAL'
!
    pi = r8pi( )
    rad = r8dgrd()
!
    call wkvect('&&OP0182.TUBUSE', 'V V R', 3000, jtubus)
    call wkvect('&&OP0182.OBSUSE', 'V V R', 3000, jobsus)
!
    call infmaj()
    call infniv(ifm, niv)
!
!     ------------------------------------------------------------------
!                       CREATION DE LA TABLE
!     ------------------------------------------------------------------
!
    call jeexin(resu//'.TBLP', ireuse)
    if (ireuse .ne. 0) then
!     SI REENTRANT ON CONSERVE LA DESCRIPTION ET LE TYPE DE L'OBSTACLE
        call tbliva(resu, 1, 'LIEU', [ibid], [r8b],&
                    [c16b], 'DEFIOBST', k8b, [r8b], 'FONCTION',&
                    k8typ, ibid, r8b, c16b, nomf,&
                    irett)
        call tbliva(resu, 1, 'LIEU', [ibid], [r8b],&
                    [c16b], 'DEFIOBST', k8b, [r8b], 'TYPE',&
                    k8typ, ibid, r8b, c16b, typini,&
                    iret2)
        if (irett .ne. 0 .or. iret2 .ne. 0) then
            call utmess('F', 'PREPOST4_96')
        endif
        call copisd('FONCTION', 'V', nomf, '&&OP0182.REUSE.NOMF')
        call detrsd('TABLE', resu)
        call copisd('FONCTION', 'G', '&&OP0182.REUSE.NOMF', nomfon)
    else
!     SI PAS REENTRANT : TYPE DISCRET, INITIALISATION DE NOMFON
        typini = 'DISCRET'
        ASSERT(lxlgut(nomfon).le.24)
        call wkvect(nomfon(1:19)//'.PROL', 'G V K24', 6, lpro)
        zk24(lpro) = 'FONCTION'
        zk24(lpro+1) = 'LINLIN'
        zk24(lpro+2) = 'THETA'
        zk24(lpro+3) = 'R'
        zk24(lpro+4) = 'EE'
        zk24(lpro+5) = nomfon
        npo = 721
        call wkvect(nomfon(1:19)//'.VALE', 'G V R', npo*2, idtob)
        idrob = idtob + npo
        jeui = 5.d-4
        do 20 i = 1, npo
            zr(idrob+i-1) = jeui
            zr(idtob+i-1) = (i-1)*rad*360.d0/(npo-1)
20      continue
    endif
    call tbcrsd(resu, 'G')
    call tbajpa(resu, nbpara, nopara, typara)
!
! --- INSERTION DE LA LIGNE DE DESCRIPTION DANS LA TABLE
    nopar1(1) = 'LIEU'
    nopar1(2) = 'TYPE'
    nopar1(3) = 'FONCTION'
    tabk(1) = 'DEFIOBST'
    tabk(2) = typini
    tabk(3) = nomfon
    call tbajli(resu, 3, nopar1, [ibid], [r8b],&
                [c16b], tabk, 0)
!
!     utiliser uniquement par CALFIG
    call jelira(nomfon(1:19)//'.VALE', 'LONMAX', npo)
!
    ns = 12
    dinst = 0.d0
    do 10 i = 1, ns
        sect(i) = (i-1)*360.d0/ns
        voltub(i) = 0.d0
        volobs(i) = 0.d0
10  end do
!
!     ------------------------------------------------------------------
!            LES VOLUMES D'USURE TUBE ET OBST PAR SECTEUR
!     ------------------------------------------------------------------
!
    call getvid(' ', 'TABL_USURE', scal=tabpus, nbret=npu)
    if (npu .ne. 0) then
        call morevu(tabpus, dinst, ns, sect, voltub,&
                    volobs)
    endif
!
    call getvr8(' ', 'V_USUR_TUBE', nbval=0, nbret=nis)
    if (nis .ne. 0) then
        ns = -nis
        if (ns .ne. 10 .and. ns .ne. 12) then
            call utmess('F', 'PREPOST3_63')
        endif
        call getvr8(' ', 'V_USUR_TUBE', nbval=ns, vect=vust, nbret=nis)
        call getvr8(' ', 'V_USUR_OBST', nbval=ns, vect=vuso, nbret=nis)
        do 12 i = 1, ns
            sect(i) = (i-1)*360.d0/ns
            voltub(i) = vust(i)
            volobs(i) = vuso(i)
12      continue
    endif
!
    sect(ns+1) = 360.d0
!
!     ------------------------------------------------------------------
!            REMPLACEMENT DU TUBE PERCE PAR UN TUBE NEUF
!     ------------------------------------------------------------------
!
    call getvr8(' ', 'PERCEMENT', scal=perce, nbret=nis)
!
!     ------------------------------------------------------------------
!          PARAMETRES POUR L'USURE DES OBSTACLES EN FONCTION DE LA
!                    HAUTEUR DE LA CARTE OU DU GUIDAGE
!     ------------------------------------------------------------------
!
    call getvid(' ', 'GUIDE', scal=guide, nbret=n1)
!
    call tbliva(guide, 1, 'LIEU', [ibid], [r8b],&
                [c16b], 'DEFIOBST', k8b, [r8b], 'TYPE',&
                k8typ, ibid, r8b, c16b, type,&
                irett)
    call tbliva(guide, 1, 'LIEU', [ibid], [r8b],&
                [c16b], 'DEFIOBST', k8b, [r8b], 'FONCTION',&
                k8typ, ibid, r8b, c16b, nomfg,&
                iret2)
    if (irett .ne. 0 .or. iret2 .ne. 0) then
        call utmess('F', 'PREPOST4_96')
    endif
    call jelira(nomfg(1:19)//'.VALE', 'LONMAX', nco)
    nco = nco/2
    nomob1 = type(1:7)
!
! --- CAS DES DISCRETS, EN SUPPOSANT QUE LE RAYON EST CONSTANT
!     SI PAS CONSTANT LE CALCUL DE L'USURE EST FAUX
!
    if (nomob1 .eq. 'DISCRET') then
        call jeveuo(nomfg(1:19)//'.VALE', 'L', idthe)
        idray = idthe + nco
        hobst = 0.d0
        robst = zr(idray)
        nomobs = nomob1
        nomobs = nomob1
    else
        nomobs = type(8:12)
    endif
!
    denc = 3.05d-3
!
    if (nomobs .eq. 'CARSP') then
!         -----------------
        robst = 5.59d-3
        hobst = 18.d-3
        if (type(1:6) .eq. 'GUID_E') hobst = 11.d-3
!
    else if (nomobs.eq.'CARTE') then
!             -----------------
        if (type(14:17) .eq. '1300') robst = 5.34d-3
        if (type(14:16) .eq. '900') robst = 5.325d-3
        hobst = 18.d-3
        if (type(1:6) .eq. 'GUID_E') hobst = 11.d-3
!
    else if (nomobs.eq.'GCONT') then
!             -----------------
        if (type(6:6) .eq. 'B' .or. type(6:6) .eq. 'D') denc = 3.175d-3
        if (type(14:17) .eq. '1300') then
            robst = 5.44d-3
            hobst = 16.7d-3
        else if (type(14:16).eq.'900') then
            robst = 5.425d-3
            hobst = 10.d-3
        endif
    endif
!
    if (type(1:6) .eq. 'GUID_A' .or. type(1:6) .eq. 'GUID_B' .or. type(1:6) .eq. 'GUID_C'&
        .or. type(1:6) .eq. 'GUID_D') then
        guidag = 'ENCO_1'
    else if (type(1:6).eq.'GUID_E' .or. type(1:6).eq.'GUID_F') then
        guidag = 'ENCO_2'
    endif
!
    if (nomobs .eq. 'GCOMB') then
        robst = 5.49d-3
        hobst = 12.d-3
        guidag = 'CERCLE'
    endif
!
    arete = asin(denc/robst)*180.d0/pi
    arete2 = 180.d0-arete
!
    call getvr8(' ', 'R_MOBILE', scal=rtube, nbret=nr)
    call getvid(' ', 'CRAYON', scal=obcray, nbret=nc)
    if (nr .eq. 0) then
        if (nc .eq. 0) then
            if (type(14:17) .eq. '1300') rtube = 4.84d-3
            if (type(14:16) .eq. '900') rtube = 4.825d-3
        else
            call tbliva(obcray, 1, 'LIEU', [ibid], [r8b],&
                        [c16b], 'DEFIOBST', k8b, [r8b], 'TYPE',&
                        k8typ, ibid, r8b, c16b, typobc,&
                        irett)
            call tbliva(obcray, 1, 'LIEU', [ibid], [r8b],&
                        [c16b], 'DEFIOBST', k8b, [r8b], 'FONCTION',&
                        k8typ, ibid, r8b, c16b, nomobc,&
                        iret2)
            ASSERT(irett.eq.0.and.iret2.eq.0)
            if (typobc(14:17) .eq. '1300') then
                rtube = 4.84d-3
                goto 61
            endif
            if (typobc(14:16) .eq. '900') then
                rtube = 4.825d-3
                goto 61
            endif
            call jelira(nomobc(1:19)//'.VALE', 'LONMAX', ncr)
            ncr = ncr/2
            guidag = ' '
61          continue
        endif
    endif
!
! --- CALCUL DE LA SURFACE INITIALE :
!     -----------------------------
!
    if (type(14:17) .eq. '1300') etube = 0.98d-3
    if (type(14:16) .eq. '900') etube = 0.47d-3
    rint = rtube-etube
    sinit = pi*((rtube*rtube)-(rint*rint))
!
    if (niv .ge. 2) then
        write(ifm,1020) rtube
        write(ifm,1021) etube
        write(ifm,1022) robst
        write(ifm,1023) hobst
        write(ifm,*) '  TYPE OBSTACLE: ', guidag
        write(ifm,1000)
        do 140 i = 1, ns
            write(ifm,1010) i, sect(i), sect(i+1), voltub(i), volobs(&
            i)
140      continue
    endif
    1000 format('==> IMPRESSION DES VOLUMES USES PAR SECTEUR:',/,'  SECT',&
     &   '   ANGL_DEBUT      ANGL_FIN      USURE_TUBE      USURE_OBST')
    1010 format(1p,2x,i2,3x,e12.5,3x,e12.5,3x,e12.5,3x,e12.5)
    1020 format(1p,'    TUBE RAYON EXT: ', e12.5 )
    1021 format(1p,'         EPAISSEUR: ', e12.5 )
    1022 format(1p,'    OBST RAYON INT: ', e12.5 )
    1023 format(1p,'           HAUTEUR: ', e12.5 )
!
!*********************************************************************
!
! ---  ENCO_1 , ENCO_2 , CERCLE
!
!*********************************************************************
!
    if (guidag .eq. 'ENCO_1' .or. guidag .eq. 'ENCO_2' .or. guidag .eq. 'CERCLE') then
!
        call mousto(guidag, dimtub, voltub, zr(jtubus), dimobs,&
                    volobs, zr(jobsus), rtube, robst, sect,&
                    arete, arete2, ns, guide, hobst,&
                    etube, resu, denc, perce)
!
!*********************************************************************
!
! --- AUTRE
!
!*********************************************************************
!
    else
        dimtub = ncr
        call jeveuo(nomobc(1:19)//'.VALE', 'L', idthe)
        idray = idthe + ncr
        do 101 i = 1, dimtub
            zr(jtubus+2*i-2) = zr(idthe+i-1)/rad
            zr(jtubus+2*i-1) = zr(idray+i-1)
101      continue
!
        dimobs = nco
        call jeveuo(nomfg(1:19)//'.VALE', 'L', idthe)
        idray = idthe + nco
        do 102 i = 1, dimobs
            zr(jobsus+2*i-2) = zr(idthe+i-1)/rad
            zr(jobsus+2*i-1) = zr(idray+i-1)
102      continue
    endif
!*********************************************************************
!
    call calfig(guidag, resu, dimobs, dimtub, zr(jobsus),&
                zr(jtubus))
!
!
!     CALCUL DES SECTIONS USEES SUR TUBE ET OBSTACLE :
!     ----------------------------------------------
!
    svotub = voltub(1)
    svoobs = volobs(1)
    do 103 i = 2, ns
        svotub = svotub + voltub(i)
        svoobs = svoobs + volobs(i)
103  end do
    susetu = svotub / hobst
    suseob = svoobs / hobst
!
    if (niv .ge. 1) then
        write(ifm,*)'SURFACE INITIALE TUBE =', sinit
        write(ifm,*)'SURFACE USEE     TUBE =', susetu
        write(ifm,*)'SURFACE USEE     OBST =', suseob
    endif
!
    nopar1(1) = nopara(1)
    nopar1(2) = nopara(8)
    nopar1(3) = nopara(9)
    tabr(1) = sinit
    tabr(2) = susetu
    tabk(1) = 'TUBE'
    call tbajli(resu, 3, nopar1, [ibid], tabr,&
                [c16b], tabk, 0)
    nopar1(1) = nopara(1)
    nopar1(2) = nopara(9)
    tabr(1) = suseob
    tabk(1) = 'OBST'
    call tbajli(resu, 2, nopar1, [ibid], tabr,&
                [c16b], tabk, 0)
!
!
    call jedema()
end subroutine
