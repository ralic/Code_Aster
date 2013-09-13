subroutine op0048()
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
!     ------------------------------------------------------------------
!     CALCUL MECANIQUE TRANSITOIRE PAR INTEGRATION DIRECTE
!     DIFFERENTS TYPES D'INTEGRATION SONT POSSIBLES:
!     - IMPLICITES :  THETA-WILSON
!                     NEWMARK
!     - EXPLICITE  :  DIFFERENCES CENTREES
!     ------------------------------------------------------------------
!
!  HYPOTHESES :                                                "
!  ----------   SYSTEME CONSERVATIF DE LA FORME  K.U    +    M.U = F
!           OU                                           '     "
!               SYSTEME DISSIPATIF  DE LA FORME  K.U + C.U + M.U = F
!
!     ------------------------------------------------------------------
!
    implicit none
!
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/r8vide.h"
#include "asterfort/cochre.h"
#include "asterfort/dismoi.h"
#include "asterfort/dladap.h"
#include "asterfort/dldiff.h"
#include "asterfort/dlnewi.h"
#include "asterfort/dltali.h"
#include "asterfort/dltlec.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecalc.h"
#include "asterfort/mecham.h"
#include "asterfort/mechti.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/u2mess.h"
#include "asterfort/vrcins.h"
#include "asterfort/vrcref.h"
#include "asterfort/wkvect.h"
    character(len=6) :: nompro
    parameter ( nompro = 'OP0048' )
!
    integer :: nveca, nchar
    integer :: imat(3), nume, niv, ibid, ifm, iondp, ladpa, numrep
    integer :: ialifo, iaadve, nondp, ifexte, ifamor, ifliai
    integer :: neq, idepl0, ivite0, iacce0, iwk, iordr
    integer :: iinteg, iret
    integer :: jord, nbord, jchar
    integer :: lresu, lcrre, iresu, nbexre, l
    integer :: nbchre, iocc, nfon
    real(kind=8) :: t0, time, rundf, alpha
    character(len=1) :: base, typcoe
    character(len=2) :: codret
    character(len=8) :: k8b, masse, rigid, amort, baseno, result
    character(len=8) :: materi, carael, kstr, nomfon, charep
    character(len=19) :: solveu, infcha, ligrel
    character(len=24) :: modele, carele, charge, fomult, mate
    character(len=24) :: numedd, chamgd
    character(len=24) :: infoch, criter
    character(len=24) :: chgeom, chcara(18), chharm, chtime
    character(len=24) :: chvarc, chvref, chstru, k24bla, compor
    complex(kind=8) :: calpha(2)
    character(len=19) :: force0, force1
!
    logical :: lamort, lcrea, lprem, exipou
!     -----------------------------------------------------------------
    data modele   /'                        '/
!                     123456789012345678901234
!     -----------------------------------------------------------------
!
    call jemarq()
    rundf=r8vide()
!
!====
! 1. LES DONNEES DU CALCUL
!====
!
! 1.1. ==> RECUPERATION DU NIVEAU D'IMPRESSION
!
    call getvis(' ', 'INFO', scal=niv, nbret=ibid)
    call infmaj()
!
    call infniv(ifm, niv)
!
! 1.2. ==> NOM DES STRUCTURES
!
    baseno = '&&'//nompro
!
!               12   345678   9012345678901234
    solveu = '&&'//nompro//'.SOLVEUR   '
    infcha = '&&'//nompro//'.INFCHA    '
    charge = '&&'//nompro//'.INFCHA    .LCHA'
    infoch = '&&'//nompro//'.INFCHA    .INFC'
    chvarc = '&&OP0048.VARC'
    chvref = '&&OP0048.VREF'
    alpha = 0.d0
    calpha = (0.d0 , 0.d0)
    nfon = 0
    typcoe = ' '
    charep = ' '
    chtime = ' '
    k24bla = ' '
    base = 'G'
!
!
    lprem = .true.
    lamort = .true.
    amort = ' '
    criter = '&&RESGRA_GCPC'
!
!====
! 2. LES DONNEES DU CALCUL
!====
!
    call dltlec(result, modele, numedd, materi, mate,&
                carael, carele, imat, masse, rigid,&
                amort, lamort, nchar, nveca, infcha,&
                charge, infoch, fomult, iaadve, ialifo,&
                nondp, iondp, solveu, iinteg, t0,&
                nume, baseno, numrep)
!
    neq = zi(imat(1)+2)
!
!====
! 3. CREATION DES VECTEURS DE TRAVAIL SUR BASE VOLATILE
!====
!
    call wkvect(baseno//'.DEPL0', 'V V R', neq, idepl0)
    call wkvect(baseno//'.VITE0', 'V V R', neq, ivite0)
    call wkvect(baseno//'.ACCE0', 'V V R', neq, iacce0)
    call wkvect(baseno//'.FEXTE', 'V V R', 2*neq, ifexte)
    call wkvect(baseno//'.FAMOR', 'V V R', 2*neq, ifamor)
    call wkvect(baseno//'.FLIAI', 'V V R', 2*neq, ifliai)
    call wkvect(baseno//'.TRAV', 'V V R', neq, iwk)
!
    call getfac('EXCIT_RESU', nbexre)
    if (nbexre .ne. 0) then
        call wkvect(baseno//'.COEF_RRE', 'V V R  ', nbexre, lcrre)
        call wkvect(baseno//'.LISTRESU', 'V V K8 ', nbexre, lresu)
        do 252 iresu = 1, nbexre
            call getvid('EXCIT_RESU', 'RESULTAT', iocc=iresu, scal=zk8( lresu+iresu-1), nbret=l)
            call getvr8('EXCIT_RESU', 'COEF_MULT', iocc=iresu, scal=zr(lcrre+iresu-1), nbret=l)
252      continue
    endif
!
!===
! 4. INITIALISATION DE L'ALGORITHME
!===
!
    force0 = '&&OP0048.FORCE0'
    force1 = '&&OP0048.FORCE1'
    call dltali(neq, result, imat, masse, rigid,&
                zi(iaadve), zk24(ialifo), nchar, nveca, lcrea,&
                lprem, lamort, t0, mate, carele,&
                charge, infoch, fomult, modele, numedd,&
                nume, solveu, criter, zr(idepl0), zr(ivite0),&
                zr(iacce0), zr(ifexte+neq), zr(ifamor+neq), zr(ifliai+neq), baseno,&
                zr(iwk), force0, force1)
!
!====
! 5. INTEGRATION SELON LE TYPE SPECIFIE
!====
!
    if (iinteg .eq. 1) then
!
        call dlnewi(result, force0, force1, lcrea, lamort,&
                    iinteg, neq, imat, masse, rigid,&
                    amort, zr(idepl0), zr(ivite0), zr(iacce0), zr( ifexte),&
                    zr(ifamor), zr(ifliai), t0, nchar, nveca,&
                    zi(iaadve), zk24(ialifo), modele, mate, carele,&
                    charge, infoch, fomult, numedd, nume,&
                    solveu, criter, zk8(iondp), nondp, numrep)
!
    else if (iinteg.eq.2) then
!
        call dlnewi(result, force0, force1, lcrea, lamort,&
                    iinteg, neq, imat, masse, rigid,&
                    amort, zr(idepl0), zr(ivite0), zr(iacce0), zr( ifexte),&
                    zr(ifamor), zr(ifliai), t0, nchar, nveca,&
                    zi(iaadve), zk24(ialifo), modele, mate, carele,&
                    charge, infoch, fomult, numedd, nume,&
                    solveu, criter, zk8(iondp), nondp, numrep)
!
    else if (iinteg.eq.3) then
!
        call dldiff(result, force1, lcrea, lamort, neq,&
                    imat, masse, rigid, amort, zr(idepl0),&
                    zr(ivite0), zr(iacce0), zr(ifexte), zr(ifamor), zr(ifliai),&
                    t0, nchar, nveca, zi(iaadve), zk24(ialifo),&
                    modele, mate, carele, charge, infoch,&
                    fomult, numedd, nume, solveu, numrep)
!
    else if (iinteg.eq.4) then
!
        call dladap(result, t0, lcrea, lamort, neq,&
                    imat, masse, rigid, amort, zr(idepl0),&
                    zr(ivite0), zr(iacce0), zr(ifexte), zr(ifamor), zr(ifliai),&
                    nchar, nveca, zi(iaadve), zk24(ialifo), modele,&
                    mate, carele, charge, infoch, fomult,&
                    numedd, nume, solveu, numrep)
!
    endif
!
!====
! 6. RESULTATS
!====
!
!
    call jeveuo(result//'           .ORDR', 'L', jord)
    call jelira(result//'           .ORDR', 'LONUTI', nbord)
    do 61 iordr = 1, nbord
        call rsadpa(result, 'E', 1, 'MODELE', zi(jord+iordr-1),&
                    0, ladpa, k8b)
        zk8(ladpa)=modele(1:8)
        call rsadpa(result, 'E', 1, 'CHAMPMAT', zi(jord+iordr-1),&
                    0, ladpa, k8b)
        zk8(ladpa)=materi
        call rsadpa(result, 'E', 1, 'CARAELEM', zi(jord+iordr-1),&
                    0, ladpa, k8b)
        zk8(ladpa)=carael
61  end do
!
! --- ON CALCULE LE CHAMP DE STRUCTURE STRX_ELGA SI BESOIN
!
    call dismoi('F', 'EXI_STR2', modele, 'MODELE', ibid,&
                kstr, iret)
    if (kstr(1:3) .eq. 'OUI') then
        compor = materi(1:8)//'.COMPOR'
        ligrel = modele(1:8)//'.MODELE'
        exipou=.false.
!
        call dismoi('F', 'EXI_POUX', modele, 'MODELE', ibid,&
                    k8b, iret)
        if (k8b(1:3) .eq. 'OUI') then
            exipou = .true.
            if (nchar .ne. 0) then
                call jeveuo(charge, 'L', jchar)
                call cochre(zk24(jchar), nchar, nbchre, iocc)
                if (nbchre .gt. 1) then
                    call u2mess('F', 'DYNAMIQUE_19')
                endif
                if (iocc .gt. 0) then
                    call getvid('EXCIT', 'CHARGE', iocc=iocc, scal=charep, nbret=iret)
                    call getvid('EXCIT', 'FONC_MULT', iocc=iocc, scal=nomfon, nbret=nfon)
                endif
            endif
            typcoe = 'R'
            alpha = 1.d0
        endif
        do 62 , iordr = 0 , nbord
        call rsexch(' ', result, 'DEPL', iordr, chamgd,&
                    iret)
        if (iret .gt. 0) goto 62
        call mecham('STRX_ELGA', modele, carael, 0, chgeom,&
                    chcara, chharm, iret)
        if (iret .ne. 0) goto 62
        call rsadpa(result, 'L', 1, 'INST', iordr,&
                    0, ladpa, k8b)
        time = zr(ladpa)
        call mechti(chgeom(1:8), time, rundf, rundf, chtime)
        call vrcins(modele, mate, carael, time, chvarc(1:19),&
                    codret)
        call vrcref(modele(1:8), mate(1:8), carael(1:8), chvref(1: 19))
        if (exipou .and. nfon .ne. 0) then
            call fointe('F ', nomfon, 1, 'INST', time,&
                        alpha, iret)
        endif
        call rsexch(' ', result, 'STRX_ELGA', iordr, chstru,&
                    iret)
        if (iret .eq. 0) goto 62
        ibid = 0
        call mecalc('STRX_ELGA', modele, chamgd, chgeom, mate,&
                    chcara, k24bla, k24bla, chtime, k24bla,&
                    chharm, ' ', ' ', ' ', ' ',&
                    k24bla, charep, typcoe, alpha, calpha,&
                    k24bla, k24bla, chstru, k24bla, ligrel,&
                    base, chvarc, chvref, k24bla, compor,&
                    k24bla, k24bla, k8b, ibid, k24bla,&
                    iret)
!
        call rsnoch(result, 'STRX_ELGA', iordr)
!
62      continue
    endif
!
    call jedema()
end subroutine
