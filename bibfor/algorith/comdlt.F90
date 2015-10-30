subroutine comdlt()
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     DYNA_VIBRA // TYPE_CALCUL = 'TRAN'  BASE_CALCUL = 'PHYS'
!     CALCUL MECANIQUE TRANSITOIRE PAR INTEGRATION DIRECTE
!     DIFFERENTS TYPES D'INTEGRATION SONT POSSIBLES:
!     - IMPLICITES :  THETA-WILSON
!                     NEWMARK
!     - EXPLICITE  :  A PAS CONSTANT  : DIFFERENCES CENTREES
!                           ADAPATATIF: ADAPT ORDRE 2   
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
#include "asterf_types.h"
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
#include "asterfort/getvtx.h"
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
#include "asterfort/utmess.h"
#include "asterfort/vrcins.h"
#include "asterfort/vrcref.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"

    character(len=6) :: nompro
    parameter ( nompro = 'COMDLT' )
!
    integer :: nveca, nchar
    integer :: imat(3), nume, niv, ibid, ifm, iondp, ladpa, numrep
    integer :: ialifo, iaadve, nondp, ifexte, ifamor, ifliai
    integer :: neq, idepl0, ivite0, iacce0, iwk, iordr
    integer :: iinteg, iret, nbpas, nbpas_min, nbpas_max
    integer :: nbord, jchar, jinst, pasar, nbar
    integer :: lresu, lcrre, iresu, nbexre, l
    integer :: nbchre, iocc, nfon, nbexcl, i, counter, lsize
    real(kind=8) :: t0, time, rundf, alpha, tinit, tfin, dt, dtmin, dtmax, cdivi
    character(len=1) :: base, typcoe
    character(len=2) :: codret
    character(len=8) :: k8b, masse, rigid, amort, baseno, result
    character(len=8) :: materi, carael, kstr, nomfon, charep
    character(len=9) :: nomsym(6)
    character(len=19) :: solveu, infcha, ligrel, linst
    character(len=12) :: allschemes(4), schema, schtyp
    character(len=24) :: modele, carele, charge, fomult, mate
    character(len=24) :: numedd, chamgd
    character(len=24) :: infoch, criter
    character(len=24) :: chgeom, chcara(18), chharm, chtime
    character(len=24) :: chvarc, chvref, chstru, k24bla, compor
    complex(kind=8) :: calpha
    character(len=19) :: force0, force1
    character(len=46) :: champs

!
    aster_logical :: lamort, lcrea, lprem, exipou
    integer, pointer :: ordr(:) => null()
    character(len=8), pointer :: chexc(:) => null()

!     -----------------------------------------------------------------
    data modele   /'                        '/
    data allschemes /'NEWMARK', 'WILSON', 'DIFF_CENTRE', 'ADAPT_ORDRE2'/
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
    chvarc = '&&COMDLT.VARC'
    chvref = '&&COMDLT.VREF'
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
        do iresu = 1, nbexre
            call getvid('EXCIT_RESU', 'RESULTAT', iocc=iresu, scal=zk8( lresu+iresu-1), nbret=l)
            call getvr8('EXCIT_RESU', 'COEF_MULT', iocc=iresu, scal=zr(lcrre+iresu-1), nbret=l)
        end do
    endif
!
!===
! 4. IMPRESSIONS RECAPITULATIVES POUR L'UTILISATEUR
!===
!
    call utmess('I', 'DYNAMIQUE_55', nk=3, valk=['D Y N A _ V I B R A',&
                                                 'TRANsitoire        ',&
                                                 'PHYSique           '])
    call utmess('I', 'DYNAMIQUE_82', sk=modele, si=neq)
    call utmess('I', 'DYNAMIQUE_60')
    call utmess('I', 'DYNAMIQUE_61', nk=2, valk=[masse, rigid])
    if (lamort) then
        call utmess('I', 'DYNAMIQUE_62', sk=amort)
    else
        call utmess('I', 'DYNAMIQUE_64')
    end if

    schema = allschemes(iinteg)
    schtyp = 'explicite'
    if ((iinteg.eq.1).or.(iinteg.eq.2))  schtyp = 'implicite'
    call getvid('INCREMENT', 'LIST_INST', iocc=1, scal=linst, nbret=iret)
    if (iret.eq.0) then
        call getvr8('INCREMENT', 'INST_INIT', iocc=1, scal=tinit, nbret=iret)
        if (iret.eq.0) tinit=0.d0
        call getvr8('INCREMENT', 'INST_FIN', iocc=1, scal=tfin)
        call getvr8('INCREMENT', 'PAS', iocc=1, scal=dt, nbret=iret)
    else
        call jeveuo(linst//'.VALE', 'L', jinst)
        call jelira(linst//'.VALE', 'LONMAX', nbpas)
        tinit = zr(jinst)
        tfin  = zr(jinst+nbpas-1)
        dt = (tfin-tinit)/real(nbpas-1)
    end if
    if (iinteg.ne.4) then
        nbpas = nint((tfin-tinit)/dt)
        call utmess('I', 'DYNAMIQUE_70', nk=2, valk=[schema, schtyp],&
                                         nr=1, valr=[dt],&
                                         ni=1, vali=[nbpas])
    else
        call getvr8('SCHEMA_TEMPS', 'PAS_MINI', iocc=1, scal=dtmin, nbret=iret)
        if (iret.eq.0) dtmin = dt*1.d-6 
        call getvr8('SCHEMA_TEMPS', 'PAS_MAXI', iocc=1, scal=dt, nbret=iret)
        if (iret.eq.0) dtmax = dt*1.d6 
        call utmess('I', 'DYNAMIQUE_66', nk=1, valk=[schema],&
                                         nr=3, valr=[dt, dtmin, dtmax])
        call getvr8('SCHEMA_TEMPS', 'COEF_DIVI_PAS', iocc=1, scal=cdivi)
        call utmess('I', 'DYNAMIQUE_68', nr=1, valr=[cdivi])
        nbpas_min = nint((tfin-tinit)/dtmax)
        nbpas_max = nint((tfin-tinit)/dtmin)
        call utmess('I', 'DYNAMIQUE_69', ni=2, vali=[nbpas_min, nbpas_max])
    end if

!
!===
! 5. INITIALISATION DE L'ALGORITHME
!===
!
    force0 = '&&COMDLT.FORCE0'
    force1 = '&&COMDLT.FORCE1'
    call dltali(neq, result, imat, masse, rigid,&
                zi(iaadve), zk24(ialifo), nchar, nveca, lcrea,&
                lprem, lamort, t0, mate, carele,&
                charge, infoch, fomult, modele, numedd,&
                nume, solveu, criter, zr(idepl0), zr(ivite0),&
                zr(iacce0), zr(ifexte+neq), zr(ifamor+neq), zr(ifliai+neq), baseno,&
                zr(iwk), force0, force1)

    call utmess('I', 'DYNAMIQUE_80', nr=2, valr=[tinit, tfin])

    call getvis('ARCHIVAGE', 'PAS_ARCH', iocc=1, scal=pasar, nbret=iret)
    if (iret.ne.0) then
        call utmess('I', 'DYNAMIQUE_85', si=pasar)
    else
        call getvid('ARCHIVAGE', 'LIST_INST', iocc=1, scal=linst, nbret=iret)
        if (iret.ne.0) then
            call jelira(linst//'.VALE', 'LONMAX', nbar)
        else
            call getvr8('ARCHIVAGE', 'INST', iocc=1, nbval=0, nbret=iret)
            nbar = -iret
        end if
        call utmess('I', 'DYNAMIQUE_86', si=nbar)
    end if


    call getvtx('ARCHIVAGE', 'CHAM_EXCLU', iocc=1, nbval=0, nbret=iret)

    nomsym(1) ='DEPL'
    nomsym(2) ='VITE'
    nomsym(3) ='ACCE'
    nomsym(4) ='FORC_EXTE'
    nomsym(5) ='FORC_AMOR'
    nomsym(6) ='FORC_LIAI'
    if (iret .ne. 0) then
        nbexcl = -iret
        AS_ALLOCATE(vk8 = chexc, size=nbexcl)
        call getvtx('ARCHIVAGE', 'CHAM_EXCLU', iocc=1, nbval=nbexcl, vect=chexc)
        do i = 1, nbexcl
            if (chexc(i)(1:4).eq.'DEPL') nomsym(1) = ' '
            if (chexc(i)(1:4).eq.'VITE') nomsym(2) = ' '
            if (chexc(i)(1:4).eq.'ACCE') nomsym(3) = ' '
        end do
        AS_DEALLOCATE(vk8 = chexc)
    endif

    call getfac('ENERGIE', iret)
    if (iret .eq. 0) then
        nomsym(4) = ' '
        nomsym(5) = ' '
        nomsym(6) = ' '
    endif
    champs  = ' '
    counter = 0
    do i = 1, 6
        if (nomsym(i)(1:4).ne.'    ') then
            lsize = 9
            if (nomsym(i)(5:5).eq.' ') lsize = 4
            champs(counter+1:counter+lsize+1) = nomsym(i)(1:lsize)//' '
            counter = counter + lsize + 1
        end if
    end do
    call utmess('I', 'DYNAMIQUE_96', sk=champs)

!
!====
! 6. INTEGRATION SELON LE TYPE SPECIFIE
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
! 7. RESULTATS
!====
!
!
    call jeveuo(result//'           .ORDR', 'L', vi=ordr)
    call jelira(result//'           .ORDR', 'LONUTI', nbord)
    do iordr = 1, nbord
        call rsadpa(result, 'E', 1, 'MODELE', ordr(iordr),&
                    0, sjv=ladpa, styp=k8b)
        zk8(ladpa)=modele(1:8)
        call rsadpa(result, 'E', 1, 'CHAMPMAT', ordr(iordr),&
                    0, sjv=ladpa, styp=k8b)
        zk8(ladpa)=materi
        call rsadpa(result, 'E', 1, 'CARAELEM', ordr(iordr),&
                    0, sjv=ladpa, styp=k8b)
        zk8(ladpa)=carael
    end do
!
! --- ON CALCULE LE CHAMP DE STRUCTURE STRX_ELGA SI BESOIN
!
    call dismoi('EXI_STR2', modele, 'MODELE', repk=kstr)
    if (kstr(1:3) .eq. 'OUI') then
        compor = materi(1:8)//'.COMPOR'
        ligrel = modele(1:8)//'.MODELE'
        exipou=.false.
!
        call dismoi('EXI_POUX', modele, 'MODELE', repk=k8b)
        if (k8b(1:3) .eq. 'OUI') then
            exipou = .true.
            if (nchar .ne. 0) then
                call jeveuo(charge, 'L', jchar)
                call cochre(zk24(jchar), nchar, nbchre, iocc)
                if (nbchre .gt. 1) then
                    call utmess('F', 'DYNAMIQUE_19')
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
                    0, sjv=ladpa, styp=k8b)
        time = zr(ladpa)
        call mechti(chgeom(1:8), time, rundf, rundf, chtime)
        call vrcins(modele, mate, carael, time, chvarc(1:19),&
                    codret)
        call vrcref(modele(1:8), mate(1:8), carael(1:8), chvref(1: 19))
        if (exipou .and. nfon .ne. 0) then
            call fointe('F ', nomfon, 1, ['INST'], [time],&
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
                    k24bla, iret)
!
        call rsnoch(result, 'STRX_ELGA', iordr)
!
 62     continue
    endif
!
    call jedema()
end subroutine
