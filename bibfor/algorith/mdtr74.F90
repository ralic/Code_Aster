subroutine mdtr74(nomres)
!
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
!
! --------------------------------------------------------------------------------------------------
!
!     BUT: CALCUL TRANSITOIRE PAR DYNA_TRAN_MODAL DANS LE CAS OU LES
!          MATRICES ET VECTEURS PROVIENNENT D'UNE PROJECTION SUR :
!              - MODE_MECA
!              - MODE_GENE
!              - BASE_MODALE
!
! ----------------------------------------------------------------------
!
    implicit none
    character(len=8) :: nomres
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/gettco.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8prem.h"
#include "asterfort/copisd.h"
#include "asterfort/copmat.h"
#include "asterfort/copmod.h"
#include "asterfort/cricho.h"
#include "asterfort/dismoi.h"
#include "asterfort/dyarch.h"
#include "asterfort/extdia.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/inicou.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/lecdon.h"
#include "asterfort/mdadap.h"
#include "asterfort/mdallo.h"
#include "asterfort/mdchoc.h"
#include "asterfort/mddevo.h"
#include "asterfort/mdeul1.h"
#include "asterfort/mdgene.h"
#include "asterfort/mdicho.h"
#include "asterfort/mdidisvisc.h"
#include "asterfort/mdinit.h"
#include "asterfort/mditm1.h"
#include "asterfort/mditmi.h"
#include "asterfort/mdnewm.h"
#include "asterfort/mdptem.h"
#include "asterfort/mdrecf.h"
#include "asterfort/mdrede.h"
#include "asterfort/mdrevi.h"
#include "asterfort/mdruku.h"
#include "asterfort/mdtr74grd.h"
#include "asterfort/mtdscr.h"
#include "asterfort/rsadpa.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
! --------------------------------------------------------------------------------------------------
    character(len=1) :: niv
    character(len=8) :: k8b, k8var, masgen, riggen, amogen, gyogen
    character(len=8) :: basemo, matass, vecgen, monmot, listam, typflu, nombm, foncv, fonca, foncp
    character(len=8) :: rigass, mailla, resgen, bamo1, bamo2, rgygen, fk(2), dfk(2)
    character(len=14) :: numddl, numgem, numgek, numgec, numgeg, k14b
    character(len=16) :: typbas, typba2, method
    character(len=19) :: lisarc, nomstm, nomstk, nomstg, masse, marig, fonct
    character(len=24) :: numk24, numm24, numc24, lisins, nomnoe, typeba, valk(3)
    logical :: lamor, okmethod
    integer :: itypfl, nexcit, nexcir, ntotex
    integer :: vali(3), jvec, jvecr, j1
    real(kind=8) :: acrit, agene
    real(kind=8) :: valr(3)
    real(kind=8) :: dt, dts, dtu, dtmax, dtmin
    real(kind=8) :: rad
    complex(kind=8) :: cbid
!
! --------------------------------------------------------------------------------------------------
!   COUPLAGE EDYOS/FISSURE
    real(kind=8) :: vrotat, dtsto, angini
    integer :: nbedyo, unitpa
    integer :: info
!
! --------------------------------------------------------------------------------------------------
    integer :: nbpal, nbrfis
    integer :: iadri
    character(len=24) :: npal
    logical :: prdeff
!
! --------------------------------------------------------------------------------------------------
    integer :: i, iam, iamog, ibid, icoupl, idiff
    integer :: ier, ifimp, ifm, ig, iindic, im
    integer :: indic, ioc, iparch, iptcho, iret, irfimp
    integer :: ng1, ng2, nng1, nng0, nng2,  jscdeg, jgyog, jrgyg
    integer :: isoupl, itrans, itrloc, ivchoc, iveci1, ivecr1
    integer :: ivecr2, ivecr3, ivecr4, ivecr5, jabsc, jaccs, jamo1
    integer :: jamo2, jamog, jarch, jbase, jbasf, jcodim, jcoefm
    integer :: jdcho, jdep0, jdepl, jdeps,  jdfk
    integer :: jfcho, jfk, jfond, jfonv, jiadve, jicho
    integer :: jidesc, jinst, jinti, jinumo, jlocf, jmasg, jnoacc
    integer :: jnodep, jnoec, jnomfo, jnovit, jordr, jparc, jpard
    integer :: jpass, jphie, jpoids, jpsdel, jpsid, jpuls
    integer :: jraig, jranc, jredc, jredd, jrede, jrefa
    integer :: jrevc, jrevv
    integer ::   jrevi, jrhoe,   jvcho
    integer :: jvit0, jvits, lamre, lires,  lmat, lnoe
    integer :: lprofv, lprol, n, n1, n2, na, nbamor
    integer :: nbcho1, nbchoc, nbexit, nbf, nbflam, nbfv, nbm0
    integer :: nbmd, nbmg, nbmod2, nbmode, nbmp, nbnli, nbpas
    integer :: nbrede, nbrevi, nbsauv, nbsism(2), nbstoc, nbstok
    integer :: nbstom, neq, ngr, nm, nmp, nr
    integer :: nterm, nts, numvif, nv, nbobjs
    real(kind=8) :: crit, deux, dtarch, eps, omeg2
    real(kind=8) :: seuil, tfexm, tfin, tinit, ts, vgap
    character(len=24), pointer :: group_ma(:) => null()
    real(kind=8), pointer :: pulsat2(:) => null()
    integer, pointer :: scdek(:) => null()
    integer, pointer :: scdem(:) => null()
    integer, pointer :: desc(:) => null()
    integer, pointer :: nequ(:) => null()
    character(len=24), pointer :: refac(:) => null()
    character(len=24), pointer :: refag(:) => null()
    character(len=24), pointer :: refak(:) => null()
    character(len=24), pointer :: refam(:) => null()
    cbid = dcmplx(0.d0, 0.d0)
!
! --------------------------------------------------------------------------------------------------
    data k14b/'              '/
    call jemarq()
    deux = 2.d0
! --------------------------------------------------------------------------------------------------
!
    ibid = 0
    jranc = 1
    jparc = 1
    jnoec = 1
    jrede = 1
    jpard = 1
    jfond = 1
    jrevi = 1
    jfonv = 1
    jdepl = 1
    jcoefm = 1
    jiadve = 1
    jinumo = 1
    jidesc = 1
    jnodep = 1
    jnovit = 1
    jnomfo = 1
    jpsdel = 1
    jpsid = 1
    jinst = 1
    monmot = 'NON'
    lisins = ' '
    prdeff = .true.
    call infniv(ifm, info)
!
    rad = r8dgrd()
    nbobjs=1
    lamor = .false.
!
!   RECUPERATION DES ARGUMENTS DE LA COMMANDE
    call getvtx('SCHEMA_TEMPS', 'SCHEMA', iocc=1, scal=method, nbret=n1)
    call getfac('EXCIT', nexcit)
    call getfac('EXCIT_RESU', nexcir)
!
!   RECUPERATION DES MATRICES PROJETEES
    call getvid(' ', 'MATR_MASS', scal=masgen, nbret=nm)
    call getvid(' ', 'MATR_RIGI', scal=riggen, nbret=nr)
    call getvid(' ', 'MATR_AMOR', scal=amogen, nbret=na)
    if (nexcit .ne. 0) then
        call wkvect('&&MDTR74.NOMVEC', 'V V K8', nexcit, jvec)
        do i = 1, nexcit
            call getvid('EXCIT', 'VECT_ASSE_GENE', iocc=i, scal=vecgen, nbret=nv)
            zk8(jvec-1+i) = vecgen
        enddo
    endif
    if (nexcir .ne. 0) then
        call wkvect('&&MDTR74.NOMVER', 'V V K8', nexcir, jvecr)
        do i = 1, nexcir
            call getvid('EXCIT_RESU', 'RESULTAT', iocc=i, scal=resgen, nbret=nv)
            zk8(jvecr-1+i) = resgen
!           VERIF : LA BASE DE MODES ASSOCIEE EST CELLE DES MATRICES GENE
            call jeveuo(masgen//'           .REFA', 'L', j1)
            bamo1=zk24(j1-1+1)(1:8)
            call dismoi('BASE_MODALE', resgen, 'RESU_DYNA', repk=bamo2)
            if (bamo1 .ne. bamo2) then
                call utmess('F', 'ALGORITH17_18', si=i)
            endif
        enddo
    endif
    if (na .eq. 0) lamor = .true.
!
!   RECUPERATION DE LA NUMEROTATION GENERALISEE DE M, K
    call jeveuo(masgen//'           .REFA', 'L', vk24=refam)
    numgem = refam(2)(1:14)
    nomstm = numgem//'.SLCS'
    call jeveuo(nomstm//'.SCDE', 'L', vi=scdem)
    nbstom = scdem(1)*scdem(4)
!
    call jeveuo(riggen//'           .REFA', 'L', vk24=refak)
    numgek = refak(2)(1:14)
    nomstk = numgek//'.SLCS'
    call jeveuo(nomstk//'.SCDE', 'L', vi=scdek)
    nbstok = scdek(1)*scdek(4)
!
!   RECUPERATION DE LA BASE MODALE ET NOMBRE DE MODES
    call jeveuo(masgen//'           .DESC', 'L', vi=desc)
    nbmode = desc(2)
    basemo = refam(1)(1:8)
    call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=rigass)
!
!   on recupere le type de base modale
    call dismoi('TYPE_BASE', basemo, 'RESU_DYNA', repk=typeba, arret='C',&
                ier=iret)
! --------------------------------------------------------------------------------------------------
    marig = '&&MDTR74.RIGI'
    call copisd('MATR_ASSE', 'V', rigass, marig)
    call jeexin(marig//'.REFA', ier)
    if (ier .eq. 0) call wkvect(marig//'.REFA', 'V V K24', 20, jrefa)
    call jeveuo(marig//'.REFA', 'E', jrefa)
    zk24(jrefa-1+7)='&&OP0074.SOLVEUR'
!
    call gettco(basemo, typbas)
    call mtdscr(masgen)
    call jeveuo(masgen//'           .&INT', 'L', lmat)
    nterm = zi(lmat+14)
    typba2 = typbas
!
    if ((typbas.eq.'MODE_MECA'.and.nterm.gt.nbmode) .or.&
        (typbas.eq.'MODE_GENE'.and.nterm.gt.nbmode)) then
        typbas = 'BASE_MODA'
    endif
!
    if (typeba(1:) .ne. ' ' .and. nterm .eq. nbmode) then
        typbas = 'MODE_MECA'
!
    else if (typeba(1:).ne.' '.and.nterm.gt.nbmode) then
        typbas = 'BASE_MODA'
    endif
!
    nbstoc = nbmode
!
    if (typba2(1:9) .eq. 'MODE_MECA' .and. typeba(1:1) .eq. ' ') then
        call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=matass)
        call dismoi('NOM_MAILLA', matass, 'MATR_ASSE', repk=mailla)
        call dismoi('NOM_NUME_DDL', matass, 'MATR_ASSE', repk=numddl)
        call dismoi('NB_EQUA', matass, 'MATR_ASSE', repi=neq)
        nbmod2 = nbmode
!
    else if (typeba(1:1).ne.' ') then
        call dismoi('NUME_DDL', basemo, 'RESU_DYNA', repk=numddl)
        call dismoi('NOM_MAILLA', numddl, 'NUME_DDL', repk=mailla)
        call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neq)
        nbmod2 = nbmode
    else if (typba2(1:9).eq.'MODE_GENE') then
        call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=matass)
        call jeveuo(matass//'           .REFA', 'L', jrefa)
        numddl = zk24(jrefa-1+2)(1:14)
        call dismoi('NOM_MAILLA', numddl, 'NUME_DDL', repk=mailla)
        call jeveuo(numddl//'.NUME.NEQU', 'L', vi=nequ)
        neq = nequ(1)
        nbmod2 = nbmode
    else
        call utmess('F', 'ALGORITH5_65')
    endif
!
!   RECOPIE DES MATRICES DANS DES VECTEURS DE TRAVAIL
    call wkvect('&&MDTR74.MASSEGEN', 'V V R', nbstom, jmasg)
    call wkvect('&&MDTR74.RAIDEGEN', 'V V R', nbstok, jraig)
    call wkvect('&&MDTR74.AMORTGEN', 'V V R', nbstoc, jamo1)
    call wkvect('&&MDTR74.PULSATIO', 'V V R', nbmode, jpuls)
    AS_ALLOCATE(vr=pulsat2, size=nbmode)
    numm24(1:14) = numgem
    numk24(1:14) = numgek
    call extdia(masgen, numm24, 0, zr(jmasg))
    call extdia(riggen, numk24, 0, zr(jraig))
!
!   RECOPIE DES MODES DU CONCEPT RESULTAT DANS UN VECTEUR
    call wkvect('&&MDTR74.BASEMODE', 'V V R', nbmode*neq, jbase)
    call copmod(basemo, numer=numddl, bmodr=zr(jbase), nbmodes=nbmode, nequa=neq)
!
    do i = 0, nbmode - 1
        omeg2 = abs(zr(jraig+i)/zr(jmasg+i))
        zr(jpuls+i) = sqrt(omeg2)
        pulsat2(1+i) = omeg2
    enddo
!
!   RECUPERATION DE L AMORTISSEMENT D'UNE LISTE D'AMORTISSEMENTS REDUITS
    if (lamor) then
        amogen = '&&AMORT'
        call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=0, nbret=n1)
        call getvid('AMOR_MODAL', 'LIST_AMOR', iocc=1, nbval=0, nbret=n2)
        if (n1 .ne. 0 .or. n2 .ne. 0) then
            if (n1 .ne. 0) then
                nbamor = -n1
            else
                call getvid('AMOR_MODAL', 'LIST_AMOR', iocc=1, scal=listam, nbret=n)
                call jelira(listam//'           .VALE', 'LONMAX', nbamor)
            endif
            if (nbamor .gt. nbmode) then
!
                vali (1) = nbmode
                vali (2) = nbamor
                vali (3) = nbmode
                valk (1) = 'PREMIERS COEFFICIENTS'
                call utmess('A', 'ALGORITH16_18', sk=valk(1), ni=3, vali=vali)
                call wkvect('&&MDTR74.AMORTI', 'V V R8', nbmode, jamog)
                if (n1 .ne. 0) then
                    call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=nbmode,&
                                vect=zr(jamog), nbret=n)
                else
                    call jeveuo(listam//'           .VALE', 'L', iamog)
                    do iam = 1, nbmode
                        zr(jamog+iam-1) = zr(iamog+iam-1)
                    enddo
                endif
            else if (nbamor.lt.nbmode) then
!
                call wkvect('&&MDTR74.AMORTI', 'V V R8', nbamor, jamog)
                if (n1 .ne. 0) then
                    call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=nbamor,&
                                vect=zr(jamog), nbret=n)
                else
                    call jeveuo(listam//'           .VALE', 'L', iamog)
                    do iam = 1, nbamor
                        zr(jamog+iam-1) = zr(iamog+iam-1)
                    enddo
                endif
                idiff = nbmode - nbamor
                vali (1) = idiff
                vali (2) = nbmode
                vali (3) = idiff
                call utmess('I', 'ALGORITH16_19', ni=3, vali=vali)
                call wkvect('&&MDTR74.AMORTI2', 'V V R8', nbmode, jamo2)
                do iam = 1, nbamor
                    zr(jamo2+iam-1) = zr(jamog+iam-1)
                enddo
                do iam = nbamor + 1, nbmode
                    zr(jamo2+iam-1) = zr(jamog+nbamor-1)
                enddo
                jamog = jamo2
            else if (nbamor.eq.nbmode) then
!
                call wkvect('&&MDTR74.AMORTI', 'V V R8', nbamor, jamog)
                if (n1 .ne. 0) then
                    call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=nbamor,&
                                vect=zr(jamog), nbret=n)
                else
                    call jeveuo(listam//'           .VALE', 'L', iamog)
                    do iam = 1, nbamor
                        zr(jamog+iam-1) = zr(iamog+iam-1)
                    enddo
                endif
            endif
        else
            call wkvect('&&MDTR74.AMORTI', 'V V R8', nbmode, jamog)
            do im = 1, nbmode
                if (typeba(1:1) .eq. ' ') then
                    call rsadpa(basemo, 'L', 1, 'AMOR_REDUIT', im,&
                                0, sjv=lamre, styp=k8b)
                    zr(jamog+im-1) = zr(lamre)
                else
                    zr(jamog+im-1) = 0.d0
                endif
            enddo
        endif
        amogen = '        '
!
    endif
!
!   RECUPERATION D'UNE MATRICE D'AMORTISSEMENTS DIMENSIONNELS
    if (.not.lamor) then
!       RECUP NUMEROTATION GENE DE MATRICE AMORTISSEMENT
        call jeveuo(amogen//'           .REFA', 'L', vk24=refac)
        numgec = refac(2)(1:14)
        numc24(1:14) = numgec
        call extdia(amogen, numc24, 0, zr(jamo1))
        do i = 1, nbmod2
            acrit = deux*sqrt(abs(zr(jmasg+i-1)*zr(jraig+i-1)))
            agene = zr(jamo1+i-1)
            if (agene .gt. acrit) then
                vali (1) = i
                valr (1) = agene
                valr (2) = acrit
                valk (1) = ' '
                call utmess('A', 'ALGORITH16_20', sk=valk(1), si=vali(1), nr=2,&
                            valr=valr)
            endif
        enddo
!       PROBLEME POSSIBLE DU JEVEUO SUR UNE COLLECTION
        call wkvect('&&MDTR74.AMORTI', 'V V R8', nbmode*nbmode, jamog)
        call copmat(amogen, numgec, zr(jamog))
!
    endif
    vrotat = 0.d0
    call getvtx(' ', 'VITESSE_VARIABLE', scal=k8var, nbret=n1)
    call wkvect('&&MDTR74.GYROSC', 'V V R8', nbmode*nbmode, jgyog)
    call wkvect('&&MDTR74.RIGYRO', 'V V R8', nbmode*nbmode, jrgyg)
    if (k8var .eq. 'OUI') then
        call getvid(' ', 'VITE_ROTA', scal=foncv, nbret=nng1)
        call getvid(' ', 'ACCE_ROTA', scal=fonca, nbret=nng2)
        call getvid(' ', 'MATR_GYRO', scal=gyogen, nbret=ng1)
        call getvid(' ', 'MATR_RIGY', scal=rgygen, nbret=ng2)
        call jeveuo(gyogen//'           .REFA', 'L', vk24=refag)
        numgeg = refag(2)(1:14)
        nomstg = numgeg//'.SLCS'
        call jeveuo(nomstg//'.SCDE', 'L', jscdeg)
        call copmat(gyogen, numgeg, zr(jgyog))
        if (ng2 .ne. 0) then
            call copmat(rgygen, numgeg, zr(jrgyg))
        endif
    else
        call getvr8(' ', 'VITE_ROTA', scal=vrotat, nbret=n1)
    endif
!
!   VERIFICATION DES DONNEES GENERALISEES
    call mdgene(basemo, nbmode, k14b, masgen, riggen,&
                amogen, nexcit, jvec, iret)
    if (iret .ne. 0) goto 120
!
    if (method .eq. 'ITMI') then
!       DONNEES ITMI
        call jedetr('&&MDTR74.PULSATIO')
        call jedetr('&&MDTR74.MASSEGEN')
        call jedetr('&&MDTR74.AMORTGEN')
        call jedetr('&&MDTR74.BASEMODE')
        call jedetr('&&MDTR74.AMORTI')
!       ET POUR GAGNER DE LA PLACE
        AS_DEALLOCATE(vr=pulsat2)
        nbmode = 0
        call mditmi(typflu, nombm, icoupl, nbm0, nbmode,&
                    nbmd, vgap, itrans, eps, ts,&
                    nts, itypfl)
        call jeveuo('&&MDITMI.PULSATIO', 'L', jpuls)
        call jeveuo('&&MDITMI.MASSEGEN', 'L', jmasg)
        call jeveuo('&&MDITMI.AMORTI', 'L', jamog)
        call jeveuo('&&MDITMI.AMORTGEN', 'L', jamo1)
        call jeveuo('&&MDITMI.BASEMODE', 'L', jbase)
        call jeveuo('&&MDITMI.LOCFL0', 'L', jlocf)
        call getvis('SCHEMA_TEMPS', 'NUME_VITE_FLUI', iocc=1, scal=numvif, nbret=n1)
        call getvis('SCHEMA_TEMPS', 'NB_MODE_FLUI', iocc=1, scal=nbmp, nbret=nmp)
        if (itypfl .eq. 1) then
            call jeveuo('&&MDITMI.TEMP.IRES', 'L', lires)
            call jeveuo('&&MDITMI.TEMP.PROFV', 'L', lprofv)
            call jeveuo('&&MDITMI.TEMP.RHOE', 'L', jrhoe)
            call jeveuo('&&MDITMI.TEMP.BASEFL', 'L', jbasf)
            call jeveuo('&&MDITMI.TEMP.PHIE', 'L', jphie)
            call jeveuo('&&MDITMI.TEMP.ABSCV', 'L', jabsc)
            iveci1 = lires
            ivecr1 = lprofv
            ivecr2 = jrhoe
            ivecr3 = jbasf
            ivecr4 = jphie
            ivecr5 = jabsc
        endif
        if (itypfl .eq. 2) then
            call jeveuo('&&MDITMI.TEMP.CODIM', 'L', jcodim)
            call jeveuo('&&MDITMI.TEMP.POIDS', 'L', jpoids)
            call jeveuo('&&MDITMI.TEMP.PHIE', 'L', jphie)
            iveci1= 1
            ivecr1= jmasg
            ivecr2 = jcodim
            ivecr3 = jpoids
            ivecr4 = jphie
            ivecr5= 1
        endif
    endif
!
!   RECUPERATION DES PARAMETRES D'EXCITATION
    ntotex = nexcit + nexcir*nbmode
    jnoacc=1
    if (ntotex .ne. 0) then
        call wkvect('&&MDTR74.COEFM', 'V V R8', ntotex, jcoefm)
        call wkvect('&&MDTR74.IADVEC', 'V V IS', ntotex, jiadve)
        call wkvect('&&MDTR74.INUMOR', 'V V IS', ntotex, jinumo)
        call wkvect('&&MDTR74.IDESCF', 'V V IS', ntotex, jidesc)
        call wkvect('&&MDTR74.NOMFON', 'V V K8', 2*ntotex, jnomfo)
        call wkvect(nomres//'           .FDEP', 'G V K8', 2*ntotex, jnodep)
        call wkvect(nomres//'           .FVIT', 'G V K8', 2*ntotex, jnovit)
        call wkvect(nomres//'           .FACC', 'G V K8', 2*ntotex, jnoacc)
!
        call mdrecf(nexcit, nexcir, zi(jidesc), zk8(jnomfo), zr(jcoefm),&
                    zi(jiadve), zi(jinumo), zk8(jnodep), zk8(jnovit), zk8(jnoacc),&
                    neq, typba2, basemo, nbmode, zr(jraig),&
                    monmot, nomres)
        call jeexin(nomres//'           .IPSD', iret)
        if (iret .ne. 0) call jeveuo(nomres//'           .IPSD', 'E', jpsdel)
!
        if (method .eq. 'ITMI') then
            nbf = 0
            do i = 1, ntotex
                call jelira(zk8(jnomfo-1+i)//'           .VALE', 'LONMAX', nbfv)
                nbfv = nbfv/2
                nbf = max(nbf,nbfv)
            enddo
        endif
    endif
!
! --------------------------------------------------------------------------------------------------
!
    call getfac('CHOC', nbcho1)
!
!   Amortisseurs anti-sismique : ANTI_SISM, DIS_VISC
    nbsism(1:2)= 0
    call getfac('ANTI_SISM', nbsism(1))
    if ( nbsism(1).ne.0 ) then
        okmethod = (method.eq.'EULER').or.(method(1:5).eq.'RUNGE').or.(method(1:5).eq.'ADAPT')
        if ( .not. okmethod ) then
            valk(1) = 'DYNA_TRAN_MODAL'
            valk(2) = 'ANTI_SISM'
            valk(3) = method
            call utmess('F', 'ALGORITH5_81', nk=3, valk=valk)
        endif
    endif
!
    call getfac('DIS_VISC',  nbsism(2))
    if ( nbsism(2).ne.0 ) then
        okmethod = (method.eq.'EULER').or.(method(1:5).eq.'RUNGE')
        if ( .not. okmethod ) then
            valk(1) = 'DYNA_TRAN_MODAL'
            valk(2) = 'DIS_VISC'
            valk(3) = method
            call utmess('F', 'ALGORITH5_81', nk=3, valk=valk)
        endif
    endif
!
    call getfac('FLAMBAGE', nbflam)
!
    nbchoc = 0
    do ioc = 1, nbcho1
        call getvtx('CHOC', 'MAILLE', iocc=ioc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            nbchoc = nbchoc - n1
        else
            call getvtx('CHOC', 'GROUP_MA', iocc=ioc, nbval=0, nbret=n2)
            if (n2 .ne. 0) then
                ngr = -n2
                AS_ALLOCATE(vk24=group_ma, size=ngr)
                call getvtx('CHOC', 'GROUP_MA', iocc=ioc, nbval=ngr, vect=group_ma)
                do ig = 0, ngr-1
                    call jelira(jexnom(mailla//'.GROUPEMA', group_ma(ig+1)), 'LONMAX', nbmg)
                    nbchoc = nbchoc + nbmg
                enddo
                AS_DEALLOCATE(vk24=group_ma)
            else
                nbchoc = nbchoc + 1
            endif
        endif
    enddo
!
    nbnli = nbchoc + nbsism(1)+nbsism(2) + nbflam
!
! --------------------------------------------------------------------------------------------------
!
!   COUPLAGE  EDYOS
    nbedyo = 0
    call getfac('COUPLAGE_EDYOS', nbedyo)
    nbpal = 0
    dtsto = 0.d0
    if (nbedyo .ne. 0) then
        call getfac('PALIER_EDYOS', nbedyo)
        if (nbedyo .ne. 0) then
!           Lecture des donnees paliers
            call getvis('PALIER_EDYOS', 'UNITE', iocc=1, scal=unitpa, nbret=n1)
            if (n1 .ne. 0) then
                call lecdon(.true., unitpa, prdeff)
            else
                call lecdon(.false., 0, prdeff)
            endif
            call getvr8('COUPLAGE_EDYOS', 'PAS_TPS_EDYOS', iocc=1, scal=dtsto, nbret=n1)
        else
            call utmess('F', 'EDYOS_48')
        endif
!       Recuperation du nombre de paliers
        npal='N_PAL'
        call jeveuo(npal, 'L', iadri)
        nbpal=zi(iadri)
        nbnli = nbnli + nbpal
    endif
!
!   NON LINEARITE DE ROTOR FISSURE
    nbrfis = 0
    angini = 0.d0
    call getfac('ROTOR_FISS', nbrfis)
    if (nbrfis .ne. 0) then
        if (method .ne. 'EULER') then
            call utmess('F', 'ALGORITH5_80')
        endif
        nbnli = nbnli + nbrfis
        call wkvect('&&MDTR74.FK', 'V V K8', 2*nbrfis, jfk)
        call wkvect('&&MDTR74.DFK', 'V V K8', 2*nbrfis, jdfk)
        do ioc = 1, nbrfis
            call getvid('ROTOR_FISS', 'K_PHI', iocc=ioc, scal=fonct, nbret=n1)
            fk(1) = fonct(1:8)
            call jeveuo(fonct//'.PROL', 'L', lprol)
            fk(2) = zk24(lprol)(1:8)
            call getvid('ROTOR_FISS', 'DK_DPHI', iocc=ioc, scal=fonct, nbret=n1)
            dfk(1) = fonct(1:8)
            call jeveuo(fonct//'.PROL', 'L', lprol)
            dfk(2) = zk24(lprol)(1:8)
            if (k8var .eq. 'OUI') then
                call getvid('ROTOR_FISS', 'ANGL_ROTA', iocc=ioc, scal=foncp, nbret=nng0)
            else
                call getvr8('ROTOR_FISS', 'ANGL_INIT', iocc=ioc, scal=angini, nbret=n1)
                angini=angini*rad
            endif
        enddo
    endif
!
    jinti=1
    if (nbnli .ne. 0) then
        call wkvect('&&MDTR74.RANG_CHOC', 'V V I ', nbnli*mdtr74grd('LOGCHO'), jranc)
        call wkvect('&&MDTR74.DEPL', 'V V R8', nbnli*6*nbmode, jdepl)
        call wkvect('&&MDTR74.PARA_CHOC', 'V V R8', nbnli*mdtr74grd('PARCHO'), jparc)
        call wkvect('&&MDTR74.NOEU_CHOC', 'V V K8', nbnli*9, jnoec)
        call wkvect('&&MDTR74.INTI_CHOC', 'V V K8', nbnli, jinti)
        if (ntotex .ne. 0) then
            nbexit = ntotex
            call wkvect('&&MDTR74.PSID', 'V V R8', nbnli*6*ntotex, jpsid)
        else
            nbexit = 1
        endif
        call mdchoc(nbnli, nbchoc, nbflam, nbsism, nbrfis,&
                    nbpal, zi(jranc), zr(jdepl), zr(jparc), zk8(jnoec),&
                    zk8(jinti), zr(jpsdel), zr(jpsid), numddl, nbmode,&
                    zr(jpuls), zr(jmasg), lamor, zr(jamog), zr(jbase),&
                    neq, nbexit, info, monmot, iret)
        if (iret .ne. 0) goto 120
        call getfac('VERI_CHOC', ivchoc)
        if (ivchoc .ne. 0) then
            call wkvect('&&MDTR74.FIMPO', 'V V R8', neq, ifimp)
            call wkvect('&&MDTR74.RFIMPO', 'V V R8', neq, irfimp)
            call wkvect('&&MDTR74.PTCHOC', 'V V R8', neq, iptcho)
            if (info .eq. 2) then
                call wkvect('&&MDTR74.SOUPL', 'V V R8', nbmode, isoupl)
                call wkvect('&&MDTR74.TRLOC', 'V V R8', nbmode, itrloc)
                call wkvect('&&MDTR74.INDIC', 'V V I', nbmode, iindic)
            else
                itrloc = 1
                isoupl = 1
                iindic = 1
            endif
            nbnli = nbnli - nbpal
            call cricho(nbmode, zr(jraig), nbnli, zr(jparc), zk8(jnoec),&
                        info, zr(ifimp), zr(irfimp), zr(itrloc), zr(isoupl),&
                        zi( iindic), neq, zr(jbase), seuil, marig,&
                        nbnli)
            nbnli = nbnli + nbpal
            call getvr8('VERI_CHOC', 'SEUIL', iocc=1, scal=crit, nbret=n1)
            if (seuil .gt. crit) then
                niv = 'A'
                call getvtx('VERI_CHOC', 'STOP_CRITERE', iocc=1, scal=k8b, nbret=n1)
                if (k8b .eq. 'OUI') niv = 'F'
                valr (1) = seuil
                call utmess('I', 'ALGORITH16_21', sr=valr(1))
                call utmess(niv, 'ALGORITH5_66')
            endif
        endif
    endif
!
!   RELATION EFFORT DEPLACEMENT
    call getfac('RELA_EFFO_DEPL', nbrede)
    if (nbrede .ne. 0) then
        call wkvect('&&MDTR74.DPLR', 'V V R8', nbrede*6*nbmode, jrede)
        call wkvect('&&MDTR74.FONC_REDE', 'V V K8', nbrede*3, jfond)
        call mdrede(numddl, nbrede, nbmode, zr(jbase), neq,&
                    zr(jrede), zk8(jfond), iret)
        if (iret .ne. 0) goto 120
    endif
!
!   RELATION EFFORT VITESSE
    call getfac('RELA_EFFO_VITE', nbrevi)
    if (nbrevi .ne. 0) then
        call wkvect('&&MDTR74.DPLV', 'V V R8', nbrevi*6*nbmode, jrevi)
        call wkvect('&&MDTR74.FONC_REVI', 'V V K8', nbrevi*3, jfonv)
        call mdrevi(numddl, nbrevi, nbmode, zr(jbase), neq,&
                    zr(jrevi), zk8(jfonv), iret)
        if (iret .ne. 0) goto 120
    endif
!
!   DESTRUCTION DU VECTEUR BASE MODALE (POUR FAIRE DE LA PLACE)
    call jedetr('&&MDTR74.BASEMODE')
!
!   VERIFICATION DU PAS DE TEMPS
    call mdptem(nbmode, zr(jmasg), zr(jpuls), nbnli, zr(jdepl),&
                zr(jparc), zk8(jnoec), dt, dts, dtu,&
                dtmax, dtmin, tinit, tfin, nbpas,&
                info, iret, lisins)
!
!   COUPLAGE EDYOS
    if (nbedyo .ne. 0) then
        call inicou(nbpas, tinit, tfin, dt, dtsto,&
                    vrotat)
    endif
!
    if (method .eq. 'ITMI') then
        tfexm = tfin - tinit
        if (nts .eq. 0) ts = tfexm
    endif
    if (iret .ne. 0) goto 120
!
!   ARCHIVAGE
    if (method(1:5) .eq. 'ADAPT' .or. method .eq. 'ITMI' .or. method(1:5) .eq. 'RUNGE') then
        call getvis('ARCHIVAGE', 'PAS_ARCH', iocc=1, scal=iparch, nbret=n1)
        if (n1 .eq. 0) iparch = 1
        if (method(1:5) .eq. 'ADAPT') then
            dtarch = dtmax*iparch
            nbsauv = int((tfin-tinit)/dtarch) + 1
            if ((tfin - (tinit+(nbsauv-1)*dtarch)) .ge. r8prem()) then
                nbsauv=nbsauv+1
            endif
        else if (method.eq.'ITMI') then
!           DANS LE CAS ITMI, NBSAUV NE SERA CONNU QUE DANS MDITM2
            nbsauv = 0
        else if (method(1:5).eq.'RUNGE') then
!           DANS LE CAS RUNGE ON ARCHIVE TOUS LES PAS DE CALCUL
            if (dt .gt. dtmax) then
                nbsauv = int((tfin-tinit)/dtmax) + 1
            else
                nbsauv=nbpas+1
            endif
        endif
!
    else
        lisarc = '&&MDTR74.ARCHIVAGE'
        k8b = ' '
        call dyarch(nbpas, lisins, lisarc, nbsauv, 0,&
                    ibid, k8b)
        call jeveuo(lisarc, 'E', jarch)
    endif
!
    if (typbas(1:9) .eq. 'BASE_MODA' .and. method .ne. 'DEVOGE') then
        call copmat(masgen, numgem, zr(jmasg))
        call copmat(riggen, numgek, zr(jraig))
    endif
!
!   ALLOCATION DES VECTEURS DE SORTIE
    if (method(1:5) .ne. 'RUNGE') then
!       Dans le cas de RUNGE KUTTA, l'allocation se fait a l'intérieur de la routine MDRUKU
!       Pour cette option il y a plusieurs objets de longueur variable. La taille d'un nouvel
!       objet est NBSAUV*1.5. La routine 'concrk' se charge de compacter les objets.
        call mdallo(nomres, 'TRAN', nbsauv, sauve='GLOB', checkarg=.false.,&
                    method=method, base=basemo, nbmodes=nbmode, rigi=riggen, mass=masgen,&
                    amor=amogen, jordr=jordr, jdisc=jinst, jdepl=jdeps, jvite=jvits,&
                    jacce=jaccs, dt=dt, jptem=jpass, nbchoc=nbnli, noecho=zk8(jnoec),&
                    intitu=zk8(jinti), jfcho=jfcho, jdcho=jdcho, jvcho=jvcho, jadcho=jicho,&
                    nbrede=nbrede, fonred=zk8(jfond), jredc=jredc, jredd=jredd, nbrevi=nbrevi,&
                    fonrev=zk8(jfonv), jrevc=jrevc, jrevv=jrevv)
    endif
!
    if (info .eq. 1 .or. info .eq. 2) then
        valk (1) = typbas(1:9)
        valk (2) = method
        valk (3) = basemo
        vali (1) = neq
        vali (2) = nbmode
        call utmess('I', 'ALGORITH16_22', nk=3, valk=valk, ni=2,&
                    vali=vali)
        if (method(1:5) .eq. 'ADAPT') then
            valr (1) = dt
            vali (1) = nbsauv
            call utmess('I', 'ALGORITH16_23', si=vali(1), sr=valr(1))
        else if (method.eq.'ITMI') then
            vali (1) = numvif
            vali (2) = nbmode
            vali (3) = nbm0
            valr (1) = vgap
            valr (2) = dt
            valr (3) = tfexm
            call utmess('I', 'ALGORITH16_24', ni=3, vali=vali, nr=3,&
                        valr=valr)
            if (itrans .ne. 0) then
                valr(1) = eps
                call utmess('I', 'ALGORITH16_78', sr=valr(1))
            endif
            if (icoupl .ne. 0) then
                vali (1) = nbmp
                call utmess('I', 'ALGORITH16_79', si=vali(1))
            endif
            vali (1) = iparch
            call utmess('I', 'ALGORITH16_25', si=vali(1))
        else
            valr (1) = dt
            vali (1) = nbpas
            vali (2) = nbsauv
            call utmess('I', 'ALGORITH16_26', ni=2, vali=vali, sr=valr(1))
        endif
        if (nbchoc .ne. 0) then
            vali (1) = nbchoc
            call utmess('I', 'ALGORITH16_80', si=vali(1))
        endif
        if (nbsism(1)+nbsism(2) .ne. 0) then
            vali (1) = nbsism(1)+nbsism(2)
            call utmess('I', 'ALGORITH16_81', si=vali(1))
        endif
        if (nbflam .ne. 0) then
            vali (1) = nbflam
            call utmess('I', 'ALGORITH16_82', si=vali(1))
        endif
        if (nbrede .ne. 0) then
            vali (1) = nbrede
            call utmess('I', 'ALGORITH16_83', si=vali(1))
        endif
        if (nbrevi .ne. 0) then
            vali (1) = nbrevi
            call utmess('I', 'ALGORITH16_84', si=vali(1))
        endif
    endif
!
    if (method .eq. 'EULER') then
        call mdeul1(nbpas, dt, nbmode, zr(jpuls), pulsat2,&
                    zr(jmasg), ibid, zr(jraig), ibid, zr(jrgyg),&
                    lamor, zr(jamog), ibid, zr(jgyog), foncv,&
                    fonca, typbas, basemo, tinit, zi(jarch),&
                    nbsauv, nbnli, zi(jranc), zr(jdepl), zr(jparc),&
                    zk8(jnoec), nbrede, zr(jrede), zk8(jfond), nbrevi,&
                    zr(jrevi), zk8(jfonv), zr(jdeps), zr(jvits), zr(jaccs),&
                    zi(jordr), zr(jinst), zr(jfcho), zr(jdcho), zr(jvcho),&
                    zi(jicho), zi(jredc), zr(jredd), zi(jrevc), zr(jrevv),&
                    zr(jcoefm), zi(jiadve), zi(jinumo), zi(jidesc), zk8(jnodep),&
                    zk8(jnovit), zk8(jnoacc), zk8(jnomfo), zr(jpsid), monmot,&
                    nbrfis, fk, dfk, angini, foncp,&
                    nbpal, dtsto, vrotat, prdeff, nomres,&
                    ntotex, zr(jpass), zk8(jinti))
!
    else if (method(1:5).eq.'RUNGE') then
        call mdruku(method, tinit, tfin, dt, dtmin,&
                    dtmax, nbsauv, nbobjs, nbmode, zr(jpuls),&
                    pulsat2, zr(jmasg), ibid, zr(jraig), ibid,&
                    zr(jrgyg), lamor, zr(jamog), ibid, zr(jgyog),&
                    foncv, fonca, typbas, basemo, nbnli,&
                    zk8(jinti), zi(jranc), zr(jdepl), zr(jparc), zk8(jnoec),&
                    nbrede, zr(jrede), zk8(jfond), nbrevi, zr(jrevi),&
                    zk8(jfonv), zr(jcoefm), zi(jiadve), zi(jinumo), zi(jidesc),&
                    zk8(jnodep), zk8(jnovit), zk8(jnoacc), zk8(jnomfo), zr(jpsid),&
                    monmot, nbrfis, fk, dfk, angini,&
                    foncp, nbpal, dtsto, vrotat, prdeff,&
                    nomres, ntotex, masgen, riggen, amogen)
!       Les pointeurs sur la SD 'nomres' sont crée par 'mdallo' et sont en sortie de cette routine.
!       Pour Runge c'est fait dans mdruku ==> IL NE SONT DONC PAS CONNUS ICI
!                                         ==> ON VA CHERCHER CEUX DONT ON A BESOIN
            if ( nbnli .ne. 0 ) then
                call jeveuo(nomres//'           .DISC', 'L', jinst)
                call jeveuo(nomres//'           .FCHO', 'L', jfcho)
                call jeveuo(nomres//'           .DLOC', 'L', jdcho)
                call jeveuo(nomres//'           .VCHO', 'L', jvcho)
            endif
!
    else if (method(1:5).eq.'ADAPT') then
        call mdadap(dt, dtmax, nbmode, zr(jpuls), pulsat2,&
                    zr(jmasg), ibid, zr(jraig), ibid, lamor,&
                    zr(jamog), ibid, typbas, basemo, tinit,&
                    tfin, dtarch, nbsauv, nbnli, zi( jranc),&
                    zr(jdepl), zr(jparc), zk8(jnoec), nbrede, zr(jrede),&
                    zk8(jfond), nbrevi, zr(jrevi), zk8(jfonv), zr(jdeps),&
                    zr( jvits), zr(jaccs), zr(jpass), zi(jordr), zr(jinst),&
                    zr(jfcho), zr( jdcho), zr(jvcho), zi(jicho), zi(jredc),&
                    zr(jredd), zr(jcoefm), zi(jiadve), zi(jinumo), zi(jidesc),&
                    zk8(jnodep), zk8(jnovit), zk8( jnoacc), zk8(jnomfo), zr(jpsid),&
                    monmot, nbpal, dtsto, vrotat, prdeff,&
                    method, nomres, ntotex, zi(jrevc), zr(jrevv), &
                    zk8(jinti))
    else if (method.eq.'NEWMARK') then
        call mdnewm(nbpas, dt, nbmode, zr(jpuls), pulsat2,&
                    zr(jmasg), zr(jraig), zr(jrgyg), lamor, zr(jamog),&
                    zr(jgyog), foncv, fonca, typbas, basemo,&
                    tinit, zi(jarch), zr(jdeps), zr(jvits), zr(jaccs),&
                    zi(jordr), zr(jinst), nomres, ntotex, zi(jidesc),&
                    zk8(jnomfo), zr( jcoefm), zi(jiadve), zi(jinumo), zr(jpass))
!
    else if (method.eq.'DEVOGE') then
        call mddevo(nbpas, dt, nbmode, zr(jpuls), pulsat2,&
                    zr(jmasg), zr(jamog), basemo, tinit, zi(jarch),&
                    nbsauv, nbnli, zi(jranc), zr( jdepl), zr(jparc),&
                    zk8(jnoec), nbrede, zr(jrede), zk8(jfond), nbrevi,&
                    zr(jrevi), zk8(jfonv), zr(jdeps), zr(jvits), zr(jaccs),&
                    zi(jordr), zr(jinst), zr(jfcho), zr(jdcho), zr(jvcho),&
                    zi(jicho), zi(jredc), zr(jredd), zr(jcoefm), zi(jiadve),&
                    zi( jinumo), zi(jidesc), zk8(jnodep), zk8(jnovit), zk8(jnoacc),&
                    zk8( jnomfo), zr(jpsid), monmot, nomres, ntotex,&
                    zr(jpass), zi(jrevc), zr(jrevv), zk8(jinti))
    endif
!
!   Impression des résultats de choc
!       sauf pour ITMI
!       sauf dans le cas DIS_VISC
    if ( (method.ne.'ITMI').and.(nbnli.ne.0).and.(nbsism(2).eq.0) ) then
        call mdicho(nomres, nbsauv, zr(jinst), zr(jfcho), zr(jdcho),&
                    zr(jvcho), nbnli, nbchoc, zr(jparc), zk8(jnoec))
    endif
!   Impression des résultats des DIS_VISC
    if ( nbsism(2).ne.0 ) then
        call mdidisvisc(nomres, nbnli, zi(jranc), zk8(jnoec), nbsauv, &
                        zr(jinst) )
    endif
!
    if (method .eq. 'ITMI') then
!       CONDITIONS INITIALES
        call wkvect('&&MDITMI.DEPL_0', 'V V R8', nbmode, jdep0)
        call wkvect('&&MDITMI.VITE_0', 'V V R8', nbmode, jvit0)
        call mdinit(nombm, nbmode, 0, zr(jdep0), zr(jvit0),&
                    [0.d0], iret, tinit, intitu=zk8(jinti), noecho=zk8(jnoec) )
        if (iret .ne. 0) goto 120
!       NOMBRE DE POINTS DE DISCRETISATION DU TUBE
        call dismoi('REF_MASS_PREM', basemo, 'RESU_DYNA', repk=masse)
        call mtdscr(masse)
        call dismoi('NOM_MAILLA', masse, 'MATR_ASSE', repk=mailla)
!       RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE
        nomnoe = mailla//'.NOMNOE'
        call jelira(nomnoe, 'NOMUTI', lnoe)
        indic = lnoe
!
        call mditm1(nbmode, nbmd, nbmp, nbnli, indic,&
                    nbf, info, itrans, eps, icoupl,&
                    typflu, zi(iveci1), zl(jlocf), dt, tfexm,&
                    ts, iparch, ntotex, zk8(jnomfo), zi(jinumo),&
                    zr(jmasg), zr(jamo1), zr(jpuls), zr(ivecr3), zr(jdepl),&
                    zr(jparc), zk8(jnoec), zk8(jinti), zr( ivecr5), zr(ivecr1),&
                    zr(ivecr2), vgap, zr(ivecr4), nbchoc, zr( jdep0),&
                    zr(jvit0), zr(jamog), nbsauv)
    endif
!
120 continue
!
    if (iret .ne. 0) then
        call utmess('F', 'ALGORITH5_24')
    endif
!
    AS_DEALLOCATE(vr=pulsat2)
    call jedema()
end subroutine
