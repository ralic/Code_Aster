subroutine op0037()
    implicit none
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
! aslint: disable=W1501
!     OPERATEUR DE NORMALISATION DES MODES
!     ------------------------------------------------------------------
!
!     PARAMETRES "MODE_MECA"
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/gcucon.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/r8prem.h"
#include "asterc/r8vide.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/massdir.h"
#include "asterfort/mtdscr.h"
#include "asterfort/posddl.h"
#include "asterfort/pteddl.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsexis.h"
#include "asterfort/rsorac.h"
#include "asterfort/rsvpar.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/utnono.h"
#include "asterfort/vpcrea.h"
#include "asterfort/vpddl.h"
#include "asterfort/vpmain.h"
#include "asterfort/vpnor1.h"
#include "asterfort/vpnor2.h"
#include "asterfort/vpnorm.h"
#include "asterfort/vppfac.h"
#include "asterfort/vppgen.h"
#include "asterfort/vprecu.h"
#include "asterfort/vpstor.h"
#include "asterfort/wkvect.h"
#include "asterfort/wpnorm.h"
!-----------------------------------------------------------------------
    integer :: i, ib, ic, ideb, ie, ieq, ierd
    integer :: iex, ifin, ilgcon, im, ind, iprec, isign
    integer :: ival, l, ladpa, lcmp, lcoef, lg, lmod, lgr, ln
    integer :: lmode, lnorm, lnumor, lprod, lvali, lvalk, lvalr
    integer :: mosign, nbmod, nbmode, nbpafi, nbpafk, nbpafr
    integer :: nbpaft, nbpami, nbpamk, nbpamr, nbpamt, nbpara, nbpari
    integer :: nbpark, nbparr, nbtrou, ncmp, ncmpac, neq, npari
    integer :: npark, nparr, numddl, numnoe, tmod(1)
    real(kind=8) :: dir(3), xmastr(3), maxmas, dmass
!-----------------------------------------------------------------------
    parameter   ( nbpami=1 , nbpamr=15 , nbpamk=1, nbpamt=17 )
!     PARAMETRES "MODE_FLAMB"
    parameter   ( nbpafi=1 , nbpafr=1  , nbpafk=1, nbpaft=3  )
    integer :: lmat(2), ibid, ifm, niv, lddl2, vali, iret
    integer :: l1, l2, l3, lmasse, lraide, lamor, lddl
    real(kind=8) :: r8b
    complex(kind=8) :: c16b
    aster_logical :: lrefe, lbasm, lamo, lcmplx, lparam
    character(len=1) :: typmod
    character(len=24) :: valk(4)
    character(len=8) :: modeou, modein, nomcmp(7), k8b, cmp, noma, mat1, mat2, mat3, noeud
    character(len=14) :: nume
    character(len=16) :: typcon, nomcmd, norm, nomsy
    character(len=19) :: k19b, chamno
    character(len=24) :: masse, amor, raide, refe, method, kvec, kvali, kvalr
    character(len=24) :: kvalk, noparm(nbpamt), noparf(nbpaft), nopara(nbpamt)
    character(len=24) :: mate, cara, modele, typeba, nomgrn
!
    integer :: iarg
!     ------------------------------------------------------------------
    data  nomcmp / 'LAGR', 'DX', 'DY', 'DZ', 'DRX', 'DRY', 'DRZ' /
    data  kvec  / '&&OP0037.VAL_PROPRE'/
    data  kvali / '&&OP0037.GRAN_MODAL_I' /
    data  kvalr / '&&OP0037.GRAN_MODAL_R' /
    data  kvalk / '&&OP0037.GRAN_MODAL_K' /
    data  noparm /        'NUME_MODE'       , 'NORME'           ,&
     &  'FREQ'            , 'OMEGA2'          , 'AMOR_REDUIT'     ,&
     &  'MASS_GENE'       , 'RIGI_GENE'       , 'AMOR_GENE'       ,&
     &  'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,&
     &  'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,&
     &  'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ' /
    data  noparf / 'NUME_MODE'  , 'NORME'   ,'CHAR_CRIT'   /
!     ------------------------------------------------------------------
!
!     --- RECUPERATION DU RESULTAT ET DU MODE A TRAITER ---
    call jemarq()
    call getres(modeou, typcon, nomcmd)
    call gcucon(modeou, typcon, iex)
!
    lbasm = .false.
    lamo = .false.
    lcmplx= .false.
    lparam= .false.
!
    xmastr(1) = 0.d0
    xmastr(2) = 0.d0
    xmastr(3) = 0.d0
!
    call getvid('  ', 'MODE', iocc=1, scal=modein, nbret=l)
!
    call dismoi('TYPE_BASE', modein, 'RESU_DYNA', repk=typeba, arret='C',&
                ier=iret)
    if (typeba(1:1) .ne. ' ') lbasm = .true.
!
    if (iex .gt. 0) then
        if (modeou .ne. modein) then
            valk (1) = modeou
            valk (2) = modein
            call utmess('F', 'ALGELINE4_33', nk=2, valk=valk)
        endif
    endif
!
    if (typcon(1:11) .eq. 'MODE_MECA_C') then
        nomsy = 'DEPL'
        nbpari = nbpami
        nbparr = nbpamr - 9
        nbpark = nbpamk
        nbpara = nbpamt - 9
        do i = 1, nbpara
            nopara(i) = noparm(i)
        end do
    else if (typcon(1:9) .eq. 'MODE_MECA') then
        nomsy = 'DEPL'
!        --- VERIFIER SI TOUS LES PARAMETRES MODAUX EXISTENT DANS LA SD
!          - OU BIEN ILS SERONT CALCULES DANS NORM_MODE
!          - (CAS DES MODE_MECA NON DYNAMIQUES)
        call rsvpar(modein, 1, 'FACT_PARTICI_DX', ibid, r8vide(),&
                    k8b, l1)
        call rsvpar(modein, 1, 'FACT_PARTICI_DY', ibid, r8vide(),&
                    k8b, l2)
        call rsvpar(modein, 1, 'FACT_PARTICI_DZ', ibid, r8vide(),&
                    k8b, l3)
        if ((l1+l2+l3) .eq. 330) lparam = .true.
!
        nbpari = nbpami
        nbparr = nbpamr
        nbpark = nbpamk
        nbpara = nbpamt
        if (.not.(lbasm .or. lparam)) then
            nbparr = nbpamr - 9
            nbpara = nbpamt - 9
        endif
        do i = 1, nbpara
            nopara(i) = noparm(i)
        end do
    else if (typcon(1:10) .eq. 'MODE_FLAMB') then
        nomsy = 'DEPL'
        nbpari = nbpafi
        nbparr = nbpafr
        nbpark = nbpafk
        nbpara = nbpaft
        do i = 1, nbpara
            nopara(i) = noparf(i)
        end do
    else
        call utmess('F', 'ALGELINE2_33', sk=typcon)
    endif
!
!
!     ---RECUPERATION DU NIVEAU D'IMPRESSION---
!
    call infmaj()
    call infniv(ifm, niv)
!
    call jelira(modein//'           .ORDR', 'LONUTI', iret)
! SI LA BANDE DE FREQUENCE EST VIDE, ON NE FAIT RIEN
!  => DIRECT A LA FIN APRES UN PETIT MESSAGE D'INFO
    if (iret .eq. 0) then
        if (niv .ge. 1) then
            call utmess('I', 'ALGELINE7_5', sk=modein)
            call utmess('I', 'ALGELINE7_6')
        endif
        goto 999
    endif
!
!
!     --- PROTECTION DES OBJETS PERES (AYANT GENERE DES OBJETS .PAPA)
!
    call rsorac(modein, 'LONUTI', ibid, r8b, k8b,&
                c16b, 0.0d0, k8b, tmod, 1,&
                nbtrou)
    nbmod=tmod(1)            
    call wkvect('&&OP0037.NUMERO.ORDRE', 'V V I', nbmod, lnumor)
    call rsorac(modein, 'TOUT_ORDRE', ibid, r8b, k8b,&
                c16b, 0.0d0, k8b, zi(lnumor), nbmod,&
                nbtrou)
    do im = 1, nbmod
        call rsexch(' ', modein, 'DEPL', zi(lnumor+im-1), k19b,&
                    iret)
        call jeexin(k19b//'.PAPA', iret)
        if (iret .ne. 0) then
            call jelira(k19b//'.PAPA', 'LONUTI', ival)
            if (ival .ne. 0) then
                call utmess('F', 'ALGELINE2_34')
            endif
        endif
!
!       ------ AU PASSAGE, ON FAIT UN TEST SUR LE TYPE DES MODES
!              (REEL OU COMPLEXE)
        call jelira(k19b//'.VALE', 'TYPE', cval=typmod)
        if (typmod .eq. 'C') lcmplx = .true.
!
    end do
!
!     --- INITIALISATION ---
    norm = ' '
    noeud = ' '
    typeba = ' '
    ncmp = 0
    ideb = 0
    ifin = 0
    lcmp = 1
    lmat(1) = 0
    lmat(2) = 0
    lddl = 1
!
!     --- MATRICES DE REFERENCE DES MODES ---
    lrefe = .true.
    if (lbasm) then
        call getvid(' ', 'RAIDE', scal=mat1, nbret=l1)
        call getvid(' ', 'MASSE', scal=mat2, nbret=l2)
        call getvid(' ', 'AMOR', scal=mat3, nbret=l3)
        if (l1 .eq. 0) then
            call dismoi('REF_RIGI_PREM', modein, 'RESU_DYNA', repk=mat1, arret='C',&
                        ier=iret)
            if (iret .eq. 0) then
                l1 = 1
                call utmess('A+', 'ALGELINE_1', nk=1, valk=[modein])
                call utmess('A', 'ALGELINE_2', nk=1, valk=[mat1])
            endif
        endif
        if (l2 .eq. 0) then
            call dismoi('REF_MASS_PREM', modein, 'RESU_DYNA', repk=mat2, arret='C',&
                        ier=iret)
            if (iret .eq. 0) then
                l2 = 1
                call utmess('A+', 'ALGELINE_1', nk=1, valk=[modein])
                call utmess('A', 'ALGELINE_3', nk=1, valk=[mat2])
            endif
        endif
        if (l3 .eq. 0) then
            call dismoi('REF_AMOR_PREM', modein, 'RESU_DYNA', repk=mat3, arret='C',&
                        ier=iret)
            if (iret .eq. 0) then
                l3 = 1
            endif
        endif
        if ((l1*l2) .eq. 0) then
            call utmess('F+', 'ALGELINE_1', nk=1, valk=[modein])
            call utmess('F', 'ALGELINE_6')
        endif
        masse = mat2
        raide = mat1
        amor = ' '
        lamo=.false.
        if ((l3 .ne. 0) .and. (mat3 .ne. ' ')) then
            lamo=.true.
            amor=mat3
        endif
    else
        call dismoi('REF_RIGI_PREM', modein, 'RESU_DYNA', repk=raide, arret='C')
        call dismoi('REF_MASS_PREM', modein, 'RESU_DYNA', repk=masse, arret='C')
        call dismoi('REF_AMOR_PREM', modein, 'RESU_DYNA', repk=amor, arret='C')
        if (raide .eq. ' ') then
            lrefe = .false.
            call rsexch(' ', modein, 'DEPL', 1, chamno,&
                        iret)
            refe = k19b//'.REFE'
            call jeveuo(refe, 'L', lmode)
            noma = zk24(lmode )(1:8)
            nume = zk24(lmode+1)(1:14)
!           --- Ne pas calculer de masses unitaires pour ce cas
            goto 100
        endif
    endif
!
!
!     --- NUMEROTATION ASSOCIEE AUX DDL ---
    call dismoi('NOM_NUME_DDL', raide, 'MATR_ASSE', repk=nume)
    call dismoi('NOM_MAILLA', raide, 'MATR_ASSE', repk=noma)
    call dismoi('CARA_ELEM', raide, 'MATR_ASSE', repk=cara)
    call dismoi('CHAM_MATER', raide, 'MATR_ASSE', repk=mate)
    call dismoi('NOM_MODELE', raide, 'MATR_ASSE', repk=modele)
!
!     --- COMPATIBILITE DES MODES ---
    if (lbasm) then
        call jeexin(modeou//'           .REFD', l1)
        call jeexin(modeou//'           .INDI', l2)
        if (l1 .gt. 0) call jedetr(modeou//'           .REFD')
        if (l2 .gt. 0) call jedetr(modeou//'           .INDI')
    end if
    call vpcrea(0, modeou, masse, amor, raide,&
                nume, ibid)
!
!
!     --- POUR LES MODES DE FLAMBAGE PAS DE MASSE UNITAIRE ---
    if (typcon(1:10) .eq. 'MODE_FLAMB') then 
        goto 100
    end if
    
    maxmas = 0.d0
    do i = 1, 3
        dir(1) = 0.d0
        dir(2) = 0.d0
        dir(3) = 0.d0
        dir(i) = 1.d0
        call massdir(masse, dir, dmass)
        xmastr(i) = dmass
        maxmas = max(maxmas, dmass)
    end do

    if (maxmas .le. r8prem()) then
        call utmess('I', 'ALGELINE5_58')
    endif
!
100 continue
!
!     --- OPTION DE NORMALISATION  ---
    method = '                        '
    call getvtx(' ', 'NORME', scal=norm, nbret=l)
    if (l .ne. 0) then
        if (norm .eq. 'MASS_GENE') then
!        --- CALCUL DE LA MASSE DU MODELE
            if (.not.lrefe) then
                call utmess('F', 'ALGELINE2_35')
            endif
            if (lbasm .and. lcmplx .and. (amor.eq.' ')) then
                call utmess('F', 'ALGELINE_8')
            endif
            method(1:9) = 'MASS_GENE'
            call mtdscr(masse)
            call jeveuo(masse(1:19)//'.&INT', 'E', lmat(1))
            if (amor .ne. ' ') then
                call mtdscr(amor)
                call jeveuo(amor(1:19)//'.&INT', 'E', lmat(2))
            endif
        else if (norm .eq. 'RIGI_GENE') then
            if (.not.lrefe) then
                call utmess('F', 'ALGELINE2_35')
            endif
            if (lbasm .and. lcmplx .and. (amor.eq.' ')) then
                call utmess('F', 'ALGELINE_8')
            endif
            method(1:9) = 'RAID_GENE'
            call mtdscr(raide)
            call jeveuo(raide(1:19)//'.&INT', 'E', lmat(1))
            if (amor .ne. ' ') then
                call mtdscr(masse)
                call jeveuo(masse(1:19)//'.&INT', 'E', lmat(2))
            endif
        else if (norm .eq. 'EUCL') then
            method(1:4) = 'EUCL'
            ncmp = 1
            call wkvect('&&OP0037.LISTE.CMP', 'V V K8', ncmp, lcmp)
            zk8(lcmp) = nomcmp(1)
        else if (norm .eq. 'EUCL_TRAN') then
            method(1:9) = 'EUCL_TRAN'
            ncmp = 3
            call wkvect('&&OP0037.LISTE.CMP', 'V V K8', ncmp, lcmp)
            zk8(lcmp) = nomcmp(2)
            zk8(lcmp+1) = nomcmp(3)
            zk8(lcmp+2) = nomcmp(4)
        else if (norm .eq. 'TRAN') then
            method(1:4) = 'TRAN'
            ncmp = 3
            call wkvect('&&OP0037.LISTE.CMP', 'V V K8', ncmp, lcmp)
            zk8(lcmp) = nomcmp(2)
            zk8(lcmp+1) = nomcmp(3)
            zk8(lcmp+2) = nomcmp(4)
            norm = 'AVEC_CMP'
        else if (norm .eq. 'ROTA') then
            method(1:4) = 'ROTA'
            ncmp = 3
            call wkvect('&&OP0037.LISTE.CMP', 'V V K8', ncmp, lcmp)
            zk8(lcmp) = nomcmp(5)
            zk8(lcmp+1) = nomcmp(6)
            zk8(lcmp+2) = nomcmp(7)
            norm = 'AVEC_CMP'
        else if (norm .eq. 'TRAN_ROTA') then
            method(1:9) = 'TRAN_ROTA'
            ncmp = 6
            call wkvect('&&OP0037.LISTE.CMP', 'V V K8', ncmp, lcmp)
            zk8(lcmp) = nomcmp(2)
            zk8(lcmp+1) = nomcmp(3)
            zk8(lcmp+2) = nomcmp(4)
            zk8(lcmp+3) = nomcmp(5)
            zk8(lcmp+4) = nomcmp(6)
            zk8(lcmp+5) = nomcmp(7)
            norm = 'AVEC_CMP'
        else
            valk (1) = norm
            vali = ibid
            call utmess('F', 'ALGELINE4_36', sk=valk(1), si=vali)
        endif
    endif
!
    call getvem(noma, 'NOEUD', ' ', 'NOEUD', 1,&
                iarg, 1, noeud, ln)
    call getvtx(' ', 'GROUP_NO', scal=nomgrn, nbret=lgr)
!
    if (lgr .ne. 0) then
        call utnono(' ', noma, 'NOEUD', nomgrn, noeud,&
                    l)
        if (l .eq. 10) then
            call utmess('F', 'ELEMENTS_67', sk=nomgrn)
        else if (l.eq.1) then
            valk(1) = nomgrn
            valk(2) = noeud
            call utmess('A', 'SOUSTRUC_87', nk=2, valk=valk)
        endif
    endif
!
    if ((ln .ne. 0) .or. (lgr .ne. 0)) then
        norm = 'POINT'
        ncmp = 1
        method(1:6) = 'NOEUD:'
        ideb = 7
        lg = lxlgut(noeud)
        ifin = ideb + lg
        method(ideb:ifin) = ' '//noeud(1:lg)
        call wkvect('&&OP0037.LISTE.CMP', 'V V K8', ncmp, lcmp)
        call getvtx(' ', 'NOM_CMP', scal=zk8(lcmp), nbret=l)
        if (lrefe) then
            call posddl('NUME_DDL', nume, noeud, zk8(lcmp), numnoe,&
                        numddl)
        else
            call posddl('CHAM_NO', chamno, noeud, zk8(lcmp), numnoe,&
                        numddl)
        endif
        if (numnoe .eq. 0) then
            call utmess('F', 'ALGELINE2_36')
        endif
        if (numddl .eq. 0) then
            call utmess('F', 'ALGELINE2_37')
        endif
        ideb = ifin + 1
        do ic = 1, ncmp
            lg = lxlgut(zk8(lcmp+ic-1))
            ifin = ideb + lg
            if (ifin .gt. 24) then
                method(22:24) = '...'
                goto 52
            endif
            method(ideb:ifin) = ' '//zk8(lcmp+ic-1)(1:lg)
            ideb = ideb + lg + 1
        end do
 52     continue
    endif
!
    call getvtx(' ', 'AVEC_CMP', nbval=0, nbret=l)
    if (l .ne. 0) then
        norm = 'AVEC_CMP'
        ncmp = -l
        method(1:9) = 'AVEC_CMP:'
        call wkvect('&&OP0037.LISTE.CMP', 'V V K8', ncmp, lcmp)
        call getvtx(' ', 'AVEC_CMP', nbval=ncmp, vect=zk8(lcmp), nbret=l)
        ideb = 10
        do ic = 1, ncmp
            lg = lxlgut(zk8(lcmp+ic-1))
            ifin = ideb + lg
            if (ifin .gt. 24) then
                method(22:24) = '...'
                goto 32
            endif
            method(ideb:ifin) = ' '//zk8(lcmp+ic-1)(1:lg)
            ideb = ideb + lg + 1
        end do
 32     continue
    endif
!
    call getvtx(' ', 'SANS_CMP', nbval=0, nbret=l)
    if (l .ne. 0) then
        norm = 'SANS_CMP'
        ncmp = -l
        method(1:9) = 'SANS_CMP:'
        call wkvect('&&OP0037.LISTE.CMP', 'V V K8', ncmp, lcmp)
        call getvtx(' ', 'SANS_CMP', nbval=ncmp, vect=zk8(lcmp), nbret=l)
        ideb = 10
        do ic = 1, ncmp
            lg = lxlgut(zk8(lcmp+ic-1))
            ifin = ideb + lg
            if (ifin .gt. 24) then
                method(22:24) = '...'
                goto 42
            endif
            method(ideb:ifin) = ' '//zk8(lcmp+ic-1)(1:lg)
            ideb = ideb + lg + 1
        end do
 42     continue
    endif
!
    if (niv .ge. 1) then
        call utmess('I', 'ALGELINE7_5', sk=modein)
        if (lbasm) then
            call utmess('I', 'ALGELINE7_7')
            do im = 1, nbmod
                valk(1) = method
                vali = zi(lnumor+im-1)
                call utmess('I', 'ALGELINE7_8', sk=valk(1), si=vali)
            end do
        else
            call utmess('I', 'ALGELINE7_9')
            do im = 1, nbmod
                call rsadpa(modein, 'L', 1, 'NORME', zi(lnumor+im-1),&
                            0, sjv=ladpa, styp=k8b)
                valk(1) = zk24(ladpa)
                valk(2) = method
                vali = zi(lnumor+im-1)
                call utmess('I', 'ALGELINE7_10', nk=2, valk=valk, si=vali)
            end do
        endif
    endif
!
!     --- RECUPERATION DES VECTEURS PROPRES ET DES GRANDEURS MODALES ---
!
    call vprecu(modein, nomsy, nbmod, zi(lnumor), kvec,&
                nbpara, nopara(1), kvali, kvalr, kvalk,&
                neq, nbmode, typmod, npari, nparr,&
                npark)
!
    if (.not.lbasm) then
        if (npari .ne. nbpari) then
            call utmess('F', 'ALGELINE2_38')
        endif
        if (nparr .ne. nbparr) then
            call utmess('F', 'ALGELINE2_39')
        endif
        if (npark .ne. nbpark) then
            call utmess('F', 'ALGELINE2_40')
        endif
    endif
!
!
!     --- RECUPERATION DES COMPOSANTES ---
    if (norm .eq. 'AVEC_CMP' .or. norm .eq. 'SANS_CMP' .or. norm(1:4) .eq. 'EUCL') then
        call wkvect('&&OP0037.POSITION.DDL', 'V V I', neq*ncmp, lddl)
        if (lrefe) then
            call pteddl('NUME_DDL', nume, ncmp, zk8(lcmp), neq,&
                        tabl_equa = zi(lddl))
        else
            call pteddl('CHAM_NO', chamno, ncmp, zk8(lcmp), neq,&
                        tabl_equa = zi(lddl))
        endif
        do ic = 2, ncmp
            ind = (ic-1)*neq
            do ie = 0, neq-1
                zi(lddl+ie)= max(zi(lddl+ind+ie),zi(lddl+ie))
            end do
        end do
        if (norm .eq. 'SANS_CMP' .or. norm .eq. 'EUCL') then
            do ie = 0, neq-1
                zi(lddl+ie)= 1-zi(lddl+ie)
            end do
            if (norm .eq. 'SANS_CMP') norm='AVEC_CMP'
        endif
    else if (norm.eq.'POINT') then
        call wkvect('&&OP0037.POSITION.DDL', 'V V I', neq, lddl)
        zi(lddl+numddl-1) = 1
        norm = 'AVEC_CMP'
    endif
!
!     --- CALCUL DU NOMBRE DE COMPOSANTES ACTIVES ---
    if (ncmp .gt. 0) then
        ncmpac = 0
        do ieq = 0, neq-1
            ncmpac = ncmpac + zi(lddl+ieq)
        end do
        if (ncmpac .lt. 1) then
            call utmess('F', 'ALGELINE2_41')
        endif
    endif
!
!     --- SIGNE DES MODES ---
    isign = 0
    call getfac('MODE_SIGNE', mosign)
    if (mosign .ne. 0) then
        call getvem(noma, 'NOEUD', 'MODE_SIGNE', 'NOEUD', 1,&
                    iarg, 1, noeud, l)
        if (l .eq. 0) then
            call getvtx('MODE_SIGNE', 'GROUP_NO', iocc=1, scal=nomgrn, nbret=l)
            call utnono(' ', noma, 'NOEUD', nomgrn, noeud,&
                        l)
            if (l .eq. 10) then
                call utmess('F', 'ELEMENTS_67', sk=nomgrn)
            else if (l.eq.1) then
                valk(1) = nomgrn
                valk(2) = noeud
                call utmess('A', 'SOUSTRUC_87', nk=2, valk=valk)
            endif
        endif
        call getvtx('MODE_SIGNE', 'NOM_CMP', iocc=1, scal=cmp, nbret=l)
        if (lrefe) then
            call posddl('NUME_DDL', nume, noeud, cmp, numnoe,&
                        numddl)
        else
            call posddl('CHAM_NO', chamno, noeud, cmp, numnoe,&
                        numddl)
        endif
        if (numnoe .eq. 0) then
            call utmess('F', 'ALGELINE2_36')
        endif
        if (numddl .eq. 0) then
            call utmess('F', 'ALGELINE2_37')
        endif
        isign = 1
        call getvtx('MODE_SIGNE', 'SIGNE', iocc=1, scal=k8b, nbret=l)
        if (k8b(1:7) .eq. 'NEGATIF') isign = -1
        if (typmod .eq. 'C') then
            isign = 0
            call utmess('A', 'ALGELINE2_43')
        endif
    endif
!
    ierd = 1
    call wkvect('&&OP0037.COEF_MODE', 'V V R', nbmode, lcoef)
!
    call jeveuo(kvec, 'E', lmod)
    call jeveuo(kvali, 'E', lvali)
    call jeveuo(kvalr, 'E', lvalr)
    call jeveuo(kvalk, 'E', lvalk)
!
    if (lbasm .and. .not.(lparam)) then
        call mtdscr(masse)
        call jeveuo(masse(1:19)//'.&INT', 'E', lmasse)
        call mtdscr(raide)
        call jeveuo(raide(1:19)//'.&INT', 'E', lraide)
        if (lamo) then
            call mtdscr(amor)
            call jeveuo(amor(1:19)//'.&INT', 'E', lamor)
        else
            lamor=0
        endif
!
        call jeveuo(kvec, 'L', lmod)
!
!       CALCUL DES PARAMETRES GENERALISES
        call wkvect('&&OP0037.POSI.DDL', 'V V I', neq, lddl2)
        call wkvect('&&OP0037.DDL.BLOQ.CINE', 'V V I', neq, lprod)
        call vpddl(raide(1:19), masse(1:19), neq, ib, ib,&
                   ib, zi(lddl2), zi(lprod), ierd)
        call vppgen(lmasse, lamor, lraide, zr(lvalr+3*nbmode), zr(lvalr+ 5*nbmode),&
                    zr(lvalr+4*nbmode), zr(lmod), neq, nbmode, zi(lprod))
!
!       CALCUL DES FACTEURS DE PARTICIPATIONS ET DES MASSES EFFECTIVES
        call vppfac(lmasse, zr(lvalr+3*nbmode), zr(lmod), neq, nbmode,&
                    nbmode, zr(lvalr+6*nbmode), zr(lvalr+9*nbmode))
        lparam = .true.
    endif
!
!     --- NORMALISATION DES MODES ET ARCHIVAGE ---
    ilgcon = lxlgut(typcon)
    if (typcon(ilgcon-1:ilgcon) .eq. '_C') ilgcon = ilgcon -2
    call rsexis(modeou, iret)
    if (iret .eq. 0) call rscrsd('G', modeou, typcon(:ilgcon), nbmode)
    iprec = 0
!
    if (typmod .eq. 'R') then
        if (typcon(1:10) .eq. 'MODE_FLAMB') then
            call vpnor1(norm, neq, nbmode, zi(lddl), zr(lmod),&
                        isign, numddl, zr(lcoef))
        else
            if (lparam) then
                call vpnorm(norm, 'OUI', lmat(1), neq, nbmode,&
                            zi(lddl), zr(lmod), zr(lvalr), xmastr, isign,&
                            numddl, zr(lcoef))
            else
                call vpnorm(norm, 'NON', lmat(1), neq, nbmode,&
                            zi(lddl), zr(lmod), zr(lvalr), xmastr, isign,&
                            numddl, zr(lcoef))
            endif
        endif
        call vpstor(-1, typmod, modeou, nbmode, neq,&
                    zr(lmod), zc(1), nbmode, nbpari, nbparr,&
                    nbpark, nopara, '    ', zi(lvali), zr(lvalr),&
                    zk24(lvalk), iprec)
        call vpnor2(modeou, nbmode, zi(lnumor), zr(lcoef))
!
    else if (typmod .eq. 'C') then
!
        if (lparam) then
            call wpnorm(norm, 'OUI', lmat, neq, nbmode,&
                        zi(lddl), zc(lmod), zr(lvalr), zr(lcoef))
        else
            call wpnorm(norm, 'NON', lmat, neq, nbmode,&
                        zi(lddl), zc(lmod), zr(lvalr), zr(lcoef))
        endif

        call vpstor(-1, typmod, modeou, nbmode, neq,&
                    zr(1), zc(lmod), nbmode, nbpari, nbparr,&
                    nbpark, nopara, '    ', zi(lvali), zr(lvalr),&
                    zk24(lvalk), iprec)
!
    else
        call utmess('F', 'ALGELINE2_44', sk=typmod)
    endif
!
    do im = 1, nbmode
        call rsadpa(modeou, 'E', 1, 'NORME', zi(lnumor+im-1),&
                    0, sjv=lnorm, styp=k8b)
        zk24(lnorm) = method
    end do
!
!     --- ON MET UN TITRE ----
    call titre()
!
!
999 continue
    call jedema()
end subroutine
