subroutine op0044()
!-----------------------------------------------------------------------
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
!        MODE_ITER_INV
!     RECHERCHE DE MODES PROPRES PAR LA METHODE D'ITERATION INVERSE
!     ------------------------------------------------------------------
!        - POUR LE PROBLEME GENERALISE AUX VALEURS PROPRES.
!                         2
!                        L (M) Y  + (K) Y = 0
!
!          LES MATRICES (K), (C) ET (M) SONT REELLES SYMETRIQUES
!          LES VALEURS PROPRES ET DES VECTEURS PROPRES SONT REELS
!
!        - POUR LE PROBLEME QUADRATIQUE AUX VALEURS PROPRES.
!                         2
!                        L (M) Y  + L (C) Y + (K) Y = 0
!
!          LES MATRICES (K), (C) ET (M) SONT REELLES SYMETRIQUES
!          LES VALEURS PROPRES ET DES VECTEURS PROPRES SONT REELS OU
!          COMPLEXES CONJUGUEES
!
!     ------------------------------------------------------------------
! LOC NFREQ  : IS : NB DE FREQUENCES DONNEES PAR L'UTILISATEUR
! LOC MXFREQ : IS : NB MAXIMUM DE FREQUENCES A CALCULER
! LOC NFREQB : IS : NB DE FREQUENCES EFFECTIVES DANS LA BANDE DONNEE
!-----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
    implicit none
!
! VARIABLES LOCALES
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/isnnem.h"
#include "asterc/r8depi.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/cresol.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/omega2.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/vp1pro.h"
#include "asterfort/vpcntl.h"
#include "asterfort/vpcrea.h"
#include "asterfort/vpddl.h"
#include "asterfort/vpdich.h"
#include "asterfort/vpfopr.h"
#include "asterfort/vpinte.h"
#include "asterfort/vppara.h"
#include "asterfort/vpwecf.h"
#include "asterfort/vrrefe.h"
#include "asterfort/wkvect.h"
#include "asterfort/wp1inv.h"
#include "asterfort/wp1mul.h"
!
    integer :: nbpari, nbparr, nbpark, nbpara, mxddl
    parameter     ( nbpari=8 , nbparr=16 , nbpark=3, nbpara=27 )
    parameter     ( mxddl = 1 )
!
    integer :: indf, ifreq, ifm, iret, ierfr, ierd, idet1, ieme1, ierx, idet2
    integer :: ieme2, ieq, i, ibid, npivot(2)
    integer :: jvalp, jdet, jidet, jieme, jnpas, kfreq, k
    integer :: lmat(3), l, lamor, ltypre, lbrss, lmo, lmf, lborne, lmasse
    integer :: lraide, ldynam, lfreq, lamort, lddl, lprod, nblagr, lresui
    integer :: lresur, lresuk, lvalp, lvec
    integer :: mxfreq, ncritr, nbrss, nitsep, nitaju, nitv, idet(2), nfreqr
    integer :: nfreq, ncrit, nbmod, na1, namorr, niv, nbcine, krefa, neqact
    integer :: nfreqb, mxresf, ndim, nparr, neq, islvk, islvi, jrefa
!
    real(kind=8) :: tolsep, tolaju, tolv, fcorig, omecor, precsh, omeg, det1
    real(kind=8) :: det2, fr, am, zam(3), zfr(3), seuil, fmin, fmax, omgmin
    real(kind=8) :: omgmax, rbid, depi, undf, raux1, raux2, det(2)
    character(len=1) :: ctyp, typer
    character(len=8) :: optiov, modes, knega
    character(len=9) :: typevp
    character(len=14) :: matra, matrb, matrc
    character(len=16) :: nomcmd, typcon, optiom, optiof, optior, typres, k16bid
    character(len=19) :: masse, raide, amor, dynam, numedd, solveu
    character(len=24) :: cborne, work(5), camor, cfreq, nopara(nbpara), metres
    character(len=24) :: valk(2)
    complex(kind=8) :: cbid, dcmplx
    logical :: lbid
    character(len=1) :: ktyp
!     ------------------------------------------------------------------
    data zam     / 0.01d0 , 0.02d0 , 0.03d0 /
    data zfr     / -1.0d0 , 1.00d0 , 0.00d0 /
    data  work(1)/ '&&OP0044.VALEURS_PROPRES' /
    data  work(2)/ '&&OP0044.MANTISSE_DET   ' /
    data  work(3)/ '&&OP0044.EXPOSANT_DET   ' /
    data  work(4)/ '&&OP0044.POSITION       ' /
    data  work(5)/ '&&OP0044.NOMBRE_ITERE   ' /
    data  cborne / '&&OP0044.BORNE.USR ' /
    data  camor  / '&&OP0044.AMOR.USR ' /
    data  cfreq  / '&&OP0044.CFREQ.USR ' /
    data  nopara /&
     &  'NUME_MODE'       , 'ITER_QR'         , 'ITER_BATHE'      ,&
     &  'ITER_ARNO'       , 'ITER_JACOBI'     , 'ITER_SEPARE'     ,&
     &  'ITER_AJUSTE'     , 'ITER_INVERSE'    ,&
     &  'NORME'           , 'METHODE'         , 'TYPE_MODE'       ,&
     &  'FREQ'            ,&
     &  'OMEGA2'          , 'AMOR_REDUIT'     , 'ERREUR'          ,&
     &  'MASS_GENE'       , 'RIGI_GENE'       , 'AMOR_GENE'       ,&
     &  'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,&
     &  'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,&
     &  'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ' /
!     ------------------------------------------------------------------
!
    call jemarq()
!
    cbid=(0.d0,0.d0)
    undf = r8vide()
    indf = isnnem()
    det1 = 0.d0
    det2 = 0.d0
    idet1 = 0
    idet2 = 0
!
!     --- RECUPERATION DU RESULTAT  ---
    call getres(modes, typcon, nomcmd)
!     ------------------------------------------------------------------
!
!     --- TYPE DE CALCUL : DYNAMIQUE OU FLAMBEMENT OU GENERAL  ---
!     TYPE_RESU : 'DYNAMIQUE' OU 'MODE_FLAMB' OU 'GENERAL'
    call getvtx(' ', 'TYPE_RESU', scal=typres, nbret=ltypre)
!
!     --- CATALOGUE DE COMMANDE, DIFFERENT SELON LE TYPE_RESU
!     -> ON STOCKE DANS DES VARIABLES POUR EVITER DE FAIRE DES GETXXX
!     POUR CHAQUE TYPE_RESU
!     POUR L'INSTANT TYPE_RESU='GENERAL' REVIENT A 'MODE_FLAMB'
!     SAUF LE NOM DES MATRICES
    if (typres .eq. 'DYNAMIQUE') then
        matra = 'MATR_RIGI'
        matrb = 'MATR_MASS'
        matrc = 'MATR_AMOR'
        typevp = 'FREQ'
    else if (typres .eq. 'MODE_FLAMB') then
        matra = 'MATR_RIGI'
        matrb = 'MATR_RIGI_GEOM'
        typevp = 'CHAR_CRIT'
    else if (typres .eq. 'GENERAL') then
        matra = 'MATR_A'
        matrb = 'MATR_B'
        matrc = 'MATR_C'
        typevp = 'CHAR_CRIT'
        typres='MODE_FLAMB'
    endif
!
!     --- RECUPERATION DES ARGUMENTS MATRICIELS
    amor = ' '
    call getvid(' ', matra, scal=raide, nbret=l)
    call getvid(' ', matrb, scal=masse, nbret=l)
    lamor=0
    if (typres .ne. 'MODE_FLAMB') then
        call getvid(' ', matrc, scal=amor, nbret=lamor)
    endif
!
!     ON NE SAIT TRAITER QUE LE CAS DE LA MATRICE DE RAIDEUR REELLE
    ktyp='R'
!
!     --- COMPATIBILITE DES MODES (DONNEES ALTEREES) ---
    call exisd('MATR_ASSE', raide, ibid)
    if (ibid .ne. 0) then
        call dismoi('NOM_NUME_DDL', raide, 'MATR_ASSE', repk=numedd)
    else
        numedd=' '
    endif
    call vpcrea(0, modes, masse, amor, raide,&
                numedd, i)
!
!     TYPE_RESU : 'DYNAMIQUE' OU 'FLAMBEMENT'
    call getvr8('CALC_'//typevp, typevp, iocc=1, nbval=0, nbret=ncritr)
!
!     --- RECUPERATION DES ARGUMENTS CONCERNANT LE NOMBRE DE SHIFT ---
    call getvis('CALC_'//typevp, 'NMAX_ITER_SHIFT', iocc=1, scal=nbrss, nbret=lbrss)
!
!     --- OPTION DES FREQUENCES ET DES MODES  ---
    call getvtx('CALC_MODE', 'OPTION', iocc=1, scal=optiom, nbret=lmo)
    call getvtx('CALC_'//typevp, 'OPTION', iocc=1, scal=optiof, nbret=lmf)
!
!     --- RECUPERATION DES ARGUMENTS POUR LE CALCUL DES FREQUENCES ---
    call getvis('CALC_'//typevp, 'NMAX_'//typevp, iocc=1, scal=mxfreq, nbret=l)
    call getvr8('CALC_'//typevp, 'PREC_SEPARE', iocc=1, scal=tolsep, nbret=l)
    call getvis('CALC_'//typevp, 'NMAX_ITER_SEPARE', iocc=1, scal=nitsep, nbret=l)
    call getvr8('CALC_'//typevp, 'PREC_AJUSTE', iocc=1, scal=tolaju, nbret=l)
    call getvis('CALC_'//typevp, 'NMAX_ITER_AJUSTE', iocc=1, scal=nitaju, nbret=l)
    call getvr8('CALC_'//typevp, 'SEUIL_'//typevp, iocc=1, scal=fcorig, nbret=l)
    call getvr8('CALC_'//typevp, 'PREC_SHIFT', iocc=1, scal=precsh, nbret=l)
    if (typres .eq. 'DYNAMIQUE') then
        omecor = omega2(fcorig)
    else
        omecor = fcorig
    endif
!
!     --- RECUPERATION DES ARGUMENTS POUR LE CALCUL DES MODES ---
    call getvr8('CALC_MODE', 'PREC', iocc=1, scal=tolv, nbret=l)
    call getvis('CALC_MODE', 'NMAX_ITER', iocc=1, scal=nitv, nbret=l)
!
!
    if (optiof .eq. 'SEPARE' .or. optiof .eq. 'AJUSTE') then
        call getvr8('CALC_'//typevp, typevp, iocc=1, nbval=0, nbret=nfreqr)
        call getvr8('CALC_'//typevp, typevp, iocc=1, nbval=0, nbret=ncritr)
!
        nfreq = -nfreqr
        ncrit = -ncritr
        nbmod = max(nfreq,ncrit)
        if (nbmod .lt. 2) then
            valk(1) = optiof
            valk(2) = typevp
            call utmess('E', 'ALGELINE2_52', nk=2, valk=valk)
        else
            call wkvect(cborne, 'V V R', nbmod, lborne)
            if (nfreq .ne. 0) then
                call getvr8('CALC_'//typevp, typevp, iocc=1, nbval=nfreq, vect=zr(lborne),&
                            nbret=l)
            else
                call getvr8('CALC_'//typevp, typevp, iocc=1, nbval=nfreq, vect=zr(lborne),&
                            nbret=l)
            endif
            call jedetr(cborne)
        endif
    endif
    na1=0
    if (typres .ne. 'MODE_FLAMB') then
        call getvr8('CALC_'//typevp, 'AMOR_REDUIT', iocc=1, nbval=0, nbret=na1)
    endif
    namorr = na1
    if ((lamor.eq.0) .and. (namorr.ne.0)) then
        call utmess('E', 'ALGELINE2_55')
    endif
    if ((lamor.ne.0) .and. (namorr.ne.0) .and. (optiof.ne.'PROCHE')) then
        call utmess('E', 'ALGELINE2_56')
    endif
    if (optiof .eq. 'PROCHE') then
        call getvr8('CALC_'//typevp, typevp, iocc=1, nbval=0, nbret=nfreqr)
        if ((namorr.ne.0) .and. (namorr.ne.nfreqr)) then
            call utmess('E', 'ALGELINE2_57')
        endif
    endif
!
!     ------------------------------------------------------------------
!
!     ---RECUPERATION DU NIVEAU D'IMPRESSION---
!
    call infmaj()
    call infniv(ifm, niv)
!
!     --- VERIFICATION DES "REFE" ---
    call vrrefe(masse, raide, iret)
    if (iret .gt. 0) then
        valk(1) = raide
        valk(2) = masse
        call utmess('F', 'ALGELINE2_58', nk=2, valk=valk)
    endif
    if (lamor .ne. 0) call vrrefe(masse, amor, iret)
    if (iret .gt. 0) then
        valk(1) = amor
        valk(2) = masse
        call utmess('F', 'ALGELINE2_58', nk=2, valk=valk)
    endif
!
!
!     -----------------------------------------------------------------
!     ----------- LECTURE/TRAITEMENT SD SOLVEUR LINEAIRE  -----------
!     -----------------------------------------------------------------
!     -- LECTURE DES PARAMETRES SOLVEURS LINEAIRES ET CREATION DE
!        LA SD SOLVEUR ASSOCIEE. CETTE SD SOLVEUR EST LOCALE A L'OPERA
!        TEUR. POUR CE CALCUL, C'EST ELLE QUI EST UTILISEE POUR PARAME
!        TREE LE SOLVEUR LINEAIRE, ET NON PAS LA SD SOLVEUR CREE PAR LA
!        CMDE ECLATEE NUME_DDL LORS DE LA CONSTITUTION DES MATRICES.
    call jeveuo(raide//'.REFA', 'L', jrefa)
    solveu='&&OP0044.SOLVEUR'
    call cresol(solveu)
    call jeveuo(solveu//'.SLVK', 'L', islvk)
    call jeveuo(solveu//'.SLVI', 'L', islvi)
    metres=zk24(islvk)
    if ((metres(1:4).ne.'LDLT') .and. (metres(1:10).ne.'MULT_FRONT') .and.&
        (metres(1:5).ne.'MUMPS')) then
        call utmess('F', 'ALGELINE5_71')
    endif
!
!     --- CREATION DE LA MATRICE DYNAMIQUE ---
    typer = 'R'
    dynam = '&&OP0044.DYNAMIQUE'
    call mtdefs(dynam, raide, 'V', typer)
    call jeveuo(dynam(1:19)//'.REFA', 'E', krefa)
    zk24(krefa-1+7)=solveu
!
!     --- CREATION DES DESCRIPTEURS NORMALISES DE MATRICE ---
    call mtdscr(masse)
    call jeveuo(masse(1:19)//'.&INT', 'E', lmasse)
    if (lamor .ne. 0) then
        call mtdscr(amor)
        call jeveuo(amor(1:19)//'.&INT', 'E', lamor)
    endif
    call mtdscr(raide)
    call jeveuo(raide(1:19)//'.&INT', 'E', lraide)
    call mtdscr(dynam)
    call jeveuo(dynam(1:19)//'.&INT', 'E', ldynam)
!
    neq = zi( ldynam + 2 )
!
!     TEST DE LA VALIDITE DES MATRICES PAR RAPPORT AU PERIMETRE DU
!     TEST DE STURM
    if ((zi(lmasse+3).ne.1) .or. (zi(lmasse+4).ne.1)) then
        valk(1)=masse
        call utmess('F', 'ALGELINE3_48', sk=valk(1))
    endif
    if ((zi(lraide+3).ne.1) .or. (zi(lraide+4).ne.1)) then
        valk(1)=raide
        call utmess('F', 'ALGELINE3_48', sk=valk(1))
    endif
!     ------------------------------------------------------------------
!
!     --- OPTION DES FREQUENCES ET DES MODES  ---
    call getvtx('CALC_'//typevp, 'OPTION', iocc=1, scal=optiof, nbret=lmf)
    call getvtx('CALC_MODE', 'OPTION', iocc=1, scal=optiom, nbret=lmo)
!
    optior = 'SEPARE'
    if ((lamor.ne.0) .and. (optiof.eq.'AJUSTE')) then
        optiof = 'SEPARE'
        optior = 'AJUSTE'
    endif
!
!     --- LISTE DE FREQUENCES REELLES ---
    nfreqr = 0
    ncritr = 0
    if (typres .eq. 'DYNAMIQUE') then
        call getvr8('CALC_'//typevp, typevp, iocc=1, nbval=0, nbret=nfreqr)
    else
        call getvr8('CALC_'//typevp, typevp, iocc=1, nbval=0, nbret=ncritr)
    endif
    na1=0
    if (typres .ne. 'MODE_FLAMB') then
        call getvr8('CALC_'//typevp, 'AMOR_REDUIT', iocc=1, nbval=0, nbret=na1)
    endif
    namorr = na1
    nfreq = - nfreqr
    ncrit = - ncritr
    nbmod = max (nfreq, ncrit)
!
    if ((nfreqr .ne. 0) .and. (namorr.eq.0)) then
        nfreq = -nfreqr
        call wkvect(cborne, 'V V R', nfreq, lborne)
        call getvr8('CALC_'//typevp, typevp, iocc=1, nbval=nfreq, vect=zr(lborne),&
                    nbret=l)
!         --- CONTROLE DE FREQUENCE NEGATIVE ---
        ierfr = 0
        do ifreq = 0, nfreq - 1
            if (zr(lborne+ifreq) .lt. 0.d0) ierfr = ierfr + 1
        end do
        if (ierfr .gt. 0) then
            call utmess('A', 'ALGELINE2_59', sk=typevp)
        endif
!
    endif
    if ((typres.eq.'MODE_FLAMB') .and. (namorr.eq.0)) then
        ncrit = -ncritr
        call wkvect(cborne, 'V V R', ncrit, lborne)
        call getvr8('CALC_'//typevp, typevp, iocc=1, nbval=ncrit, vect=zr(lborne),&
                    nbret=l)
    endif
!
!     --- LISTE DES AMORTISSEMENTS (CAS QUADRATIQUE) ---
    if ((nfreqr .ne. 0) .and. (namorr.ne.0)) then
        nfreq = -nfreqr
        call wkvect(cborne, 'V V R', 2*nfreq, lborne)
        call wkvect(cfreq, 'V V R', nfreq, lfreq)
        call getvr8('CALC_'//typevp, typevp, iocc=1, nbval=nfreq, vect=zr(lfreq),&
                    nbret=l)
        call wkvect(camor, 'V V R', nfreq, lamort)
        if (na1 .ne. 0) then
            call getvr8('CALC_'//typevp, 'AMOR_REDUIT', iocc=1, nbval=nfreq, vect=zr(lamort),&
                        nbret=l)
        endif
!
!         --- PASSAGE EN VALEURS PROPRES COMPLEXES ---
!         ZR(0) ZR(1)   ZR(2) ZR(3)   ZR(4), ZR(5)
!        (AM0 , FR0)   (AM1  , FR1)   (AM2,   FR2)   ETC  ETC
!
        depi = r8depi()
        do ifreq = 0, nfreq-1
            am = zr(lamort+ifreq)
            fr = zr(lfreq+ifreq)
            omeg = fr * depi
            am = -abs(am*omeg)/sqrt(1.d0-am*am)
            zr(lborne+2*ifreq) = am
            zr(lborne+2*ifreq+1) = omeg
        end do
    endif
!
!     ------------------------------------------------------------------
!     ----------- DDL : LAGRANGE, BLOQUE PAR AFFE_CHAR_CINE  -----------
!     ------------------------------------------------------------------
!
    call wkvect('&&OP0044.POSITION.DDL', 'V V I', neq*mxddl, lddl)
    call wkvect('&&OP0044.DDL.BLOQ.CINE', 'V V I', neq, lprod)
    call vpddl(raide, masse, neq, nblagr, nbcine,&
               neqact, zi(lddl), zi(lprod), ierd)
    if (ierd .ne. 0) goto 999
!
!     ==================================================================
!
!     ----------------- CALCUL DES VALEURS PROPRES ---------------------
!
!     ==================================================================
!
!     ------------------------------------------------------------------
!                     --- OPTION SEPAREE OU AJUSTEE ---
!                  --- CAS GENERALISE OU QUADRATIQUE ----
!     ------------------------------------------------------------------
!
    if (optiof .eq. 'SEPARE ' .or. optiof .eq. 'AJUSTE ') then
!
!         --- PASSAGE EN OMEGA**2 ---
        if (nfreq .ne. 0) then
            do ifreq = 0, nbmod - 1
                zr(lborne+ifreq) = omega2(zr(lborne+ifreq))
            end do
        endif
!
        omgmax=zr(lborne+nbmod-1)
        omgmin=zr(lborne)
        call vpfopr('STURMAD', typres, lmasse, lraide, ldynam,&
                    omgmin, omgmax, rbid, nfreqb, npivot,&
                    omecor, precsh, nbrss, nblagr, solveu,&
                    det, idet)
        det1=det(1)
        det2=det(2)
        idet1=idet(1)
        idet2=idet(2)
        ieme1=npivot(1)
        ieme2=npivot(2)
        zr(lborne+nbmod-1)=omgmax
        zr(lborne) =omgmin
!
!
        if (nfreqb .gt. 0) then
!
!        --- MODIFICATION EVENTUELLE DE MXFREQ
!
            if (mxfreq .eq. 0) then
                mxfreq = nfreqb
            endif
!
!           --- CREATION DU RESUFREQ ---
            mxresf = nfreqb
            call wkvect('&&OP0044.RESU_I', 'V V I', nbpari*mxresf, lresui)
            call wkvect('&&OP0044.RESU_R', 'V V R', nbparr*mxresf, lresur)
            call wkvect('&&OP0044.RESU_K', 'V V K24', nbpark*mxresf, lresuk)
!
!     --- INITIALISATION A UNDEF DE LA STRUCTURE DE DONNEES RESUF --
!
            do ieq = 1, nbparr*mxresf
                zr(lresur+ieq-1) = undf
            end do
            do ieq = 1, nbpari*mxresf
                zi(lresui+ieq-1) = indf
            end do
!
            ndim = 2*nfreqb + nfreq
            call wkvect(work(1), ' V V R ', ndim, jvalp)
            call wkvect(work(2), ' V V R ', ndim, jdet)
            call wkvect(work(3), ' V V I ', ndim, jidet)
            call wkvect(work(4), ' V V I ', ndim, jieme)
            call wkvect(work(5), ' V V I ', ndim, jnpas)
!
            zi(jidet) = idet1
            zr(jdet ) = det1
            zi(jieme) = ieme1-nblagr
            zi(jidet+nbmod-1) = idet2
            zr(jdet +nbmod-1) = det2
            zi(jieme+nbmod-1) = ieme2-nblagr
            if (typres .ne. 'DYNAMIQUE') then
                if (zr(lborne) .lt. 0.d0) then
                    zi(jieme) = - zi(jieme)
                endif
                if (zr(lborne+nbmod-1) .lt. 0.d0) then
                    zi(jieme+nbmod-1) = - zi(jieme+nbmod-1)
                endif
            endif
!
            do ifreq = 0, nbmod-1
                zr(jvalp+ifreq) = zr(lborne+ifreq)
            end do
!
!           --- CALCUL DES FREQUENCES PAR DICHOTOMIE
            call vpdich(lraide, lmasse, ldynam, tolsep, nitsep,&
                        mxfreq, nbmod, zr(jvalp), zi(jieme), zr(jdet),&
                        zi(jidet), zi(jnpas), typres, nblagr, solveu)
!
!                  --- AJUSTEMENT DES VALEURS PROPRES ---
!           --- PRISE EN COMPTE DES VALEURS PROPRES MULTIPLES ---
            call vpinte(optiof, nbmod, zr(jvalp), zr(jdet), zi(jidet),&
                        zi(jieme), zi(jnpas), tolaju, nitaju, lraide,&
                        lmasse, ldynam, zi(lresui), zr(lresur), mxresf,&
                        solveu)
!
!
        else
            call utmess('F', 'ALGELINE2_62')
        endif
!
!        --- CAS QUADRATIQUE ---
!
        if (lamor .ne. 0) then
            do ifreq = 0, nbmod - 1
                zr(lresur+mxresf+ifreq) = sqrt(zr(lresur+mxresf+ifreq) )
                am = 0.02d0
                omeg = zr(lresur+mxresf+ifreq)
                zr(lresur+2*mxresf+ifreq)=-abs(am*omeg)/sqrt(1.d0-am*&
                am)
                zr(lresur+3*mxresf+ifreq) = 0.0d0
                zr(lresur+4*mxresf+ifreq) = 0.0d0
                zr(lresur+5*mxresf+ifreq) = 0.0d0
                zr(lresur+6*mxresf+ifreq) = 0.0d0
            end do
!
        endif
!
!        --- CAS QUADRATIQUE : OPTION AJUSTE ---
!
        if ((lamor.ne.0) .and. optior .eq. 'AJUSTE') then
            call wkvect('&&OP0044.VP.MULLER', 'V V C', 3*mxresf, lvalp)
            kfreq = 0
            do ifreq = 0, nbmod-1
                omeg = zr(lresur+mxresf+ifreq)
                do i = 1, 3
                    kfreq = kfreq + 1
                    rbid = omeg+zfr(i)
                    am = -abs(zam(i)*rbid)/sqrt(1.d0-zam(i)*zam(i))
                    zc(lvalp+kfreq-1) = dcmplx(am,omeg)
                end do
            end do
!
            call wp1mul(lmasse, lamor, lraide, zc(lvalp), tolaju,&
                        nitaju, nbmod, mxresf, nbmod, zi(lresui),&
                        zr(lresur))
        endif
!
        if (mxfreq .ne. 0) then
            nbmod = min ( mxfreq , nbmod )
        endif
!
!     ------------------------------------------------------------------
!                          --- OPTION PROCHE ---
!                         --- CAS GENERALISE ----
!     ------------------------------------------------------------------
!
    else if (lamor.eq.0 .and. optiof.eq.'PROCHE  ') then
!
        mxresf = nbmod
        jvalp = lborne
        call wkvect('&&OP0044.RESU_I', 'V V I', nbpari*mxresf, lresui)
        call wkvect('&&OP0044.RESU_R', 'V V R', nbparr*mxresf, lresur)
        call wkvect('&&OP0044.RESU_K', 'V V K24', nbpark*mxresf, lresuk)
!
!     --- INITIALISATION A UNDEF DE LA STRUCTURE DE DONNEES RESUF --
!
        do ieq = 1, nbparr*mxresf
            zr(lresur+ieq-1) = undf
        end do
        do ieq = 1, nbpari*mxresf
            zi(lresui+ieq-1) = indf
        end do
!
        call wkvect('&&OP0044.POSITION', 'V V I', nbmod, jieme)
!
!        --- REMPLISSAGE DU RESUFREQ ET PASSAGE EN OMEGA**2 ---
        do ifreq = 0, nbmod-1
            zi(jieme+ifreq) = ifreq+1
            zi(lresui+ifreq) = 0
            zr(lresur+ifreq) = zr(lborne+ifreq)
            zr(lresur+2*mxresf+ifreq) = 0.0d0
        end do
        if (nfreq .ne. 0) then
            do ifreq = 0, nbmod -1
                zr(lresur+mxresf+ifreq) = omega2(zr(lborne+ifreq))
            end do
        else
            do ifreq = 0, nbmod -1
                zr(lresur+mxresf+ifreq) = zr(lborne+ifreq)
            end do
        endif
!
!     ------------------------------------------------------------------
!                          --- OPTION PROCHE ---
!                         --- CAS QUADRATIQUE ----
!     ------------------------------------------------------------------
!
    else if (lamor.ne.0 .and. optiof.eq.'PROCHE  ') then
!
        mxresf = nfreq
        call wkvect('&&OP0044.RESU_I', 'V V I', nbpari*mxresf, lresui)
        call wkvect('&&OP0044.RESU_R', 'V V R', nbparr*mxresf, lresur)
        call wkvect('&&OP0044.RESU_K', 'V V K24', nbpark*mxresf, lresuk)
!
!     --- INITIALISATION A UNDEF DE LA STRUCTURE DE DONNEES RESUF --
!
        do ieq = 1, nbparr*mxresf
            zr(lresur+ieq-1) = undf
        end do
        do ieq = 1, nbpari*mxresf
            zi(lresui+ieq-1) = indf
        end do
!
        depi = r8depi()
        k = -1
        do ifreq = 0, nfreq-1
            zi(lresui+ifreq) = ifreq+1
            k = k + 1
            zr(lresur+ifreq) = zr(lborne+k)
            if (namorr .ne. 0) then
                k = k + 1
                zr(lresur+2*mxresf+ifreq) = zr(lborne+k)
            else
                omeg = depi * zr(lborne+k)
                am = 0.02d0
                zr(lresur+mxresf+ifreq) = omeg
                raux1=-abs(am*omeg)
                raux2=sqrt(1.d0-am*am)
                zr(lresur+2*mxresf+ifreq)=raux1/raux2
            endif
        end do
        nblagr = 0
        nbmod = nfreq
!
    else
!        --- ERREUR ---
        ASSERT(.false.)
    endif
!
!     ------------------------------------------------------------------
!         --- CALCUL DES VECTEURS PROPRES PAR ITERATION INVERSE ---
!     ------------------------------------------------------------------
!
    if (lamor .eq. 0) then
!
!        --- CAS GENERALISE
!
        call wkvect('&&OP0044.VECTEUR.PROPRE', 'V V R', neq*nbmod, lvec)
        call vp1pro(optiom, lraide, lmasse, ldynam, neq,&
                    nbmod, mxresf, tolv, nitv, zi(lprod),&
                    omecor, zr(lvec), zi(lresui), zr(lresur), zk24(lresuk),&
                    nbrss, nbpari, nbparr, nbpark, typres,&
                    optiof, solveu)
!
    else
!
!        --- CAS QUADRATIQUE
!
        call wkvect('&&OP0044.VECTEUR.PROPRE', 'V V C', neq*nbmod, lvec)
        call wp1inv(lmasse, lamor, lraide, tolv, nitv,&
                    mxresf, nbmod, neq, zi(lresui), zr(lresur),&
                    zk24(lresuk), zc(lvec), solveu)
    endif
!
!     ------------------------------------------------------------------
!     ------------------------- POSITION DES MODES ---------------------
!     ------------------------------------------------------------------
!
    if ((typres.eq.'DYNAMIQUE') .and. (optiof.ne.'PROCHE') .and. (lamor.eq.0)) then
!
        do ifreq = 0, nbmod-1
            zi(lresui+ifreq) = zi(lresui+ifreq) - nblagr
        end do
!
!
    endif
    if (zi(lresui) .eq. 0) zi(lresui) = 1
!
!     ------------------------------------------------------------------
!     -------------- CALCUL DES PARAMETRES GENERALISES  ----------------
!     ----------- CALCUL DE LA NORME D'ERREUR SUR LE MODE  -------------
!     ---------------- STOCKAGE DES VECTEURS PROPRES  ------------------
!     ------------------------------------------------------------------
!
!     POSITION MODALE NEGATIVE DES MODES INTERDITE
    knega = 'NON'
!
    nparr = nbparr
    if (typcon .eq. 'MODE_ACOU') nparr = 7
!
    if (lamor .eq. 0) then
        call vppara(modes, typcon, knega, lraide, lmasse,&
                    lamor, mxresf, neq, nbmod, omecor,&
                    zi(lddl), zi(lprod), zr(lvec), [cbid], nbpari,&
                    nparr, nbpark, nopara, '    ', zi(lresui),&
                    zr(lresur), zk24(lresuk), ktyp, .false., ibid,&
                    ibid, k16bid, ibid)
    else
        call vppara(modes, typcon, knega, lraide, lmasse,&
                    lamor, mxresf, neq, nbmod, omecor,&
                    zi(lddl), zi(lprod), [rbid], zc(lvec), nbpari,&
                    nparr, nbpark, nopara, '    ', zi(lresui),&
                    zr(lresur), zk24(lresuk), ktyp, .false., ibid,&
                    ibid, k16bid, ibid)
    endif
!
!     --- IMPRESSION PROPRE A LA METHODE ----
    call vpwecf(optiof, typres, nbmod, mxresf, zi(lresui),&
                zr(lresur), zk24(lresuk), lamor, ktyp, lbid)
!
    call titre()
!
!     ------------------------------------------------------------------
!     ----------- CONTROLE DE VALIDITE DES MODES CALCULES  -------------
!     ------------------------------------------------------------------
!
    call getvtx('VERI_MODE', 'STOP_ERREUR', iocc=1, scal=optiov, nbret=lmf)
    if (optiov .eq. 'OUI') then
        ctyp = 'E'
    else
        ctyp = 'A'
    endif
    optiov = ' '
!
    call getvr8('VERI_MODE', 'SEUIL', iocc=1, scal=seuil, nbret=lmf)
    lmat(1) = lraide
    lmat(2) = lmasse
    lmat(3) = 0
!
    call vpcntl(ctyp, modes, optiov, fmin, fmax,&
                seuil, nbmod, zi(lresui), lmat, omecor,&
                rbid, ierx, fmin, fmax, zr(lresur),&
                zr(lresur+3*mxresf), zr(lresur+mxresf), typres, nblagr, solveu,&
                nbrss, precsh)
!
!     ------------------------------------------------------------------
!
!
!     ------------------------------------------------------------------
999 continue
!
!     --- DESTRUCTION DE LA MATRICE DYNAMIQUE
    call detrsd('MATR_ASSE', dynam)
    call jedema()
!
end subroutine
