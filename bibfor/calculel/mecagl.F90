subroutine mecagl(option, result, modele, depla, thetai,&
                  mate, compor, lischa, symech, chfond,&
                  nnoff, iord, ndeg, thlagr, glagr,&
                  thlag2, milieu, ndimte, pair, extim,&
                  time, nbprup, noprup, chvite, chacce,&
                  lmelas, nomcas, kcalc, fonoeu, lincr)
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/alchml.h"
#include "asterfort/calcul.h"
#include "asterfort/chpchd.h"
#include "asterfort/chpver.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcharg.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/gimpgs.h"
#include "asterfort/gmeth1.h"
#include "asterfort/gmeth2.h"
#include "asterfort/gmeth3.h"
#include "asterfort/gmeth4.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/megeom.h"
#include "asterfort/mesomm.h"
#include "asterfort/rsexch.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajvi.h"
#include "asterfort/tbajvk.h"
#include "asterfort/tbajvr.h"
#include "asterfort/utmess.h"
#include "asterfort/vrcins.h"
#include "asterfort/vrcref.h"
#include "asterfort/wkvect.h"
!
    integer :: iord, nbprup, ndimte
!
    real(kind=8) :: time
!
    character(len=19) :: lischa
    character(len=8) :: modele, thetai
    character(len=8) :: result, symech, kcalc
    character(len=16) :: option, noprup(*), nomcas
    character(len=24) :: depla, chfond, mate, compor
    character(len=24) :: chvite, chacce, fonoeu
!
    logical :: extim, thlagr, glagr, milieu, pair, thlag2, lmelas, lincr
! ......................................................................
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
!  - FONCTION REALISEE:   CALCUL DU TAUX DE RESTITUTION LOCAL D'ENERGIE
!
!  IN    OPTION --> CALC_G OU G_LAGR (SI CHARGES REELLES)
!               --> CALC_G_F OU G_LAGR_F (SI CHARGES FONCTIONS)
!  IN    RESULT --> NOM UTILISATEUR DU RESULTAT ET TABLE
!  IN    MODELE --> NOM DU MODELE
!  IN    DEPLA  --> CHAMP DE DEPLACEMENT
!  IN    THETAI --> BASE DE I CHAMPS THETA
!  IN    MATE   --> CHAMP DE MATERIAUX
!  IN    COMPOR --> COMPORTEMENT
!  IN    NCHAR  --> NOMBRE DE CHARGES
!  IN    LCHAR  --> LISTE DES CHARGES
!  IN    SYMECH --> SYMETRIE DU CHARGEMENT
!  IN    CHFOND --> VECTEUR CONTENANT LES ABSCISSES CURVILIGNES DES
!                   NOEUDS DU FOND DE FISSURE
!  IN    NNOFF  --> NOMBRE DE NOEUDS DU FOND DE FISSURE
!  IN    TIME   --> INSTANT DE CALCUL
!  IN    IORD   --> NUMERO D'ORDRE DE LA SD
!  IN    THLAGR --> VRAI SI LISSAGE THETA_LAGRANGE
!  IN    THLAG2 --> VRAI SI LISSAGE THETA_LAGRANGE_REGU
!  IN    GLAGR  --> VRAI SI LISSAGE G_LAGRANGE
!  IN    NDEG   --> DEGRE DU POLYNOME DE LEGENDRE
!  IN    LMELAS --> TRUE SI LE TYPE DE LA SD RESULTAT EST MULT_ELAS
!  IN    NOMCAS --> NOM DU CAS DE CHARGE SI LMELAS
!  IN    KCALC  --> = 'NON' : ON RECUPERE LES CHAMPS DE CONTRAINTES
!                             ET D'ENERGIE DE LA SD RESULTAT
!                   = 'OUI' : ON RECALCULE LES CHAMPS DE CONTRAINTES
!                             ET D'ENERGIE
!  IN    FONOEU --> NOM DES NOEUDS DE FOND DE FISSURE
! ......................................................................
!
    integer :: nbmxpa
    parameter (nbmxpa = 20)
!
    integer :: i, ibid, iadrg, iadrgs, iret, jresu, nchin
    integer :: nnoff, num, incr, nres, nsig, ino1, ino2, inga
    integer :: ndeg, livi(nbmxpa), numfon
    integer :: iadrno, iadgi, iadabs, ifm, niv, ifon
    real(kind=8) :: gthi(1), livr(nbmxpa), xl
    complex(kind=8) :: livc(nbmxpa)
    logical :: fonc, lxfem
    character(len=2) :: codret
    character(len=8) :: resu, fiss
    character(len=8) :: lpain(30), lpaout(1)
    character(len=16) :: opti
    character(len=19) :: chrota, chpesa, cf2d3d, chpres, chvolu, cf1d2d, chepsi
    character(len=19) :: chvarc, chvref
    character(len=19) :: basloc, pintto, cnseto, heavto, loncha, lnno, ltno
    character(len=19) :: pmilto
    character(len=19) :: longco, pinter, ainter, cface, baseco
    character(len=24) :: ligrmo, chgeom, chgthi
    character(len=24) :: chsigi, sigout, celmod
    character(len=24) :: lchin(40), lchout(1), chthet, chtime
    character(len=24) :: objcur, normff, pavolu, papres, pa2d3d
    character(len=24) :: chsig, chepsp, chvari, type, pepsin, livk(nbmxpa)
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call infniv(ifm, niv)
!
    chvarc = '&&MECAGL.VARC'
    chvref = '&&MECAGL.VARC.REF'
    chsigi = '&&MECALG.CHSIGI'
    celmod = '&&MECALG.CELMOD'
    sigout = '&&MECALG.SIGOUT'
!- RECUPERATION DU CHAMP GEOMETRIQUE
!
    call megeom(modele, chgeom)
!
    call getvid('THETA', 'FISSURE', iocc=1, scal=fiss, nbret=ibid)
    lxfem = .false.
    if (ibid .ne. 0) lxfem = .true.
!
!- RECUPERATION DU COMPORTEMENT
!
!    call getfac('COMPORTEMENT', incr)
!
    incr=0
    if (lincr) incr=1
!     if (incr .ne. 0 .and. lxfem) then
!         call utmess('F', 'RUPTURE1_43')
!     endif
!
    if (incr .ne. 0) then
        call getvid(' ', 'RESULTAT', scal=resu, nbret=nres)
        call dismoi('TYPE_RESU', resu, 'RESULTAT', repk=type)
        if (type .ne. 'EVOL_NOLI') then
            call utmess('F', 'RUPTURE1_15')
        endif
        call rsexch('F', resu, 'SIEF_ELGA', iord, chsig,&
                    iret)
        call rsexch('F', resu, 'EPSP_ELNO', iord, chepsp,&
                    iret)
        call rsexch('F', resu, 'VARI_ELNO', iord, chvari,&
                    iret)
    endif
!
!- RECUPERATION DE L'ETAT INITIAL
    if (incr .ne. 0) then
        call getvid('COMPORTEMENT', 'SIGM_INIT', iocc=1, scal=chsigi, nbret=nsig)
!- VERIFICATION DU TYPE DE CHAMP + TRANSFO, SI NECESSAIRE, EN CHAMP ELNO
        if (nsig .ne. 0) then
            call chpver('C', chsigi, 'ELNO', 'SIEF_R', ino1)
            call chpver('C', chsigi, 'NOEU', 'SIEF_R', ino2)
            call chpver('C', chsigi, 'ELGA', 'SIEF_R', inga)
            if ((ino1.eq.1) .and. (ino2.eq.1) .and. (inga.eq.1)) then
                call utmess('F', 'RUPTURE1_12')
            else if (inga.eq.0) then
                ligrmo = modele//'.MODELE'
                call detrsd('CHAMP', celmod)
                call alchml(ligrmo, 'CALC_G', 'PSIGINR', 'V', celmod,&
                            iret, ' ')
                call chpchd(chsigi, 'ELNO', celmod, 'NON', 'V',&
                            sigout)
                call chpver('C', sigout, 'ELNO', 'SIEF_R', ino1)
            endif
        endif
    else
        nsig=0
    endif
!
!- RECUPERATION (S'ILS EXISTENT) DES CHAMP DE TEMPERATURES (T,TREF)
    call vrcins(modele, mate, ' ', time, chvarc,&
                codret)
    call vrcref(modele, mate(1:8), '        ', chvref(1:19))
!
! - TRAITEMENT DES CHARGES
!
    chvolu = '&&MECAGL.VOLU'
    cf1d2d = '&&MECAGL.1D2D'
    cf2d3d = '&&MECAGL.2D3D'
    chpres = '&&MECAGL.PRES'
    chepsi = '&&MECAGL.EPSI'
    chpesa = '&&MECAGL.PESA'
    chrota = '&&MECAGL.ROTA'
    call gcharg(modele, lischa, chvolu, cf1d2d, cf2d3d,&
                chpres, chepsi, chpesa, chrota, fonc,&
                time, iord)
    if (fonc) then
        pavolu = 'PFFVOLU'
        pa2d3d = 'PFF2D3D'
        papres = 'PPRESSF'
        pepsin = 'PEPSINF'
        if (option .eq. 'CALC_G') then
            opti = 'CALC_G_F'
        else
            opti = 'G_LAGR_F'
        endif
    else
        pavolu = 'PFRVOLU'
        pa2d3d = 'PFR2D3D'
        papres = 'PPRESSR'
        pepsin = 'PEPSINR'
        opti=option
    endif
!
!- CALCUL DES G(THETA_I) AVEC I=1,NDIMTE  NDIMTE = NNOFF  SI TH-LAGRANGE
!                                         NDIMTE = NDEG+1 SI TH-LEGENDRE
    if (thlag2) then
        ndimte = ndimte
    else if (thlagr) then
        ndimte = nnoff
    else
        ndimte = ndeg + 1
    endif
!
    call wkvect('&&MECAGL.VALG', 'V V R8', ndimte, iadrg)
    call jeveuo(thetai, 'L', jresu)
!
! --- RECUPERATION DES DONNEES X-FEM
    if (lxfem) then
        pintto = modele//'.TOPOSE.PIN'
        cnseto = modele//'.TOPOSE.CNS'
        heavto = modele//'.TOPOSE.HEA'
        loncha = modele//'.TOPOSE.LON'
        pmilto = modele//'.TOPOSE.PMI'
!       ON NE PREND PAS LES LSN ET LST DU MODELE
!       CAR LES CHAMPS DU MODELE SONT DEFINIS QUE AUTOUR DE LA FISSURE
!       OR ON A BESOIN DE LSN ET LST MEME POUR LES
        lnno = fiss//'.LNNO'
        ltno = fiss//'.LTNO'
        basloc = fiss//'.BASLOC'
        longco = modele//'.TOPOFAC.LO'
        pinter = modele//'.TOPOFAC.OE'
        ainter = modele//'.TOPOFAC.AI'
        cface = modele//'.TOPOFAC.CF'
        baseco = modele//'.TOPOFAC.BA'
    endif
!
    do i = 1, ndimte
        chthet = zk24(jresu+i-1)
        call codent(i, 'G', chgthi)
        lpaout(1) = 'PGTHETA'
        lchout(1) = chgthi
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PDEPLAR'
        lchin(2) = depla
        lpain(3) = 'PTHETAR'
        lchin(3) = chthet
        lpain(4) = 'PMATERC'
        lchin(4) = mate
        lpain(5) = 'PVARCPR'
        lchin(5) = chvarc
        lpain(6) = 'PVARCRR'
        lchin(6) = chvref
        lpain(7) = pavolu(1:8)
        lchin(7) = chvolu
        lpain(8) = pa2d3d(1:8)
        lchin(8) = cf2d3d
        lpain(9) = papres(1:8)
        lchin(9) = chpres
        lpain(10) = 'PPESANR'
        lchin(10) = chpesa
        lpain(11) = 'PROTATR'
        lchin(11) = chrota
        lpain(12) = pepsin(1:8)
        lchin(12) = chepsi
        lpain(13) = 'PCOMPOR'
        lchin(13) = compor
!
        ligrmo = modele//'.MODELE'
        nchin = 13
!
        if (lxfem) then
            lpain(14) = 'PPINTTO'
            lchin(14) = pintto
            lpain(15) = 'PCNSETO'
            lchin(15) = cnseto
            lpain(16) = 'PHEAVTO'
            lchin(16) = heavto
            lpain(17) = 'PLONCHA'
            lchin(17) = loncha
            lpain(18) = 'PLSN'
            lchin(18) = lnno
            lpain(19) = 'PLST'
            lchin(19) = ltno
            lpain(20) = 'PBASLOR'
            lchin(20) = basloc
!
            lpain(21) = 'PLONGCO'
            lchin(21) = longco
            lpain(22) = 'PPINTER'
            lchin(22) = pinter
            lpain(23) = 'PAINTER'
            lchin(23) = ainter
            lpain(24) = 'PCFACE'
            lchin(24) = cface
            lpain(25) = 'PPMILTO'
            lchin(25) = pmilto
            lpain(26) = 'PBASECO'
            lchin(26) = baseco
!
            nchin = 26
        endif
!
        if ((opti.eq.'CALC_G_F') .or. (opti.eq.'G_LAGR_F')) then
            chtime = '&&MECAGL.CH_INST_R'
            call mecact('V', chtime, 'MODELE', ligrmo, 'INST_R',&
                        ncmp=1, nomcmp='INST', sr=time)
            lpain(nchin+1) = 'PTEMPSR'
            lchin(nchin+1) = chtime
            nchin = nchin + 1
        endif
        if (incr .ne. 0) then
            lpain(nchin+1) = 'PCONTRR'
            lchin(nchin+1) = chsig
            lpain(nchin+2) = 'PDEFOPL'
            lchin(nchin+2) = chepsp
            lpain(nchin+3) = 'PVARIPR'
            lchin(nchin+3) = chvari
            nchin = nchin + 3
!
!       CHAMP DE CONTRAINTE INITIALE
            if (nsig .ne. 0) then
                if (inga .eq. 0) then
                    lpain(nchin+1) = 'PSIGINR'
                    lchin(nchin+1)=sigout
                    nchin = nchin + 1
                    lpain(nchin+1) = 'PSIGING'
                    lchin(nchin+1)= chsigi
                    nchin = nchin + 1
                else
                    lpain(nchin+1) = 'PSIGINR'
                    lchin(nchin+1) = chsigi
                    nchin = nchin + 1
                endif
            endif
        endif
        if (option .eq. 'CALC_G' .or. option .eq. 'CALC_G_F') then
            if (chvite .ne. ' ') then
                lpain(nchin+1) = 'PVITESS'
                lchin(nchin+1) = chvite
                lpain(nchin+2) = 'PACCELE'
                lchin(nchin+2) = chacce
                nchin = nchin + 2
            endif
        endif
        if (kcalc .eq. 'NON') then
            call getvid(' ', 'RESULTAT', scal=resu, nbret=iret)
            call rsexch(' ', resu, 'SIEF_ELGA', iord, chsig,&
                        iret)
            lpain(nchin+1) = 'PCONTGR'
            lchin(nchin+1) = chsig
            nchin = nchin + 1
        endif
!
        call calcul('S', opti, ligrmo, nchin, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
        call mesomm(chgthi, 1, vr=gthi(1))
        zr(iadrg+i-1) = gthi(1)
    end do
!
!- CALCUL DE G(S) SUR LE FOND DE FISSURE PAR 4 METHODES
!- PREMIERE METHODE : G_LEGENDRE ET THETA_LEGENDRE
!- DEUXIEME METHODE : G_LEGENDRE ET THETA_LAGRANGE
!- TROISIEME METHODE: G_LAGRANGE ET THETA_LAGRANGE
!    (OU G_LAGRANGE_NO_NO ET THETA_LAGRANGE)
!- QUATRIEME METHODE: G_LAGRANGE_REGU ET THETA_LAGRANGE_REGU
!
    call wkvect('&&MECAGL.VALG_S', 'V V R8', nnoff, iadrgs)
    if (glagr .or. thlag2) then
        call wkvect('&&MECAGL.VALGI', 'V V R8', nnoff, iadgi)
    else
        call wkvect('&&MECAGL.VALGI', 'V V R8', ndeg+1, iadgi)
    endif
! ABSCISSE CURVILIGNE
    call jeveuo(chfond, 'L', ifon)
    objcur = '&&MECAGL.ABSGAMM0'
    call wkvect(objcur, 'V V R', nnoff, iadabs)
    do i = 1, nnoff
        zr(iadabs-1+(i-1)+1)=zr(ifon-1+4*(i-1)+4)
    end do
    xl=zr(iadabs-1+(nnoff-1)+1)
!
! NOM DES NOEUDS DU FOND
    if (.not.lxfem) call jeveuo(fonoeu, 'L', iadrno)
!
    if (thlag2) then
        num = 5
        call gmeth4(nnoff, ndimte, fonoeu, zr(iadrg), milieu,&
                    pair, zr(iadrgs), objcur, zr(iadgi), lxfem)
    else if ((.not.glagr) .and. (.not.thlagr)) then
        num = 1
        call gmeth1(nnoff, ndeg, zr(iadrg), zr(iadrgs), objcur,&
                    xl, zr( iadgi))
    else if (thlagr) then
        normff = zk24(jresu+nnoff+1-1)
        normff(20:24) = '.VALE'
        if (.not.glagr) then
            num = 2
            call gmeth2(modele, nnoff, ndeg, normff, fonoeu,&
                        zr(iadrg), zr(iadrgs), objcur, xl, zr(iadgi))
!
        else
            call gmeth3(nnoff, fonoeu, zr(iadrg), milieu, zr(iadrgs),&
                        objcur, zr(iadgi), num, lxfem)
        endif
    endif
!
!- SYMETRIE DU CHARGEMENT ET IMPRESSION DES RESULTATS
!
    if (symech .ne. 'NON') then
        do i = 1, nnoff
            zr(iadrgs+i-1) = 2.d0*zr(iadrgs+i-1)
        end do
    endif
!
!- IMPRESSION ET ECRITURE DANS TABLE(S) DE G(S)
!
    if (niv .ge. 2) then
        call gimpgs(result, nnoff, zr(iadabs), zr(iadrgs), num,&
                    zr(iadgi), ndeg, ndimte, zr(iadrg), extim,&
                    time, iord, ifm)
    endif
!
    call getvis('THETA', 'NUME_FOND', iocc=1, scal=numfon, nbret=ibid)
!
    if (lxfem) then
        call tbajvi(result, nbprup, 'NUME_FOND', numfon, livi)
    endif
!
    if (lmelas) then
        call tbajvi(result, nbprup, 'NUME_CAS', iord, livi)
        call tbajvk(result, nbprup, 'NOM_CAS', nomcas, livk)
    else
        call tbajvi(result, nbprup, 'NUME_ORDRE', iord, livi)
        call tbajvr(result, nbprup, 'INST', time, livr)
    endif
!
    do i = 1, nnoff
        if (lxfem) then
            call tbajvi(result, nbprup, 'NUM_PT', i, livi)
        else
            call tbajvk(result, nbprup, 'NOEUD', zk8(iadrno+i-1), livk)
        endif
        call tbajvr(result, nbprup, 'ABSC_CURV', zr(iadabs-1+i), livr)
        call tbajvr(result, nbprup, 'G', zr(iadrgs+i-1), livr)
        call tbajli(result, nbprup, noprup, livi, livr,&
                    livc, livk, 0)
    end do
!
!- DESTRUCTION D'OBJETS DE TRAVAIL
!
    call jedetr(objcur)
    call jedetr('&&MECAGL.VALG_S')
    call jedetr('&&MECAGL.VALGI')
    call detrsd('CHAMP_GD', chvarc)
    call detrsd('CHAMP_GD', chvref)
    call detrsd('CHAMP_GD', chvolu)
    call detrsd('CHAMP_GD', cf1d2d)
    call detrsd('CHAMP_GD', cf2d3d)
    call detrsd('CHAMP_GD', chpres)
    call detrsd('CHAMP_GD', chepsi)
    call detrsd('CHAMP_GD', chpesa)
    call detrsd('CHAMP_GD', chrota)
    call jedetr('&&MECAGL.VALG')
!
    call jedema()
end subroutine
