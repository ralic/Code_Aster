subroutine cakg3d(option, result, modele, depla, thetai,&
                  mate, compor, lischa, symech, chfond,&
                  nnoff, basloc, courb, iord, ndeg,&
                  thlagr, glagr, thlag2, pair, ndimte,&
                  extim, time, nbprup, noprup, fiss,&
                  lmelas, nomcas, lmoda, puls, milieu,&
                  connex)
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcharg.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/gkmet1.h"
#include "asterfort/gkmet3.h"
#include "asterfort/gkmet4.h"
#include "asterfort/gksimp.h"
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
    real(kind=8) :: puls
    character(len=8) :: modele, thetai, fiss
    character(len=8) :: result, symech
    character(len=16) :: option, noprup(*), nomcas
    character(len=19) :: lischa
    character(len=24) :: depla, chfond, mate, compor, basloc, courb, chpuls
    logical :: extim, thlagr, glagr, thlag2, pair, lmelas, lmoda, milieu, connex
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!  FONCTION REALISEE:   CALCUL DU TAUX DE RESTITUTION LOCAL D'ENERGIE ET
!                       DES FACTEURS D'INTENSITE DE CONTRAINTES EN 3D
!
!  IN    OPTION --> CALC_K_G
!  IN    RESULT --> NOM UTILISATEUR DU RESULTAT ET TABLE
!  IN    MODELE --> NOM DU MODELE
!  IN    DEPLA  --> CHAMP DE DEPLACEMENT
!  IN    THETAI --> BASE DE I CHAMPS THETA
!  IN    MATE   --> CHAMP DE MATERIAUX
!  IN    COMPOR --> COMPORTEMENT
!  IN    SYMECH --> SYMETRIE DU CHARGEMENT
!  IN    CHFOND --> POINTS DU FOND DE FISSURE
!  IN    NNOFF  --> NOMBRE DE POINTS DU FOND DE FISSURE
!  IN    BASLOC --> BASE LOCALE
!  IN    COURB  --> NOM DU TENSEUR DE COURBURE
!  IN    IORD   --> NUMERO D'ORDRE DE LA SD
!  IN    NDEG   --> DEGRE DU POLYNOME DE LEGENDRE
!  IN    THLAGR --> VRAI SI LISSAGE THETA EST LAGRANGE OU LAGRANGE_NO_NO
!  IN    THLAG2 --> VRAI SI LISSAGE THETA EST LAGRANGE_REGU
!  IN    GLAGR  --> VRAI SI LISSAGE G EST LAGRANGE
!  IN    TIME   --> INSTANT DE CALCUL
!  IN    FISS   --> NOM DE LA SD FISS_XFEM
!  IN    LMELAS --> TRUE SI LE TYPE DE LA SD RESULTAT EST MULT_ELAS
!  IN    NOMCAS --> NOM DU CAS DE CHARGE SI LMELAS
!  IN    MILIEU --> .TRUE.  : ELEMENT QUADRATIQUE
!                   .FALSE. : ELEMENT LINEAIRE
!  IN    CONNEX --> .TRUE.  : SI FOND FERME
!                   .FALSE. : SI FOND OUVERT
!
!
    integer :: nbmxpa
    parameter (nbmxpa = 20)
!
    integer :: nbinmx, nboumx, numfon
    parameter   (nbinmx=50,nboumx=1)
    character(len=8) :: lpain(nbinmx), lpaout(nboumx)
    character(len=24) :: lchin(nbinmx), lchout(nboumx)
!
    integer :: i, j, ibid, iadrgk, iadgks, iret, jresu, nchin
    integer :: nnoff, num, incr, nres
    integer :: ndeg, ierd, init, livi(nbmxpa)
    integer :: iadgki, iadabs, ifm, niv
    real(kind=8) :: gkthi(8), time, livr(nbmxpa)
    complex(kind=8) :: cbid, livc(nbmxpa)
    logical :: lfonc
    character(len=2) :: codret
    character(len=8) :: resu
    character(len=16) :: opti, valk
    character(len=19) :: chrota, chpesa, chvolu, ch1d2d, chepsi, ch2d3d, chpres
    character(len=19) :: chvarc, chvref
    character(len=24) :: ligrmo, chgeom, chgthi
    character(len=24) :: chsigi
    character(len=24) :: chthet, chtime
    character(len=24) :: abscur, pavolu, papres, pa2d3d
    character(len=24) :: chsig, chepsp, chvari, type, pepsin, livk(nbmxpa)
    character(len=19) :: pintto, cnseto, heavto, loncha, lnno, ltno
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     ------------------------------------------------------------------
!     1) INITIALISATIONS
!     ------------------------------------------------------------------
!
    call infniv(ifm, niv)
!
!     RECUPERATION DU CHAMP GEOMETRIQUE
    call megeom(modele, chgeom)
!
    chvarc='&&CAKG3D.VARC'
    chvref='&&CAKG3D.VARC.REF'
!
!     RECUPERATION DU COMPORTEMENT
    call getfac('COMP_INCR', incr)
    if (incr .ne. 0) then
        call getvid(' ', 'RESULTAT', scal=resu, nbret=nres)
        call dismoi('F', 'TYPE_RESU', resu, 'RESULTAT', ibid,&
                    type, ierd)
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
!     RECUPERATION DE L'ETAT INITIAL (NON TRAITE DANS CETTE OPTION)
    call getvid('COMP_INCR', 'SIGM_INIT', iocc=1, scal=chsigi, nbret=init)
    if (init .ne. 0) then
        valk='CALC_K_G'
        call utmess('F', 'RUPTURE1_13', sk=valk)
    endif
!
!     RECUPERATION (S'ILS EXISTENT) DES CHAMP DE TEMPERATURES (T,TREF)
    call vrcins(modele, mate, ' ', time, chvarc,&
                codret)
    call vrcref(modele, mate(1:8), '        ', chvref(1:19))
!
!     TRAITEMENT DES CHARGES
    chvolu = '&&CAKG3D.VOLU'
    ch1d2d = '&&CAKG3D.1D2D'
    ch2d3d = '&&CAKG3D.2D3D'
    chpres = '&&CAKG3D.PRES'
    chepsi = '&&CAKG3D.EPSI'
    chpesa = '&&CAKG3D.PESA'
    chrota = '&&CAKG3D.ROTA'
    call gcharg(modele, lischa, chvolu, ch1d2d, ch2d3d,&
                chpres, chepsi, chpesa, chrota, lfonc,&
                time, iord)
    if (lfonc) then
        pavolu = 'PFFVOLU'
        pa2d3d = 'PFF2D3D'
        papres = 'PPRESSF'
        pepsin = 'PEPSINF'
        opti = 'CALC_K_G_F'
    else
        pavolu = 'PFRVOLU'
        pa2d3d = 'PFR2D3D'
        papres = 'PPRESSR'
        pepsin = 'PEPSINR'
        opti = 'CALC_K_G'
    endif
!
!     RECUPERATION DES DONNEES XFEM OU FEM (TOPOSE)
    pintto = modele(1:8)//'.TOPOSE.PIN'
    cnseto = modele(1:8)//'.TOPOSE.CNS'
    heavto = modele(1:8)//'.TOPOSE.HEA'
    loncha = modele(1:8)//'.TOPOSE.LON'
!     ON NE PREND PAS LES LSN ET LST DU MODELE
!     CAR LES CHAMPS DU MODELE SONT DEFINIS QUE AUTOUR DE LA FISSURE
!     OR ON A BESOIN DE LSN ET LST MEME POUR LES
    lnno = fiss//'.LNNO'
    ltno = fiss//'.LTNO'
!
!
!     ------------------------------------------------------------------
!     2) CALCUL DES K(THETA_I) AVEC I=1,NDIMTE
!     ------------------------------------------------------------------
!
!     NDIMTE = NNOFF  SI TH-LAGRANGE
!     NDIMTE = NDEG+1 SI TH-LEGENDRE
!
!     pourquoi modifier NDIMTE (argument d'entree)
    if (thlag2) then
        ndimte = ndimte
    else if (thlagr) then
        ndimte = nnoff
    else
        ndimte = ndeg + 1
    endif
!
    call wkvect('&&CAKG3D.VALG', 'V V R8', ndimte*8, iadrgk)
    call jeveuo(thetai, 'L', jresu)
!
!
!
!     BOUCLE SUR LES DIFFERENTS CHAMPS THETA
    do 20 i = 1, ndimte
!
        chthet = zk24(jresu+i-1)
        call codent(i, 'G', chgthi)
!
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
        lpain(8) = 'PPESANR'
        lchin(8) = chpesa
        lpain(9) = 'PROTATR'
        lchin(9) = chrota
        lpain(10) = pepsin(1:8)
        lchin(10) = chepsi
        lpain(11) = 'PCOMPOR'
        lchin(11) = compor
        lpain(12) = 'PBASLOR'
        lchin(12) = basloc
        lpain(13) = 'PCOURB'
        lchin(13) = courb
        lpain(14) = 'PPINTTO'
        lchin(14) = pintto
        lpain(15) = 'PCNSETO'
        lchin(15) = cnseto
        lpain(16) = 'PHEAVTO'
        lchin(16) = heavto
        lpain(17) = 'PLONCHA'
        lchin(17) = loncha
        lpain(18) = pa2d3d(1:8)
        lchin(18) = ch2d3d
        lpain(19) = papres(1:8)
        lchin(19) = chpres
        lpain(20) = 'PLSN'
        lchin(20) = lnno
        lpain(21) = 'PLST'
        lchin(21) = ltno
        if (option .eq. 'CALC_K_G' .or. option .eq. 'CALC_K_G_F') then
            lpain(22) = 'PPINTER'
!          LCHIN(22) = MODELE(1:8)//'.TOPOFAC.PI'
            lchin(22) = modele(1:8)//'.TOPOFAC.OE'
            lpain(23) = 'PAINTER'
            lchin(23) = modele(1:8)//'.TOPOFAC.AI'
            lpain(24) = 'PCFACE'
            lchin(24) = modele(1:8)//'.TOPOFAC.CF'
            lpain(25) = 'PLONGCO'
            lchin(25) = modele(1:8)//'.TOPOFAC.LO'
            lpain(26) = 'PBASECO'
            lchin(26) = modele(1:8)//'.TOPOFAC.BA'
        endif
!
        nchin = 26
!
        ligrmo = modele//'.MODELE'
!
        chtime = '&&CAKG3D.CH_INST_R'
        if (opti .eq. 'CALC_K_G_F') then
            call mecact('V', chtime, 'MODELE', ligrmo, 'INST_R  ',&
                        ncmp=1, nomcmp='INST   ', sr=time)
            nchin = nchin + 1
            lpain(nchin) = 'PTEMPSR'
            lchin(nchin) = chtime
        endif
!
        if (lmoda) then
            chpuls = '&&CAKG3D.PULS'
            call mecact('V', chpuls, 'MODELE', ligrmo, 'FREQ_R  ',&
                        ncmp=1, nomcmp='FREQ   ',sr=puls)
            nchin = nchin + 1
            lpain(nchin) = 'PPULPRO'
            lchin(nchin) = chpuls
        endif
!
        ASSERT(nchin.le.nbinmx)
        call calcul('S', opti, ligrmo, nchin, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
!
!       FAIRE LA "SOMME" D'UN CHAM_ELEM
        call mesomm(chgthi, 8, ibid, gkthi, cbid,&
                    0, ibid)
!
!       SYMETRIE DU CHARGEMENT
        if (symech .eq. 'NON') then
            do 29 j = 1, 7
                zr(iadrgk-1+(i-1)*8+j) = gkthi(j)
29          continue
        else if (symech.eq.'OUI') then
!         G, fic1, fic2, fic3, K1, K2, K3,
            zr(iadrgk-1+(i-1)*8+1) = 2.d0*gkthi(1)
            zr(iadrgk-1+(i-1)*8+2) = 2.d0*gkthi(2)
            zr(iadrgk-1+(i-1)*8+3) = 0.d0
            zr(iadrgk-1+(i-1)*8+4) = 0.d0
            zr(iadrgk-1+(i-1)*8+5) = 2.d0*gkthi(5)
            zr(iadrgk-1+(i-1)*8+6) = 0.d0
            zr(iadrgk-1+(i-1)*8+7) = 0.d0
        endif
!
20  continue
!
!     ------------------------------------------------------------------
!     3) CALCUL DE G(S), K1(S), K2(S) ET K3(S) LE LONG DU FOND
!     ------------------------------------------------------------------
!
    call wkvect('&&CAKG3D.VALGK_S', 'V V R8', nnoff*6, iadgks)
!
    if (glagr .or. thlag2) then
        if (glagr) then
            call wkvect('&&CAKG3D.VALGKI', 'V V R8', nnoff*5, iadgki)
        else if (thlag2) then
            call wkvect('&&CAKG3D.VALGKI', 'V V R8', ndimte*5, iadgki)
        endif
    else
        call wkvect('&&CAKG3D.VALGKI', 'V V R8', (ndeg+1)*5, iadgki)
    endif
!
    abscur='&&CAKG3D.ABSCU'
    call wkvect(abscur, 'V V R', nnoff, iadabs)
!
!     PREMIERE METHODE : G_LEGENDRE ET THETA_LEGENDRE
!     DEUXIEME METHODE : G_LEGENDRE ET THETA_LAGRANGE
!     TROISIEME METHODE: G_LAGRANGE ET THETA_LAGRANGE
!                       (OU G_LAGRANGE_NO_NO ET THETA_LAGRANGE)
!
    if (thlag2) then
        num = 5
        call gkmet4(nnoff, ndimte, chfond, pair, iadrgk,&
                    milieu, connex, iadgks, iadgki, abscur,&
                    num)
    else if ((.not.glagr) .and. (.not.thlagr)) then
        num = 1
        call gkmet1(ndeg, nnoff, chfond, iadrgk, iadgks,&
                    iadgki, abscur)
!
    else if (thlagr) then
        if (.not.glagr) then
            num = 2
            call utmess('F', 'RUPTURE1_17')
        else
            num = 3
            call gkmet3(nnoff, chfond, iadrgk, milieu, connex,&
                        iadgks, iadgki, abscur, num, modele)
        endif
    endif
!
!     IMPRESSION DE G(S), K1(S), K2(S) ET K3(S)
    if (niv .ge. 2) then
        call gksimp(result, nnoff, zr(iadabs), iadrgk, num,&
                    iadgks, ndeg, ndimte, iadgki, extim,&
                    time, iord, ifm)
    endif
!
!     ECRITURE DE LA TABLE DE G(S), K1(S), K2(S) ET K3(S)
    call getvis('THETA', 'NUME_FOND', iocc=1, scal=numfon, nbret=ibid)
!
    if (lmelas) then
        call tbajvi(result, nbprup, 'NUME_CAS', iord, livi)
        call tbajvk(result, nbprup, 'NOM_CAS', nomcas, livk)
        call tbajvi(result, nbprup, 'NUME_FOND', numfon, livi)
    else if (lmoda) then
        call tbajvi(result, nbprup, 'NUME_MODE', iord, livi)
    else
        call tbajvi(result, nbprup, 'NUME_ORDRE', iord, livi)
        call tbajvr(result, nbprup, 'INST', time, livr)
        call tbajvi(result, nbprup, 'NUME_FOND', numfon, livi)
    endif
!
    do 40 i = 1, nnoff
        call tbajvi(result, nbprup, 'NUM_PT', i, livi)
        call tbajvr(result, nbprup, 'ABSC_CURV', zr(iadabs-1+i), livr)
        call tbajvr(result, nbprup, 'K1', zr(iadgks-1+6*(i-1)+2), livr)
        call tbajvr(result, nbprup, 'K2', zr(iadgks-1+6*(i-1)+3), livr)
        call tbajvr(result, nbprup, 'K3', zr(iadgks-1+6*(i-1)+4), livr)
        call tbajvr(result, nbprup, 'G', zr(iadgks-1+6*(i-1)+1), livr)
        call tbajvr(result, nbprup, 'BETA', zr(iadgks-1+6*(i-1)+6), livr)
        call tbajvr(result, nbprup, 'G_IRWIN', zr(iadgks-1+6*(i-1)+5), livr)
        call tbajli(result, nbprup, noprup, livi, livr,&
                    livc, livk, 0)
40  continue
!
!- DESTRUCTION D'OBJETS DE TRAVAIL
!
    call jedetr(abscur)
    call jedetr('&&CAKG3D.VALGK_S')
    call jedetr('&&CAKG3D.VALGKI')
    call detrsd('CHAMP_GD', chvarc)
    call detrsd('CHAMP_GD', chvref)
    call detrsd('CHAMP_GD', chvolu)
    call detrsd('CHAMP_GD', ch1d2d)
    call detrsd('CHAMP_GD', ch2d3d)
    call detrsd('CHAMP_GD', chpres)
    call detrsd('CHAMP_GD', chepsi)
    call detrsd('CHAMP_GD', chpesa)
    call detrsd('CHAMP_GD', chrota)
!
    call jedetr('&&CAKG3D.VALG')
!
    call jedema()
end subroutine
