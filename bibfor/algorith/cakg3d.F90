subroutine cakg3d(option, result, modele, depla, thetai,&
                  mate, compor, lischa, symech, chfond,&
                  nnoff, basloc, courb, iord, ndeg,&
                  liss, ndimte,&
                  extim, time, nbprup, noprup, fiss,&
                  lmelas, nomcas, lmoda, puls, milieu,&
                  connex, coor, iadnoe, typdis)

    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/chpver.h"
#include "asterfort/chpchd.h"
#include "asterfort/alchml.h"
#include "asterfort/xelgano.h"
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
#include "asterfort/gksimp.h"
#include "asterfort/infniv.h"
#include "asterfort/inical.h"
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
    integer :: iord, nbprup, ndimte, coor, iadnoe
    real(kind=8) :: puls
    character(len=8) :: modele, thetai, fiss
    character(len=8) :: result, symech
    character(len=16) :: option, noprup(*), nomcas
    character(len=16), intent(in), optional :: typdis
    character(len=19) :: lischa
    character(len=24) :: depla, chfond, mate, compor, basloc, courb, chpuls, liss
    aster_logical :: extim, lmelas, lmoda, milieu, connex
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!  IN    LISS   --> TYPE DE LISSAGE
!  IN    TIME   --> INSTANT DE CALCUL
!  IN    FISS   --> NOM DE LA SD FISS_XFEM
!  IN    LMELAS --> TRUE SI LE TYPE DE LA SD RESULTAT EST MULT_ELAS
!  IN    NOMCAS --> NOM DU CAS DE CHARGE SI LMELAS
!  IN    MILIEU --> .TRUE.  : ELEMENT QUADRATIQUE
!                   .FALSE. : ELEMENT LINEAIRE
!  IN    CONNEX --> .TRUE.  : SI FOND FERME
!                   .FALSE. : SI FOND OUVERT
!  IN    COOR   --> COORDONNEES ET ABSCISSES CURVILIGNES DES NOEUDS
!                   DU FOND DE FISSURE (IADFIS DANS OP0100)
!  IN    IADNOE --> NOM DES NOEUDS DE FOND DE FISSURE
!  IN    TYPDIS --> TYPE DE DISCONTINUITE SI FISSURE XFEM 
!                   'FISSURE' OU 'COHESIF'
! ======================================================================
    integer :: nbmxpa
    parameter (nbmxpa = 20)
!
    integer :: nbinmx, nboumx, numfon
    parameter   (nbinmx=50,nboumx=1)
    character(len=8) :: lpain(nbinmx), lpaout(nboumx)
    character(len=24) :: lchin(nbinmx), lchout(nboumx)
!
    integer :: i, j, ibid, iadrgk, iadgks, iret, jresu, nchin
    integer :: nnoff, num, ino1, ino2, inga
!     integer :: incr, nres
    integer :: ndeg, nsig, livi(nbmxpa),pbtype
    integer :: iadgki, iadabs, ifm, niv
    real(kind=8) :: gkthi(8), time, livr(nbmxpa), diff2g, difrel
!    real(kind=8) :: xi(nnoff-1), yi(nnoff-1), zi(nnoff-1)
    complex(kind=8) :: livc(nbmxpa)
    aster_logical :: lfonc, lxfem
    character(len=2) :: codret
!    character(len=8) :: resu
!    character(len=24) :: chsig, chepsp, chvari, type
    character(len=16) :: opti, typdisc
    character(len=19) :: chrota, chpesa, chvolu, ch1d2d, chepsi, ch2d3d, chpres
    character(len=19) :: chvarc, chvref
    character(len=24) :: ligrmo, chgeom, chgthi
    character(len=24) :: chsigi, celmod, sigelno, sigseno
    character(len=24) :: chthet, chtime
    character(len=24) :: abscur, pavolu, papres, pa2d3d
    character(len=24) :: pepsin, livk(nbmxpa)
    character(len=19) :: pintto, cnseto, heavto, loncha, lnno, ltno, pmilto, hea_no, stano
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    codret = ' '
!
!     ------------------------------------------------------------------
!     1) INITIALISATIONS
!     ------------------------------------------------------------------
!
    call infniv(ifm, niv)
!   Securite appels
    if(present(typdis)) then
        typdisc = typdis
    else
        typdisc = 'RIEN'
    endif
!   cas FEM ou X-FEM
    call getvid('THETA', 'FISSURE', iocc=1, scal=fiss, nbret=ibid)
    lxfem = .false.
    if (ibid .ne. 0) lxfem = .true.
!
!   RECUPERATION DU CHAMP GEOMETRIQUE
    call megeom(modele, chgeom)
    

!   Recuperation du LIGREL
    ligrmo = modele//'.MODELE'    
!
    chvarc='&&CAKG3D.VARC'
    chvref='&&CAKG3D.VARC.REF'
!   Initialisation des champs
    chvolu = ' '
    chpesa = ' '
    chrota = ' '
    chepsi = ' '
    pintto = ' '
    cnseto = ' '
    heavto = ' '
    hea_no = ' '
    loncha = ' '
    pmilto = ' '
    chpres = ' '
    stano  = ' '
!
!   RECUPERATION DU COMPORTEMENT (dans cakg2d, on recupere pas incr
!    call getfac('COMPORTEMENT', incr)
!    if (incr .ne. 0) then
!        call getvid(' ', 'RESULTAT', scal=resu, nbret=nres)
!        call dismoi('TYPE_RESU', resu, 'RESULTAT', repk=type)
!        if (type .ne. 'EVOL_NOLI') then
!            call utmess('F', 'RUPTURE1_15')
!        endif
!        call rsexch('F', resu, 'SIEF_ELGA', iord, chsig,&
!                    iret)
!        call rsexch('F', resu, 'EPSP_ELNO', iord, chepsp,&
!                    iret)
!        call rsexch('F', resu, 'VARI_ELNO', iord, chvari,&
!                    iret)
!    endif
!
!
!   Recuperation de l'etat initial
!   ------------------------------

    chsigi = '&&CAKG3D.CHSIGI'
    celmod = '&&CAKG3D.CELMOD'
    sigelno= '&&CAKG3D.SIGELNO'
    sigseno= '&&CAKG3D.SIGSENO'

    call getvid('ETAT_INIT', 'SIGM', iocc=1, scal=chsigi, nbret=nsig)

!   Verification du type de champ + transfo, si necessaire en champ elno
    if (nsig .ne. 0) then

!       chpver renvoit 0 si OK et 1 si PB
        call chpver('C', chsigi(1:19), 'ELNO', 'SIEF_R', ino1)
        call chpver('C', chsigi(1:19), 'NOEU', 'SIEF_R', ino2)
        call chpver('C', chsigi(1:19), 'ELGA', 'SIEF_R', inga)

!       Verification du type de champ
        pbtype=0
        if (.not.lxfem) then
!         cas FEM : verif que le champ est soit ELNO, soit NOEU, soit ELGA
          if (ino1.eq.1 .and. ino2.eq.1 .and. inga.eq.1) pbtype=1
        elseif (lxfem) then
!         cas X-FEM : verif que le champ est ELGA (seul cas autorise)
          if (inga.eq.1) pbtype=1
        endif            
        if (pbtype.eq.1) call utmess('F', 'RUPTURE1_12')

!       transformation si champ ELGA
        if (inga.eq.0) then

!           traitement du champ pour les elements finis classiques
            call detrsd('CHAMP',celmod)
            call alchml(ligrmo, 'CALC_G', 'PSIGINR', 'V', celmod,&
                        iret, ' ')
            call chpchd(chsigi(1:19), 'ELNO', celmod, 'OUI', 'V',&
                        sigelno)
            call chpver('F', sigelno(1:19), 'ELNO', 'SIEF_R', ibid)

!           calcul d'un champ supplementaire aux noeuds des sous-elements si X-FEM
            if (lxfem) call xelgano(modele,chsigi,sigseno)
!            call imprsd('CHAMP',chsigi,6,'chsigi')

        endif
    endif


!   RECUPERATION (S'ILS EXISTENT) DES CHAMP DE TEMPERATURES (T,TREF)
    call vrcins(modele, mate, ' ', time, chvarc,&
                codret)
!
    call vrcref(modele, mate(1:8), '        ', chvref(1:19))
!
!     TRAITEMENT DES CHARGES
!     ON SHUNTE SI COHESIF
    if(typdisc.eq.'COHESIF') then
!       Parametres non utilises
!       ms renseignes pour ne pas pourrir l'appel à calcul.F90
!       avec des conditions partout
        pavolu = 'PFRVOLU'
        pa2d3d = 'PFR2D3D'
        papres = 'PPRESSR'
        pepsin = 'PEPSINR'
!       De façon analogue à ce qui est fait pour CALC_K_G_F,
!       on change le nom d'option: programmation plus simple.
!       Evite de passer un champ supplementaire à calcul.F90
!       pour porter l'info du type de discontinuite
!       Evite egalement l'ajout d'un test de sortie pour éléments XH
        opti='CALC_K_G_COHE'
    else   
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
    endif
!
!     RECUPERATION DES DONNEES XFEM OU FEM (TOPOSE)
    pintto = modele(1:8)//'.TOPOSE.PIN'
    cnseto = modele(1:8)//'.TOPOSE.CNS'
    heavto = modele(1:8)//'.TOPOSE.HEA'
    hea_no = modele(1:8)//'.TOPONO.HNO'
    loncha = modele(1:8)//'.TOPOSE.LON'
    pmilto = modele(1:8)//'.TOPOSE.PMI'
    stano = modele(1:8)//'.STNO'
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
!   pourquoi modifier NDIMTE (argument d'entree)
    if ((liss.eq.'LAGRANGE').or.(liss.eq.'LAGRANGE_NO_NO').or.(liss.eq.'MIXTE')) then
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
!   BOUCLE SUR LES DIFFERENTS CHAMPS THETA
    do i = 1, ndimte
!
        call inical(26,lpain,lchin,1,lpaout,lchout)
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
        lpain(27) = 'PPMILTO'
        lchin(27) = pmilto
        lpain(28) = 'PHEA_NO'
        lchin(28) = hea_no
        lpain(29) = 'PSTANO'
        lchin(29) = stano
!
        nchin = 29
!

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
                        ncmp=1, nomcmp='FREQ   ', sr=puls)
            nchin = nchin + 1
            lpain(nchin) = 'PPULPRO'
            lchin(nchin) = chpuls
        endif

!       CHAMP DE CONTRAINTE INITIALE
        if (nsig .ne. 0) then
          if (inga .eq. 0) then
!           champ de contrainte initiale transforme en ELNO
            lpain(nchin+1) = 'PSIGINR'
            lchin(nchin+1)=sigelno
            nchin = nchin + 1

!           si X-FEM : champ de contrainte initiale transforme en SE-ELNO
            if (lxfem) then
                lpain(nchin+1) = 'PSIGISE'
                lchin(nchin+1) = sigseno
                nchin = nchin + 1
            endif

          else
!           champ de contrainte initiale donne par l'uutilisateur (NOEUD ou ELNO)
            lpain(nchin+1) = 'PSIGINR'
            lchin(nchin+1) = chsigi
            nchin = nchin + 1
          endif
        endif

        ASSERT(nchin.le.nbinmx)

        call calcul('S', opti, ligrmo, nchin, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
!
!       FAIRE LA "SOMME" D'UN CHAM_ELEM
        call mesomm(chgthi, 8, vr=gkthi)
!
!       SYMETRIE DU CHARGEMENT
        if (symech .eq. 'NON') then
            do j = 1, 7
                zr(iadrgk-1+(i-1)*8+j) = gkthi(j)
            end do
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
    end do
!
!     ------------------------------------------------------------------
!     3) CALCUL DE G(S), K1(S), K2(S) ET K3(S) LE LONG DU FOND
!     ------------------------------------------------------------------
!
    call wkvect('&&CAKG3D.VALGK_S', 'V V R8', nnoff*5, iadgks)
!
    if ((liss.eq.'LAGRANGE').or.(liss.eq.'LAGRANGE_NO_NO')) then
        call wkvect('&&CAKG3D.VALGKI', 'V V R8', nnoff*5, iadgki)
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
!
    if ((liss.ne.'LAGRANGE').and.(liss.ne.'LAGRANGE_NO_NO').and.(liss.ne.'MIXTE')) then
        num = 1
        call gkmet1(ndeg, nnoff, chfond, iadrgk, iadgks,&
                    iadgki, abscur)
!
    else if ((liss.eq.'LAGRANGE').or.(liss.eq.'LAGRANGE_NO_NO').or.(liss.eq.'MIXTE')) then
        if ((liss.ne.'LAGRANGE').and.(liss.ne.'LAGRANGE_NO_NO')) then
            num = 2
            call utmess('F', 'RUPTURE1_17')
        else
            num = 3
            call gkmet3(nnoff, chfond, iadrgk, milieu, connex,&
                        iadgks, iadgki, abscur, num, typdis)
        endif
    endif
!
!     IMPRESSION DE G(S), K1(S), K2(S) ET K3(S)
    if (niv .ge. 2) then
        call gksimp(result, nnoff, zr(iadabs), num,&
                    iadgks, ndeg, ndimte, iadgki, extim,&
                    time, iord, ifm)
    endif
!
!     ECRITURE DE LA TABLE DE G(S), K1(S), K2(S) ET K3(S)
    call getvis('THETA', 'NUME_FOND', iocc=1, scal=numfon, nbret=ibid)
!
    call tbajvi(result, nbprup, 'NUME_FOND', numfon, livi)
    
    if (lmelas) then
        call tbajvi(result, nbprup, 'NUME_CAS', iord, livi)
        call tbajvk(result, nbprup, 'NOM_CAS', nomcas, livk)
    else if (lmoda) then
        call tbajvi(result, nbprup, 'NUME_MODE', iord, livi)
    else
        call tbajvi(result, nbprup, 'NUME_ORDRE', iord, livi)
        call tbajvr(result, nbprup, 'INST', time, livr)
    endif
!
    diff2g = 0.d0
    difrel = 0.d0
!
    do i = 1, nnoff
        if (.not.lxfem) then
            call tbajvk(result, nbprup, 'NOEUD', zk8(iadnoe-1+i), livk)
        endif
        call tbajvi(result, nbprup, 'NUM_PT', i, livi)
        call tbajvr(result, nbprup, 'ABSC_CURV', zr(coor-1+4*(i-1)+4), livr)
        call tbajvr(result, nbprup, 'COOR_X', zr(coor-1+4*(i-1)+1), livr)
        call tbajvr(result, nbprup, 'COOR_Y', zr(coor-1+4*(i-1)+2), livr)
        call tbajvr(result, nbprup, 'COOR_Z', zr(coor-1+4*(i-1)+3), livr)
        call tbajvr(result, nbprup, 'K1', zr(iadgks-1+5*(i-1)+2), livr)
        call tbajvr(result, nbprup, 'K2', zr(iadgks-1+5*(i-1)+3), livr)
        call tbajvr(result, nbprup, 'K3', zr(iadgks-1+5*(i-1)+4), livr)
        call tbajvr(result, nbprup, 'G', zr(iadgks-1+5*(i-1)+1), livr)
        call tbajvr(result, nbprup, 'G_IRWIN', zr(iadgks-1+5*(i-1)+5), livr)
        call tbajli(result, nbprup, noprup, livi, livr,&
                    livc, livk, 0)
        if ((codret .eq. 'OK') .and. ( abs( zr(iadgks-1+5*(i-1)+1) ) .ge. 1.e-12 ) ) then
            difrel= abs((zr(iadgks-1+5*(i-1)+1)- zr(iadgks-1+5*(i-1)+5))/zr(iadgks-1+5*(i-1)+1))
            diff2g = diff2g + difrel
        endif
!
    end do
!
    if ((codret .eq. 'OK') .and. (diff2g/nnoff.gt.0.5)) call utmess('A', 'RUPTURE1_71',&
                                                                    sr = diff2g)
!
!
!- DESTRUCTION D'OBJETS DE TRAVAIL
!
    call jedetr(abscur)
    call jedetr('&&CAKG3D.VALGK_S')
    if(typdisc.ne.'COHESIF') then
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
    endif
!
    call jedema()
end subroutine
