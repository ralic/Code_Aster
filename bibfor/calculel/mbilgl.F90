subroutine mbilgl(option, result, modele, depla1, depla2,&
                  thetai, mate, lischa, symech, chfond,&
                  nnoff, ndeg, thlagr, glagr, thlag2,&
                  milieu, ndimte, pair, extim, timeu,&
                  timev, indi, indj, nbprup, noprup,&
                  lmelas, nomcas, fonoeu)
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/gcharg.h"
#include "asterfort/getvid.h"
#include "asterfort/gimpgs.h"
#include "asterfort/gmeth1.h"
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
#include "asterfort/tbajli.h"
#include "asterfort/tbajvi.h"
#include "asterfort/tbajvk.h"
#include "asterfort/tbajvr.h"
#include "asterfort/utmess.h"
#include "asterfort/vrcins.h"
#include "asterfort/vrcref.h"
#include "asterfort/wkvect.h"
!
    integer :: nnoff, indi, indj, ndeg
    integer :: nbprup, ndimte
!
    real(kind=8) :: timeu, timev
!
    character(len=19) :: lischa
    character(len=8) :: modele, thetai
    character(len=8) :: result, symech
    character(len=16) :: option, noprup(*), nomcas
    character(len=24) :: depla1, depla2, chfond, mate, fonoeu
!
    logical :: extim, thlagr, glagr, milieu, pair
    logical :: ufonc, vfonc, thlag2, lmelas
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
!  IN    OPTION --> G_BILI / G_BILI_F
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
!  IN    THLAGR --> VRAI SI LISSAGE THETA_LAGRANGE
!  IN    THLAG2 --> VRAI SI LISSAGE THETA_LAGRANGE_REGU
!  IN    GLAGR  --> VRAI SI LISSAGE G_LAGRANGE
!  IN    NDEG   --> DEGRE DU POLYNOME DE LEGENDRE
!  IN    MILIEU --> .TRUE.  : ELEMENT QUADRATIQUE
!                   .FALSE. : ELEMENT LINEAIRE
!  IN    LMELAS --> TRUE SI LE TYPE DE LA SD RESULTAT EST MULT_ELAS
!  IN    NOMCAS --> NOM DU CAS DE CHARGE SI LMELAS
!  IN    FONOEU --> NOM DES NOEUDS DE FOND DE FISSURE
!
! ......................................................................
!
    integer :: nbmxpa
    parameter (nbmxpa = 20)
!
    integer :: i, ibid, iadrg, iadrgs, jresu, nchin
    integer :: num
    integer :: ifon, init
    integer :: iadrno, iadgi, iadabs, ifm, niv
    integer :: iord, livi(nbmxpa)
!
    real(kind=8) :: gthi, livr(nbmxpa), xl
!
    complex(kind=8) :: cbid, livc(nbmxpa)
!
!
    character(len=2) :: codret
    character(len=8) :: lpain(25), lpaout(1)
    character(len=16) :: opti, livk(nbmxpa), valk
    character(len=24) :: ligrmo, chgeom, chgthi
    character(len=19) :: vrcmoi, vrcplu
    character(len=24) :: chvref
    character(len=24) :: lchin(25), lchout(1), chthet, chtimu, chtimv
    character(len=24) :: objcur
    character(len=24) :: chsigi
!
    character(len=19) :: uchvol, vchvol, ucf12d, vcf12d, ucf23d, vcf23d
    character(len=19) :: uchpre, vchpre, ucheps, vcheps, uchpes, vchpes
    character(len=19) :: uchrot, vchrot
    character(len=24) :: upavol, vpavol, upa23d, vpa23d, upapre, vpapre
    character(len=24) :: upepsi, vpepsi
!
    data vrcmoi /'&&MBILGL.VRCM'/
    data vrcplu /'&&MBILGL.VRCP'/
    data chvref /'&&MBILGL.VRCR'/
    data chtimu /'&&MBILGL.TIMU'/
    data chtimv /'&&MBILGL.TIMV'/
    data uchvol /'&&MBILGL.VOLU'/
    data ucf12d /'&&MBILGL.1D2D'/
    data ucf23d /'&&MBILGL.2D3D'/
    data uchpre /'&&MBILGL.PRES'/
    data ucheps /'&&MBILGL.EPSI'/
    data uchpes /'&&MBILGL.PESA'/
    data uchrot /'&&MBILGL.ROTA'/
    data vchvol /'&&MBILGL.VOLU'/
    data vcf12d /'&&MBILGL.1D2D'/
    data vcf23d /'&&MBILGL.2D3D'/
    data vchpre /'&&MBILGL.PRES'/
    data vcheps /'&&MBILGL.EPSI'/
    data vchpes /'&&MBILGL.PESA'/
    data vchrot /'&&MBILGL.ROTA'/
!
!
!     ------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
!- RECUPERATION DU CHAMP GEOMETRIQUE
!
    call megeom(modele, chgeom)
!
!- RECUPERATION DE L'ETAT INITIAL (NON TRAITE DANS CETTE OPTION)
!
    call getvid('COMP_INCR', 'SIGM_INIT', iocc=1, scal=chsigi, nbret=init)
    if (init .ne. 0) then
        valk='G_BILI'
        call utmess('F', 'RUPTURE1_13', sk=valk)
    endif
!
    call vrcref(modele, mate(1:8), '        ', chvref(1:19))
    call vrcins(modele, mate, ' ', timeu, vrcmoi,&
                codret)
    call vrcins(modele, mate, ' ', timev, vrcplu,&
                codret)
!
!
! - TRAITEMENT DES CHARGES U
!
!
    call gcharg(modele, lischa, uchvol, ucf12d, ucf23d,&
                uchpre, ucheps, uchpes, uchrot, ufonc,&
                timeu, indi)
!
    if (ufonc) then
        upavol = 'UPFFVOL'
        upa23d = 'UPFF23D'
        upapre = 'UPRESSF'
        upepsi = 'UEPSINF'
        opti = 'G_BILI_F'
    else
        upavol = 'UPFRVOL'
        upa23d = 'UPFR23D'
        upapre = 'UPRESSR'
        upepsi = 'UEPSINR'
        opti = option
    endif
!
! - TRAITEMENT DES CHARGES V
!
    call gcharg(modele, lischa, vchvol, vcf12d, vcf23d,&
                vchpre, vcheps, vchpes, vchrot, vfonc,&
                timev, indj)
!
    if (vfonc) then
        vpavol = 'VPFFVOL'
        vpa23d = 'VPFF23D'
        vpapre = 'VPRESSF'
        vpepsi = 'VEPSINF'
        opti = 'G_BILI_F'
    else
        vpavol = 'VPFRVOL'
        vpa23d = 'VPFR23D'
        vpapre = 'VPRESSR'
        vpepsi = 'VEPSINR'
        opti = option
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
    call wkvect('&&MBILGL.VALG', 'V V R8', ndimte, iadrg)
    call jeveuo(thetai, 'L', jresu)
!
    do 20 i = 1, ndimte
        chthet = zk24(jresu+i-1)
        call codent(i, 'G', chgthi)
        lpaout(1) = 'PGTHETA'
        lchout(1) = chgthi
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PDEPLAU'
        lchin(2) = depla1
        lpain(3) = 'PTHETAR'
        lchin(3) = chthet
        lpain(4) = 'PMATERC'
        lchin(4) = mate
        lpain(5) = 'PVARCMR'
        lchin(5) = vrcmoi
        lpain(6) = 'PDEPLAV'
        lchin(6) = depla2
        lpain(7) = 'PVARCPR'
        lchin(7) = vrcplu
        lpain(8) = upavol(1:8)
        lchin(8) = uchvol
        lpain(9) = vpavol(1:8)
        lchin(9) = vchvol
        lpain(10) = upa23d(1:8)
        lchin(10) = ucf23d
        lpain(11) = vpa23d(1:8)
        lchin(11) = vcf23d
        lpain(12) = upapre(1:8)
        lchin(12) = uchpre
        lpain(13) = vpapre(1:8)
        lchin(13) = vchpre
        lpain(14) = upepsi(1:8)
        lchin(14) = ucheps
        lpain(15) = vpepsi(1:8)
        lchin(15) = vcheps
        lpain(16) = 'UPESANR'
        lchin(16) = uchpes
        lpain(17) = 'VPESANR'
        lchin(17) = vchpes
        lpain(18) = 'UROTATR'
        lchin(18) = uchrot
        lpain(19) = 'VROTATR'
        lchin(19) = vchrot
        lpain(20) = 'PVARCRR'
        lchin(20) = chvref
!
        ligrmo = modele//'.MODELE'
        nchin = 20
        if (opti .eq. 'G_BILI_F') then
            call mecact('V', chtimu, 'MODELE', ligrmo, 'INST_R  ',&
                        ncmp=1, nomcmp='INST   ', sr=timeu)
            nchin = nchin + 1
            lpain(nchin) = 'UTEMPSR'
            lchin(nchin) = chtimu
!
            call mecact('V', chtimv, 'MODELE', ligrmo, 'INST_R  ',&
                        ncmp=1, nomcmp='INST   ', sr=timev)
            nchin = nchin + 1
            lpain(nchin) = 'VTEMPSR'
            lchin(nchin) = chtimv
        endif
!
        call calcul('S', opti, ligrmo, nchin, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
        call mesomm(chgthi, 1, ibid, gthi, cbid,&
                    0, ibid)
        zr(iadrg+i-1) = gthi
20  continue
!
! ABSCISSE CURVILIGNE
    call jeveuo(chfond, 'L', ifon)
    objcur = '&&MBILGL.ABSGAMM0'
    call wkvect(objcur, 'V V R', nnoff, iadabs)
    do 11 i = 1, nnoff
        zr(iadabs-1+(i-1)+1)=zr(ifon-1+4*(i-1)+4)
11  continue
    xl=zr(iadabs-1+(nnoff-1)+1)
!
!- CALCUL DE G(S) SUR LE FOND DE FISSURE PAR 2 METHODES
!- PREMIERE METHODE : G_LEGENDRE ET THETA_LEGENDRE
!- DEUXIEME METHODE: G_LAGRANGE ET THETA_LAGRANGE
!
    call wkvect('&&MBILGL.VALG_S', 'V V R8', nnoff, iadrgs)
    if (glagr .or. thlag2) then
        call wkvect('&&MBILGL.VALGI', 'V V R8', nnoff, iadgi)
    else
        call wkvect('&&MBILGL.VALGI', 'V V R8', ndeg+1, iadgi)
    endif
!
    if (thlag2) then
        num = 5
        call gmeth4(nnoff, ndimte, fonoeu, zr(iadrg), milieu,&
                    pair, zr( iadrgs), objcur, zr(iadgi), .false.)
    else if ((.not.glagr) .and. (.not.thlagr)) then
        num = 1
        call gmeth1(nnoff, ndeg, zr(iadrg), zr(iadrgs), objcur,&
                    xl, zr( iadgi))
    else if (glagr .and. thlagr) then
        call gmeth3(nnoff, fonoeu, zr(iadrg), milieu, zr(iadrgs),&
                    objcur, zr(iadgi), num, .false.)
    endif
!
!- SYMETRIE DU CHARGEMENT ET IMPRESSION DES RESULTATS
!
    if (symech .ne. 'NON') then
        do 30 i = 1, nnoff
            zr(iadrgs+i-1) = 2.d0*zr(iadrgs+i-1)
30      continue
    endif
!
!- IMPRESSION ET ECRITURE DANS TABLE(S) DE G(S)
!
    call jeveuo(fonoeu, 'L', iadrno)
!
    if (niv .ge. 2) then
        call gimpgs(result, nnoff, zr(iadabs), zr(iadrgs), num,&
                    zr(iadgi), ndeg, ndimte, zr(iadrg), extim,&
                    timeu, iord, ifm)
    endif
!
    if (lmelas) then
        call tbajvk(result, nbprup, 'NOM_CAS', nomcas, livk)
    else
        call tbajvr(result, nbprup, 'INST', timeu, livr)
    endif
!
    do 40 i = 1, nnoff
        call tbajvi(result, nbprup, 'NUME_CMP_I', indi, livi)
        call tbajvi(result, nbprup, 'NUME_CMP_J', indj, livi)
        call tbajvk(result, nbprup, 'NOEUD', zk8(iadrno+i-1), livk)
        call tbajvr(result, nbprup, 'ABSC_CURV', zr(iadabs-1+i), livr)
        call tbajvr(result, nbprup, 'G_BILI_LOCAL', zr(iadrgs+i-1), livr)
        call tbajli(result, nbprup, noprup, livi, livr,&
                    livc, livk, 0)
40  continue
!
!- DESTRUCTION D'OBJETS DE TRAVAIL
!
    call jedetr(objcur)
    call jedetr('&&MBILGL.VALG_S')
    call jedetr('&&MBILGL.VALG')
    call jedetr('&&MBILGL.VALGI')
!
    call detrsd('CHAMP_GD', vrcmoi)
    call detrsd('CHAMP_GD', vrcplu)
    call detrsd('CHAMP_GD', chvref)
    call detrsd('CHAMP_GD', chtimu)
    call detrsd('CHAMP_GD', chtimv)
    call detrsd('CHAMP_GD', uchvol)
    call detrsd('CHAMP_GD', vchvol)
    call detrsd('CHAMP_GD', ucf23d)
    call detrsd('CHAMP_GD', vcf23d)
    call detrsd('CHAMP_GD', uchpre)
    call detrsd('CHAMP_GD', vchpre)
    call detrsd('CHAMP_GD', ucheps)
    call detrsd('CHAMP_GD', vcheps)
    call detrsd('CHAMP_GD', uchpes)
    call detrsd('CHAMP_GD', vchpes)
    call detrsd('CHAMP_GD', uchrot)
    call detrsd('CHAMP_GD', vchrot)
!
!
    call jedema()
end subroutine
