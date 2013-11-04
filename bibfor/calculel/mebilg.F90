subroutine mebilg(optioz, result, modele, depla1, depla2,&
                  theta, mate, lischa, symech, timeu,&
                  timev, indi, indj, nbprup, noprup)
    implicit none
!
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/detrsd.h"
#include "asterfort/gcharg.h"
#include "asterfort/getvid.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/megeom.h"
#include "asterfort/mesomm.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajvi.h"
#include "asterfort/tbajvr.h"
#include "asterfort/utmess.h"
#include "asterfort/vrcins.h"
#include "asterfort/vrcref.h"
!
    character(len=19) :: lischa
    character(len=8) :: modele, result, symech
    character(len=16) :: optioz, noprup(*)
    character(len=24) :: depla1, depla2, mate, theta
    real(kind=8) :: timeu, timev
    integer :: indi, indj, nbprup
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
!     - FONCTION REALISEE:   CALCUL DU G BILINEAIRE EN 3D
!
! IN   OPTION  --> G_BILI
! IN   RESULT  --> NOM UTILISATEUR DU RESULTAT ET TABLE
! IN   MODELE  --> NOM DU MODELE
! IN   DEPLA1  --> CHAMP DE DEPLACEMENT U
! IN   THETA   --> CHAMP THETA
! IN   MATE    --> CHAMP DE MATERIAUX
! IN   SYMECH  --> SYMETRIE DU CHARGEMENT
! IN   EXTIM   --> VRAI SI L'INSTANT EST DONNE
! IN   TIME    --> INSTANT DE CALCUL
! IN   INDI    --> INDICE I DU DEPLACEMENT U DANS LA SD
! IN   INDJ    --> INDICE J DU DEPLACEMENT U DANS LA SD
! ......................................................................
!
    integer :: nbmxpa
    parameter (nbmxpa = 20)
!
    integer :: init, niv, ifm
    integer :: nchin, livi(nbmxpa)
!
    real(kind=8) :: g(1), livr(nbmxpa)
!
    complex(kind=8) :: livc(nbmxpa)
!
    logical :: ufonc, vfonc
!
    character(len=2) :: codret
    character(len=8) :: lpain(20), lpaout(1)
    character(len=16) :: option, opti, valk
    character(len=24) :: chgeom, chvref, chsigi
    character(len=24) :: lchin(20), lchout(1), ligrmo
    character(len=19) :: uchvol, vchvol, ucf12d, vcf12d, ucf23d, vcf23d
    character(len=19) :: uchpre, vchpre, ucheps, vcheps, uchpes, vchpes
    character(len=19) :: uchrot, vchrot, vrcmoi, vrcplu
    character(len=24) :: upavol, vpavol, upa23d, vpa23d, upapre, vpapre
    character(len=24) :: upepsi, vpepsi, livk(nbmxpa)
!
!
!
    data vrcmoi /'&&MBILGL.VRCM'/
    data vrcplu /'&&MBILGL.VRCP'/
    data chvref /'&&MBILGL.VRCR'/
!
    call jemarq()
    option = optioz
    call infniv(ifm, niv)
!
!- RECUPERATION DU CHAMP GEOMETRIQUE
!
    call megeom(modele, chgeom)
!
!- RECUPERATION DE L'ETAT INITIAL (NON TRAITE DANS CETTE OPTION)
!
    call getvid('ETAT_INIT', 'SIGM', iocc=1, scal=chsigi, nbret=init)
    if (init .ne. 0) then
        valk='G_BILI'
        call utmess('F', 'RUPTURE1_13', sk=valk)
    endif
!
!- RECUPERATION (S'ILS EXISTENT) DES CHAMP
!  DE TEMPERATURE (TU, TV, TREF)
!
    call vrcref(modele, mate(1:8), '        ', chvref(1:19))
    call vrcins(modele, mate, ' ', timeu, vrcmoi,&
                codret)
    call vrcins(modele, mate, ' ', timev, vrcplu,&
                codret)
!
! - TRAITEMENT DES CHARGES U
!
    uchvol = '&&MBILGL.VOLU'
    ucf12d = '&&MBILGL.1D2D'
    ucf23d = '&&MBILGL.2D3D'
    uchpre = '&&MBILGL.PRES'
    ucheps = '&&MBILGL.EPSI'
    uchpes = '&&MBILGL.PESA'
    uchrot = '&&MBILGL.ROTA'
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
    vchvol = '&&MBILGL.VOLU'
    vcf12d = '&&MBILGL.1D2D'
    vcf23d = '&&MBILGL.2D3D'
    vchpre = '&&MBILGL.PRES'
    vcheps = '&&MBILGL.EPSI'
    vchpes = '&&MBILGL.PESA'
    vchrot = '&&MBILGL.ROTA'
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
    lpaout(1) = 'PGTHETA'
    lchout(1) = '&&FICGELE'
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PDEPLAU'
    lchin(2) = depla1
    lpain(3) = 'PTHETAR'
    lchin(3) = theta
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
    call calcul('S', opti, ligrmo, nchin, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
!  SOMMATION DES FIC ET G ELEMENTAIRES
!
    call mesomm(lchout(1), 1, vr=g(1))
!
    if (symech .ne. 'NON') g(1) = 2.d0*g(1)
!
! IMPRESSION DE G ET ECRITURE DANS LA TABLE RESU
!
    call tbajvi(result, nbprup, 'NUME_CMP_I', indi, livi)
    call tbajvi(result, nbprup, 'NUME_CMP_J', indj, livi)
    call tbajvr(result, nbprup, 'G_BILIN', g(1), livr)
    call tbajli(result, nbprup, noprup, livi, livr,&
                livc, livk, 0)
!
    call jedetr('&&MEBILG.VALG')
    call detrsd('CHAMP_GD', vrcmoi)
    call detrsd('CHAMP_GD', vrcplu)
    call detrsd('CHAMP_GD', chvref)
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
    call jedema()
end subroutine
