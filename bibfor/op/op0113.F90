subroutine op0113()
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
!
! ----------------------------------------------------------------------
!
! OPERATEUR MODI_MODELE_XFEM
!
!
! ----------------------------------------------------------------------
!
!
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/adalig.h"
#include "asterfort/assert.h"
#include "asterfort/cormgi.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/initel.h"
#include "asterfort/ismali.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedup1.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xcodec.h"
#include "asterfort/xcpmod.h"
#include "asterfort/xmolig.h"
#include "asterfort/xtyele.h"
#include "asterfort/xverm2.h"
#include "asterfort/xvermo.h"
!
    real(kind=8) :: crimax
    integer :: ibid, iel, ima, nmoth
    integer :: i, j2
    integer :: jmofis
    integer :: nbma, nelt
    integer :: nb1
    integer :: nfiss, jnfis
    integer :: ndim
    character(len=16) :: motfac, k16bid, line_quad
    character(len=19) :: ligr1, ligr2
    character(len=24) :: liel1, liel2
    character(len=24) :: mail2
    character(len=24) :: trav
    integer :: jmail2, jtab, jxc
    character(len=8) :: modelx, mod1, modthx, noma, k8cont, k8condi
    aster_logical :: linter
    character(len=8), pointer :: lgrf1(:) => null()
    character(len=8), pointer :: lgrf2(:) => null()
    character(len=8), pointer :: p_mod_sain(:) => null()
!
    data motfac /' '/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NOM DU MODELE MODIFIE
!
    call getres(modelx, k16bid, k16bid)
    ligr2 = modelx//'.MODELE'
    liel2 = ligr2//'.LIEL'
!
! --- NOM DU MODELE INITIAL
!
    call getvid(motfac, 'MODELE_IN', iocc=1, scal=mod1, nbret=ibid)
    ligr1 = mod1//'.MODELE'
    liel1 = ligr1//'.LIEL'
!
    call wkvect(modelx//'.MODELE_SAIN', 'G V K8', 1, vk8=p_mod_sain)
    p_mod_sain(1) = mod1
!
! --- ACCES AU MAILLAGE INITIAL
!
    call jeveuo(ligr1//'.LGRF', 'L', vk8=lgrf1)
    noma = lgrf1(1)
    call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)
!
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
!
! --- MOT-CLE MODELE_THER : CAS PARTICULIER (X-FEM THERMO MECA)
!     QUE L'ON TRAITE SEPAREMENT DU CAS GENERAL
!
    call getvid(motfac, 'MODELE_THER', iocc=1, scal=modthx, nbret=nmoth)
    if (nmoth .eq. 1) then
        call xcpmod(mod1, modthx, modelx)
        goto 999
    endif
!
! --- RECUPERER LE NOMBRE DE FISSURES
!
    call getvid(motfac, 'FISSURE', iocc=1, nbval=0, nbret=nfiss)
    nfiss = -nfiss
!
! --- CREATION DES OBJETS POUR MULTIFISSURATION DANS MODELE MODIFIE
!
    call wkvect(modelx//'.NFIS', 'G V I', 1, jnfis)
    call wkvect(modelx//'.FISS', 'G V K8', nfiss, jmofis)
    zi(jnfis) = nfiss
!
! --- RECUPERER LES FISSURES ET REMPLISSAGE DE MODELX//'.FISS'
!
    call getvid(motfac, 'FISSURE', iocc=1, nbval=nfiss, vect=zk8(jmofis),&
                nbret=ibid)
!
!     VERIFICATION DE LA COHERENCE DES MOT-CLES FISSURE ET MODELE_IN
!     (COHERENCE DES MAILLAGES SOUS-JACENTS AUX FISSURES ET MODELE)
    call xvermo(nfiss, zk8(jmofis), noma)
!
!     VERIFS POUR LES MODELISATIONS "EXOTIQUES" (multi-h, thermique, HM)
    call xverm2(nfiss, zk8(jmofis), mod1)
!
!
! --- CONTACT ?
!
    call getvtx(motfac, 'CONTACT', iocc=1, scal=k8cont, nbret=ibid)
    call wkvect(modelx//'.XFEM_CONT', 'G V I', 1, jxc)
    if (k8cont .eq. 'SANS') then
        zi(jxc) = 0
    else if (k8cont .eq. 'MORTAR') then
        zi(jxc) = 2
    else if (k8cont.eq.'STANDARD') then
        call dismoi('LINE_QUAD', ligr1, 'LIGREL', repk = line_quad)
        if (line_quad .eq. 'LINE') then
            zi(jxc) = 1
        else if (line_quad.eq.'QUAD') then
            zi(jxc) = 3
        else
            call utmess('F', 'XFEM2_3')
        endif
    else
        ASSERT(.false.)
    endif
!
! --- CREATION DU TABLEAU DE TRAVAIL
!
    trav = '&&OP0113.TAB'
    call wkvect(trav, 'V V I', nbma*5, jtab)
!
    do i = 1, nbma
        zi(jtab-1+5*(i-1)+4) = 1
    end do
!
! ---------------------------------------------------------------------
!     1)  REMPLISSAGE DE TAB : NBMA X 5 : GR1 | GR2 | GR3 | GR0 | ITYP
! ---------------------------------------------------------------------
!
    call xtyele(mod1, trav, nfiss, zk8(jmofis), zi(jxc),&
                ndim, linter)
!
! ---------------------------------------------------------------------
!       2)  MODIFICATION DE TAB EN FONCTION DE L'ENRICHISSEMENT
! ---------------------------------------------------------------------
!
    call xmolig(liel1, trav)
!
! --- ON COMPTE LE NB DE MAILLES DU LIGREL1 (= NB DE GREL DE LIEL2)
!
    nelt = 0
    do ima = 1, nbma
        if (zi(jtab-1+5*(ima-1)+5) .ne. 0) then
            nelt = nelt+1
        endif
    end do
    if (nelt .eq. 0) then
        call utmess('F', 'XFEM2_51')
    endif
!
!-----------------------------------------------------------------------
!     3)  CONSTRUCTION DU .LIEL2
!-----------------------------------------------------------------------
!
    call jecrec(liel2, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nelt)
    call jeecra(liel2, 'LONT', 2*nelt)
!
    iel=0
    do ima = 1, nbma
        if (zi(jtab-1+5*(ima-1)+5) .eq. 0) goto 300
        iel=iel+1
        call jecroc(jexnum(liel2, iel))
        call jeecra(jexnum(liel2, iel), 'LONMAX', 2)
        call jeveuo(jexnum(liel2, iel), 'E', j2)
        zi(j2-1+1)=ima
        zi(j2-1+2)=zi(jtab-1+5*(ima-1)+5)
300     continue
    end do
!
    call jelira(liel2, 'NUTIOC', nb1)
    ASSERT(nb1.eq.nelt)
!
!-----------------------------------------------------------------------
!     4)  CONSTRUCTION DU .MAILLE
!-----------------------------------------------------------------------
!
    mail2 = modelx//'.MAILLE'
    call wkvect(mail2, 'G V I', nbma, jmail2)
    do ima = 1, nbma
        zi(jmail2-1+ima)=zi(jtab-1+5*(ima-1)+5)
    end do
!
!-----------------------------------------------------------------------
!     5) DUPLICATION DU .NOMA, .NBNO
!                ET DES .NEMA, .SSSA, .NOEUD S'ILS EXISTENT
!        PUIS .REPE, .PRNM ET .PRNS AVEC CALL ADALIG CORMGI ET INITEL
!-----------------------------------------------------------------------
!
    call jedupo(ligr1//'.NBNO', 'G', ligr2//'.NBNO', .false._1)
    call jedupo(ligr1//'.LGRF', 'G', ligr2//'.LGRF', .false._1)
    call jeveuo(ligr2//'.LGRF', 'E', vk8=lgrf2)
    lgrf2(2)=modelx
!
    call jedup1(mod1//'.NEMA', 'G', modelx//'.NEMA')
    call jedup1(mod1//'.SSSA', 'G', modelx//'.SSSA')
    call jedup1(mod1//'.NOEUD', 'G', modelx//'.NOEUD')
!
    call adalig(ligr2)
    call cormgi('G', ligr2)
    call initel(ligr2)
!
!-----------------------------------------------------------------------
!     6)  CALCUL DU DÉCOUPAGE EN SOUS-TETRAS, DES FACETTES DE CONTACT
!         ET VERIFICATION DES CRITERES DE CONDITIONNEMENT
!-----------------------------------------------------------------------
!
    call getvtx(motfac, 'PRETRAITEMENTS', iocc=1, scal=k8condi, nbret=ibid)
    call getvr8(motfac, 'CRITERE', iocc=1, scal=crimax, nbret=ibid)
!
    call xcodec(noma, modelx, k8condi, crimax, linter)
!
! --- MENAGE
!
    call jedetr(trav)
!
999 continue
    call jedema()
end subroutine
