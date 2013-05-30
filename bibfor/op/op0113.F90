subroutine op0113()
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/adalig.h'
    include 'asterfort/assert.h'
    include 'asterfort/cormgi.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/initel.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jedup1.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'asterfort/xcodec.h'
    include 'asterfort/xmolig.h'
    include 'asterfort/xtyele.h'
    include 'asterfort/xvermo.h'
    real(kind=8) :: crimax
    integer :: ibid, iel, ima
    integer :: i, j2
    integer :: jlgrf1, jlgrf2, jmofis
    integer :: nbma, nelt
    integer :: nb1
    integer :: nfiss, jnfis
    integer :: ndim
    character(len=16) :: motfac, k16bid
    character(len=19) :: ligr1, ligr2
    character(len=24) :: liel1, liel2
    character(len=24) :: mail2
    character(len=24) :: trav
    integer :: jmail2, jtab, jxc, iret
    character(len=8) :: modelx, mod1, k8bid, noma, k8cont
    logical :: linter
    integer :: iarg
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
    call getvid(motfac, 'MODELE_IN', 1, iarg, 1,&
                mod1, ibid)
    ligr1 = mod1//'.MODELE'
    liel1 = ligr1//'.LIEL'
!
! --- ACCES AU MAILLAGE INITIAL
!
    call jeveuo(ligr1//'.LGRF', 'L', jlgrf1)
    noma = zk8(jlgrf1-1+1)
    call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndim,&
                k8bid, iret)
!
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                k8bid, ibid)
!
! --- RECUPERER LE NOMBRE DE FISSURES
!
    call getvid(motfac, 'FISSURE', 1, iarg, 0,&
                k8bid, nfiss)
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
    call getvid(motfac, 'FISSURE', 1, iarg, nfiss,&
                zk8(jmofis), ibid)
    call getvr8(motfac, 'CRITERE', 1, iarg, 1,&
                crimax, ibid)
!
!     VERIFICATION DE LA COHERENCE DES MOT-CLES FISSURE ET MODELE_IN
    call xvermo(nfiss, zk8(jmofis), mod1)
!
!
! --- CONTACT ?
!
    call getvtx(motfac, 'CONTACT', 1, iarg, 1,&
                k8cont, ibid)
    call wkvect(modelx//'.XFEM_CONT', 'G V I', 1, jxc)
    if (k8cont .eq. 'P1P1') then
        zi(jxc) = 1
    else if (k8cont.eq.'P2P1') then
        zi(jxc) = 3
    else
        zi(jxc) = 0
    endif
!
! --- CREATION DU TABLEAU DE TRAVAIL
!
    trav = '&&OP0113.TAB'
    call wkvect(trav, 'V V I', nbma*5, jtab)
!
    do 110 i = 1, nbma
        zi(jtab-1+5*(i-1)+4) = 1
110  end do
!
! ---------------------------------------------------------------------
!     1)  REMPLISSAGE DE TAB : NBMA X 5 : GR1 | GR2 | GR3 | GR0 | ITYP
! ---------------------------------------------------------------------
!
    call xtyele(noma, trav, nfiss, zk8(jmofis), zi(jxc),&
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
    do 230 ima = 1, nbma
        if (zi(jtab-1+5*(ima-1)+5) .ne. 0) then
            nelt = nelt+1
        endif
230  end do
    if (nelt .eq. 0) then
        call u2mess('F', 'XFEM2_51')
    endif
!
!-----------------------------------------------------------------------
!     3)  CONSTRUCTION DU .LIEL2
!-----------------------------------------------------------------------
!
    call jecrec(liel2, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nelt)
    call jeecra(liel2, 'LONT', 2*nelt, k8bid)
!
    iel=0
    do 300 ima = 1, nbma
        if (zi(jtab-1+5*(ima-1)+5) .eq. 0) goto 300
        iel=iel+1
        call jecroc(jexnum(liel2, iel))
        call jeecra(jexnum(liel2, iel), 'LONMAX', 2, k8bid)
        call jeveuo(jexnum(liel2, iel), 'E', j2)
        zi(j2-1+1)=ima
        zi(j2-1+2)=zi(jtab-1+5*(ima-1)+5)
300  end do
!
    call jelira(liel2, 'NUTIOC', nb1, k8bid)
    call assert(nb1.eq.nelt)
!
!-----------------------------------------------------------------------
!     4)  CONSTRUCTION DU .MAILLE
!-----------------------------------------------------------------------
!
    mail2 = modelx//'.MAILLE'
    call wkvect(mail2, 'G V I', nbma, jmail2)
    do 400 ima = 1, nbma
        zi(jmail2-1+ima)=zi(jtab-1+5*(ima-1)+5)
400  end do
!
!-----------------------------------------------------------------------
!     5) DUPLICATION DU .NOMA, .NBNO
!                ET DES .NEMA, .SSSA, .NOEUD S'ILS EXISTENT
!        PUIS .REPE, .PRNM ET .PRNS AVEC CALL ADALIG CORMGI ET INITEL
!-----------------------------------------------------------------------
!
    call jedupo(ligr1//'.NBNO', 'G', ligr2//'.NBNO', .false.)
    call jedupo(ligr1//'.LGRF', 'G', ligr2//'.LGRF', .false.)
    call jeveuo(ligr2//'.LGRF', 'E', jlgrf2)
    zk8(jlgrf2-1+2)=modelx
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
    call xcodec(noma, modelx, ndim, crimax, linter)
!
! --- MENAGE
!
    call jedetr(trav)
!
    call jedema()
end subroutine
