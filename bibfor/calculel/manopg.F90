subroutine manopg(ligrez, optioz, paramz, mnogaz)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/alchml.h'
    include 'asterfort/assert.h'
    include 'asterfort/cescre.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/elraca.h'
    include 'asterfort/elraga.h'
    include 'asterfort/elrfvf.h'
    include 'asterfort/indk32.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/manopx.h'
    include 'asterfort/nbelem.h'
    include 'asterfort/nbptca.h'
    include 'asterfort/typele.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: ligrez, mnogaz, optioz, paramz
! ------------------------------------------------------------------
! BUT: CREER LE CHAM_ELEM_S MNOGAZ QUI CONTIENDRA LA MATRICE
!      DE PASSAGE NOEUDS -> POINTS DE GAUSS POUR LES ELEMENTS
!      DU LIGREL ET POUR LA FAMILLE (OPTIOZ/PARAMZ)
! ------------------------------------------------------------------
!     ARGUMENTS:
! LIGREZ  IN/JXIN  K19 : LIGREL
! OPTIOZ,PARAMZ  IN  K* : OPTION ET PARAMETRE PERMETTANT DE DETERMINER
!                         LA FAMILLE DE PG UTILISEE.
! MNOGAZ  IN/JXOUT K19 : CHAM_ELEM_S (VARI_R) DE TYPE 'ELEM'
! ------------------------------------------------------------------
! REMARQUES :
!  MNOGAZ(IMA) EST UN VECTEUR V DE REELS DE DIMENSION 2 + NBNO*NBPG
!    V(1) : NBNO
!    V(2) : NBPG
!    V(2+NBNO*(IPG-1)+INO) : MATRICE DE PASSAGE (IPG,INO)
!
!  ATTENTION :
!     1) LES MAILLES TARDIVES SONT IGNOREES.
!     2) POUR ECONOMISER LE VOLUME DE MNOGAZ, ON UTILISE LE FAIT QUE
!        LES MATRICES DE PASSAGE DES ELEMENTS D'UN MEME GREL SONT
!        IDENTIQUES CAR ELLES NE DEPENDENT QUE DE L'ELREFA.
!
!        EXCEPTION : LA FAMILLE XFEM DES ELEMENTS XFEM EST UNE FAMILLE
!        SPECIALE DONT LA POSITION DES POINTS DEPEND DU DECOUPAGE DE
!        CHAQUE ELEMENT. POUR CETTE FAMILLE, ON NE PEUT PAS FAIRE
!        L'ECONOMIE DANS MNOGAZ.
!
!        ON UTILISE LA CONVENTION :
!          SI MNOGAZ(IMA,1) > 0 : LA MAILLE IMA EST LA 1ERE D'UN GREL
!              SA MATRICE EST STOCKEE ET ELLE SERT DE REFERENCE POUR
!              LES AUTRES
!          SI MNOGAZ(IMA,1) < 0 : LA MAILLE IMA N'EST PAS LA 1ERE
!              D'UN GREL. MNOGAZ(IMA,1)= -IMAREF
!              IMAREF EST LA MAILLE DE REFERENCE POUR IMA
!
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: nbpgmx, nbnomx, nbfamx, nbflmx
    parameter (nbpgmx=27,nbnomx=27,nbfamx=20,nbflmx=20)
    integer :: ibid, nbma, ima, jcesd, jcesl, jcesv, iad, jnbpg
    integer :: ilcnx1, nbpgf(nbfamx), k, jfpgl, jpnlfp
    integer :: nec, kfpg, ndim, nno, nnos, nbfpg, npg, kp, ino
    integer :: jceld, nbgrel, nel, nute, imolo, jmolo, jecono
    integer :: ierd, igr, iel, jmaref, lont1
    integer :: jnbno, jdime, iret, ncpmax, nbfam, kfam, nbpgt, iad0, iad1
    integer :: nblfpg, jnolfp, nuflpg, nufgpg, jliel, jliel1
    integer :: jcesgl, jcesgv, jcesgd, nbpt, nbsp
    character(len=1) :: kbid
    character(len=3) :: exixfm
    character(len=8) :: ma, fapg(nbfamx), nomgd, famil, elrefe, param
    character(len=8) :: lielrf(nbflmx), lifapg(nbflmx)
    character(len=16) :: pheno, option, nomte, nofpg, valk(2)
    character(len=19) :: mnoga, ligrel, celmod, ligre1, chsgeo
    character(len=24) :: obnbpg, obnbno, obdime, kecono
    character(len=32) :: noflpg
    real(kind=8) :: xno(3*nbnomx), xpg(3*nbpgmx), vol, ff(nbnomx)
    real(kind=8) :: poipg(nbnomx)
    logical :: econom
!     ------------------------------------------------------------------
    call jemarq()
!
    mnoga=mnogaz
    ligrel=ligrez
    option=optioz
    param=paramz
!
    obnbpg = '&&MANOPG.NBPG'
    obnbno = '&&MANOPG.NBNO'
    obdime = '&&MANOPG.DIME'
!
    call jelira(ligrel//'.LIEL', 'NMAXOC', nbgrel, kbid)
    call dismoi('F', 'NOM_MAILLA', ligrel, 'LIGREL', ibid,&
                ma, ibid)
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbma,&
                kbid, ibid)
    call dismoi('F', 'PHENOMENE', ligrel, 'LIGREL', ibid,&
                pheno, ibid)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
!
    call jeveuo('&CATA.TE.PNLOCFPG', 'L', jpnlfp)
    call jelira('&CATA.TE.NOLOCFPG', 'LONMAX', nblfpg, kbid)
    call jeveuo('&CATA.TE.NOLOCFPG', 'L', jnolfp)
!
!
!     0.1 CALCUL DE '&&MANOPG.ECONO' ET '&&MANOPG.CHSGEO'
!     ------------------------------------------------------------------
!     '.ECONO': V(IGR) = 1 => LE GREL IGR EST STOCKE 'ECONOMIQUE'
    kecono='&&MANOPG.ECONO'
    chsgeo='&&MANOPG.CHSGEO'
    call manopx(ligrel, option, param, chsgeo, exixfm,&
                kecono)
    call jeveuo(kecono, 'L', jecono)
    if (exixfm .eq. 'OUI') then
        call jeveuo(chsgeo//'.CESD', 'L', jcesgd)
        call jeveuo(chsgeo//'.CESV', 'L', jcesgv)
        call jeveuo(chsgeo//'.CESL', 'L', jcesgl)
    endif
!
!
!     0.2 ON FABRIQUE UN "FAUX" LIGREL (LIGRE1) N'AYANT QUE LE NOMBRE
!        NECESSAIRE D'ELEMENTS PAR GREL POUR DIMINUER LA TAILLE DE MNOGA
!     ------------------------------------------------------------------
!
!     '&&MANOPG.MAILREF': OBJET DONNANT POUR CHAQUE MAILLE SA MAILLE
!         DE REFERENCE
    call wkvect('&&MANOPG.MAILREF', 'V V I', nbma, jmaref)
!
    ligre1='&&MANOPG.LIGRE1'
    call assert(nbgrel.gt.0)
    call jecrec(ligre1//'.LIEL', 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbgrel)
    lont1=0
    do 883, igr=1,nbgrel
    econom=zi(jecono-1+igr).eq.1
    nel = nbelem(ligrel,igr)
    if (econom .and. nel .gt. 1) then
        lont1=lont1+2
    else
        lont1=lont1+nel+1
    endif
    883 end do
!
    call jeecra(ligre1//'.LIEL', 'LONT', lont1, ' ')
    do 881, igr=1,nbgrel
    econom=zi(jecono-1+igr).eq.1
    nel = nbelem(ligrel,igr)
    if (econom .and. nel .gt. 1) then
        call jecroc(jexnum(ligre1//'.LIEL', igr))
        call jeecra(jexnum(ligre1//'.LIEL', igr), 'LONMAX', 2, kbid)
        call jeveuo(jexnum(ligre1//'.LIEL', igr), 'E', jliel1)
        call jeveuo(jexnum(ligrel//'.LIEL', igr), 'L', jliel)
        zi(jliel1-1+1)=zi(jliel-1+1)
        zi(jliel1-1+2)=zi(jliel-1+nel+1)
        do 882, iel=1,nel
        ima=zi(jliel-1+iel)
        if (ima .lt. 0) goto 882
        if (iel .eq. 1) then
            zi(jmaref-1+ima)=+zi(jliel-1+1)
        else
            zi(jmaref-1+ima)=-zi(jliel-1+1)
        endif
882      continue
    else
        call jecroc(jexnum(ligre1//'.LIEL', igr))
        call jeecra(jexnum(ligre1//'.LIEL', igr), 'LONMAX', nel+1, kbid)
        call jeveuo(jexnum(ligre1//'.LIEL', igr), 'E', jliel1)
        call jeveuo(jexnum(ligrel//'.LIEL', igr), 'L', jliel)
        zi(jliel1-1+nel+1)=zi(jliel-1+nel+1)
        do 884, iel=1,nel
        zi(jliel1-1+iel)=zi(jliel-1+iel)
        ima=zi(jliel-1+iel)
        if (ima .lt. 0) goto 884
        zi(jmaref-1+ima)=+zi(jliel-1+iel)
884      continue
    endif
    881 end do
    call jedupo(ligrel//'.LGRF', 'V', ligre1//'.LGRF', .false.)
    call jedupo(ligrel//'.NBNO', 'V', ligre1//'.NBNO', .false.)
!
!
!
!     1. ON RECUPERE LES NOMBRES DE POINTS DE GAUSS ET DE NOEUDS :
!     ------------------------------------------------------------
    call nbptca(ligre1, option, param, obnbpg, obnbno)
    call jeveuo(obnbpg, 'L', jnbpg)
    call jeveuo(obnbno, 'L', jnbno)
!
!
!     2. ALLOCATION DU CHAM_ELEM_S MNOGA :
!     ---------------------------------------------------------------
    call wkvect(obdime, 'V V I', nbma, jdime)
    ncpmax=0
    do 77, ima=1,nbma
    zi(jdime-1+ima)= zi(jnbpg-1+ima)*zi(jnbno-1+ima) +2
    ncpmax=max(ncpmax,zi(jdime-1+ima))
    77 end do
    call cescre('V', mnoga, 'ELEM', ma, 'VARI_R',&
                -ncpmax, ' ', -1, -1, zi(jdime))
!
!
!     3. ALLOCATION D'UN CHAMP MODELE POUR DETERMINER LES FAMILLES
!        DE POINTS DE GAUSS UTILISEES.
!     ---------------------------------------------------------------
    celmod='&&MANOPG.CELMOD'
    call alchml(ligre1, option, param, 'V', celmod,&
                iret, ' ')
    if (iret .ne. 0) then
        valk(1) = param
        valk(2) = option
        call u2mesk('F', 'CALCULEL7_7', 2, valk)
    endif
    call jeveuo(celmod//'.CELD', 'L', jceld)
!
!
!     4. REMPLISSAGE DE MNOGA :
!     ---------------------------------------------------------------
    call jeveuo(mnoga//'.CESD', 'L', jcesd)
    call jeveuo(mnoga//'.CESL', 'E', jcesl)
    call jeveuo(mnoga//'.CESV', 'E', jcesv)
!
!
    do 1, igr=1,nbgrel
    econom=zi(jecono-1+igr).eq.1
    call jeveuo(jexnum(ligrel//'.LIEL', igr), 'L', jliel)
    nel = nbelem(ligrel,igr)
    nute = typele(ligrel,igr)
    call jenuno(jexnum('&CATA.TE.NOMTE', nute), nomte)
    imolo = zi(jceld-1+zi(jceld-1+4+igr)+2)
    if (imolo .eq. 0) goto 1
!
    call jeveuo(jexnum(ligre1//'.LIEL', igr), 'L', jliel1)
!
!
!       4.1 DETERMINATION DE LA LISTE DES FAMILLES DE PG :
!       -----------------------------------------------------------
!       => NBFAM, LIELRF, LIFAPG
    call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', jmolo)
    call dismoi('F', 'NOM_GD', celmod, 'CHAMP', ibid,&
                nomgd, ierd)
    call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nec,&
                kbid, ierd)
    kfpg = zi(jmolo-1+4+nec+1)
!
!       -- FAMILLE "LISTE"
    if (kfpg .lt. 0) then
!          FAMILLE "LISTE" :
        call jelira(jexnum('&CATA.TE.FPG_LISTE', -kfpg), 'LONMAX', nbfam, kbid)
        nbfam=nbfam-1
        call assert(nbfam.le.nbflmx)
        call jeveuo(jexnum('&CATA.TE.FPG_LISTE', -kfpg), 'L', jfpgl)
        elrefe=zk8(jfpgl-1+nbfam+1)
        do 18,k=1,nbfam
        lielrf(k)=elrefe
        noflpg = nomte//elrefe//zk8(jfpgl-1+k)
        nuflpg = indk32(zk32(jpnlfp),noflpg,1,nblfpg)
        nufgpg = zi(jnolfp-1+nuflpg)
        call jenuno(jexnum('&CATA.TM.NOFPG', nufgpg), nofpg)
        call assert(elrefe.eq.nofpg(1:8))
        lifapg(k)=nofpg(9:16)
18      continue
!
!       -- FAMILLE "ORDINAIRE"
    else
        nbfam=1
        call jenuno(jexnum('&CATA.TM.NOFPG', kfpg), nofpg)
        lielrf(1)=nofpg(1:8)
        lifapg(1)=nofpg(9:16)
    endif
!
!
!       4.2 BOUCLE SUR LA/LES FAMILLE(S) :
!       ------------------------------------------
    nbpgt=0
    do 2, kfam=1,nbfam
    elrefe=lielrf(kfam)
    famil=lifapg(kfam)
!
    call elraca(elrefe, ndim, nno, nnos, nbfpg,&
                fapg, nbpgf, xno, vol)
    call assert(nbfpg.le.nbfamx)
    call assert(nno.le.nbnomx)
!
!         4.2.1 CALCUL DE NPG ET XPG (SI FAMILLE != XFEM)
!         ------------------------------------------------
    if (famil(1:4) .ne. 'XFEM') then
        call elraga(elrefe, famil, ndim, npg, xpg,&
                    poipg)
        call assert(npg.le.nbpgmx)
!
!         4.2.2 CALCUL DE NPG (SI FAMILLE == 'XFEM...')
!         ------------------------------------------------
    else
        ima=zi(jliel-1+1)
        npg = zi(jcesgd-1+5+4* (ima-1)+1)
    endif
!
!         4.2.3 ECRITURE DANS MANOPG :
!         ------------------------------------------
    do 3,iel = 1,nel
    ima=zi(jliel-1+iel)
    if (ima .lt. 0) goto 3
!
    call cesexi('C', jcesd, jcesl, ima, 1,&
                1, 1, iad0)
    iad0=abs(iad0)
    call assert(iad0.gt.0)
!
!           -- SI CE N'EST PAS UNE MAILLE DE REFERENCE :
    if (zi(jmaref-1+ima) .lt. 0) then
        zl(jcesl-1+iad0-1+1) = .true.
        zr(jcesv-1+iad0-1+1) = zi(jmaref-1+ima)
        goto 3
    endif
    call assert(zi(jmaref-1+ima).eq.ima)
!
!
!           -- LES 2 PREMIERES CMPS : NNO ET NPG :
    if (kfam .eq. 1) then
        zl(jcesl-1+iad0-1+1) = .true.
        zl(jcesl-1+iad0-1+2) = .true.
        zr(jcesv-1+iad0-1+1) = nno
        zr(jcesv-1+iad0-1+2) = npg
    else
        call assert(nint(zr(jcesv-1+iad0-1+1)).eq.nno)
        zr(jcesv-1+iad0-1+2) = zr(jcesv-1+iad0-1+2) + npg
    endif
!
    call cesexi('C', jcesd, jcesl, ima, 1,&
                1, 2+nno*(nbpgt+npg), iad)
    call assert(iad.lt.0)
    iad=iad0+2+nbpgt*nno
!
!           -- LES NNO*NPG AUTRES CMPS :
    do 20 kp = 1, npg
        if (famil(1:4) .eq. 'XFEM') then
            call assert(.not.econom)
            nbpt = zi(jcesgd-1+5+4* (ima-1)+1)
            nbsp = zi(jcesgd-1+5+4* (ima-1)+2)
            call assert(nbpt.eq.npg)
            call assert(nbsp.eq.1)
            call cesexi('C', jcesgd, jcesgl, ima, kp,&
                        1, 1, iad1)
            call assert(iad1.gt.0)
            call elrfvf(elrefe, zr(jcesgv-1+iad1), nbnomx, ff, nno)
        else
            call elrfvf(elrefe, xpg(ndim*(kp-1)+1), nbnomx, ff, nno)
        endif
        do 10 ino = 1, nno
            zl(jcesl-1+iad-1+nno*(kp-1)+ino) = .true.
            zr(jcesv-1+iad-1+nno*(kp-1)+ino) = ff(ino)
10      continue
20  continue
!
 3  continue
    nbpgt=nbpgt+npg
 2  continue
    1 end do
!
!
    call detrsd('CHAMP', celmod)
    call detrsd('CHAMP', chsgeo)
    call detrsd('LIGREL', ligre1)
    call jedetr(obnbpg)
    call jedetr(obnbno)
    call jedetr(obdime)
    call jedetr('&&MANOPG.MAILREF')
    call jedetr(kecono)
!
    call jedema()
end subroutine
