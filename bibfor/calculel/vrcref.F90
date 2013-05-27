subroutine vrcref(modele, chmat, carele, chvref)
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
    implicit   none
    include 'jeveux.h'
    include 'asterfort/alchml.h'
    include 'asterfort/assert.h'
    include 'asterfort/carces.h'
    include 'asterfort/celces.h'
    include 'asterfort/cescel.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/cesvar.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exisd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/juvinn.h'
    include 'asterfort/u2mesk.h'
    character(len=8) :: modele, chmat, carele
    character(len=19) :: chvref
! ======================================================================
!   BUT : FABRIQUER LE CHAMP DE VARIABLES DE COMMANDE DE "REFERENCE"
!   ARGUMENTS :
!   MODELE (K8)  IN/JXIN : SD MODELE
!   CHMAT  (K8)  IN/JXIN : SD CHAM_MATER
!   CARELE (K8)  IN/JXIN : SD CARA_ELEM (SOUS-POINTS)
!   CHVREF (K19) IN/JXOUT: SD CHAM_ELEM/ELGA (VARC DE "REFERENCE")
! ----------------------------------------------------------------------
!
!
    character(len=8) :: models, chmats, carels
    character(len=19) :: chvres
    integer :: n1, iad, isp, ipt, jcesv
    integer :: k, k2, nbma, ncmp, icmp, jcesl1, jcesv1, jcesd1
    integer :: jcesd, jcesl, ima, nbpt, nbsp, nbcvrc, jcvvar, ibid
    integer :: jdcld, jdcll, jdclv, nncp, iret, jcvrc
    character(len=8) :: kbid, varc, noma1, noma2
    character(len=19) :: dceli, celmod, cart1, ces1, ligrmo, csvref
    character(len=24) :: valk(4)
    real(kind=8) :: valref
    logical :: avrc
    save models,chmats,carels,chvres
    data models/' '/,chmats/' '/,carels/' '/,chvres/' '/
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     -- SI LE CHAMP A PRODUIRE EXISTE DEJA ET QUE LES ARGUMENTS
!        SONT LES MEMES QUE LA FOIS PRECEDENTE, ON SORT RAPIDEMENT :
    call exisd('CHAM_ELEM', chvref, iret)
    if (iret .gt. 0) then
        if (modele .ne. models) goto 5
        if (chmat .ne. chmats) goto 5
        if (carele .ne. carels) goto 5
        if (chvref .ne. chvres) goto 5
        goto 9999
    endif
 5  continue
!     -- SAUVERGARDE DES ARGUMENTS POUR LE PROCHAIN APPEL
    models=modele
    chmats=chmat
    carels=carele
    chvres=chvref
!
!
!     -- ON SE PROTEGE DES MODELES QUI NE CONNAISSENT PAS LES VRC :
    celmod='&&VRCREF.CELMOD'
    ligrmo=modele//'.MODELE'
    call jeexin(ligrmo//'.LIEL', iret)
    if (iret .eq. 0) goto 9999
    call alchml(ligrmo, 'INIT_VARC', 'PVARCPR', 'V', celmod,&
                iret, ' ')
    call detrsd('CHAMP', celmod)
    if (iret .eq. 1) goto 9999
!
!
    call jeexin(chmat//'.CVRCVARC', iret)
!     AVRC : .TRUE. SI AFFE_MATERIAU/AFFE_VARC EST UTILISE
    avrc=.false.
    if (iret .gt. 0) then
        call jeveuo(chmat//'.CVRCVARC', 'L', jcvrc)
        call jelira(chmat//'.CVRCVARC', 'LONMAX', nbcvrc, kbid)
        do 10 k = 1, nbcvrc
            if (zk8(jcvrc-1+k) .ne. ' ') then
                avrc=.true.
                goto 11
            endif
10      continue
!
11      continue
    endif
!
!
!
!     -- CAS : PAS DE AFFE_VARC :
!     ------------------------------------------
    if (.not.avrc) goto 9999
!
!     -- CAS AFFE_VARC  :
!     ------------------------
    call jeveuo(chmat//'.CVRCVARC', 'L', jcvvar)
    call jelira(chmat//'.CVRCVARC', 'LONMAX', nbcvrc, kbid)
!
!
!     0. VERIFICATION DE LA COHERENCE DE MODELE  ET CHMAT (FICHE 19507)
!     ------------------------------------------------------------------
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                noma1, ibid)
    call dismoi('F', 'NOM_MAILLA', chmat//'.CHAMP_MAT', 'CHAMP', ibid,&
                noma2, ibid)
    if (noma1 .ne. noma2) then
        valk(1)=noma1
        valk(2)=noma2
        call u2mesk('F', 'CALCULEL4_23', 2, valk)
    endif
!
!
!     1. ALLOCATION DE CSVREF
!     ------------------------------------------
    dceli='&&VRCREF.DCELI'
    csvref='&&VRCREF.CSVREF'
    call cesvar(carele, ' ', ligrmo, dceli)
!
!     -- MODIFICATION DE DCELI : TOUTES LES MAILLES ONT
!        NBCVRC COMPOSANTES.
    call jeveuo(dceli//'.CESD', 'L', jdcld)
    call jeveuo(dceli//'.CESL', 'L', jdcll)
    call jeveuo(dceli//'.CESV', 'E', jdclv)
    nbma = zi(jdcld-1+1)
!
    do 170,ima = 1,nbma
    nbpt = zi(jdcld-1+5+4* (ima-1)+1)
    nbsp = max(1,zi(jdcld-1+5+4* (ima-1)+2))
    call assert(nbpt.eq.1)
    call assert(nbsp.eq.1)
    call cesexi('C', jdcld, jdcll, ima, 1,&
                1, 2, iad)
    if (iad .gt. 0) zi(jdclv-1+iad)=nbcvrc
    170 end do
!
    call alchml(ligrmo, 'INIT_VARC', 'PVARCPR', 'V', celmod,&
                iret, dceli)
    call assert(iret.eq.0)
    call detrsd('CHAMP', dceli)
    call celces(celmod, 'V', csvref)
    call detrsd('CHAMP', celmod)
!
    call jelira(csvref//'.CESV', 'LONMAX', n1, kbid)
!
    call jeveuo(csvref//'.CESD', 'L', jcesd)
    call jeveuo(csvref//'.CESL', 'E', jcesl)
    call jeveuo(csvref//'.CESV', 'E', jcesv)
    call jelira(csvref//'.CESL', 'LONMAX', n1, kbid)
    do 777, k=1,n1
    zl(jcesl-1+k)=.false.
    777 end do
!
!
!
!     2. REMPLISSAGE DE CSVREF.CESV :
!     ------------------------------------------
    varc=' '
    do 1, k=1,nbcvrc
    if (zk8(jcvvar-1+k) .eq. varc) goto 1
    varc=zk8(jcvvar-1+k)
    cart1 = chmat//'.'//varc//'.1'
    ces1='&&VRCREF.CES1'
    call carces(cart1, 'ELEM', ' ', 'V', ces1,&
                'A', iret)
    call assert(iret.eq.0)
!
    call jeveuo(ces1//'.CESD', 'L', jcesd1)
    call jeveuo(ces1//'.CESV', 'L', jcesv1)
    call jeveuo(ces1//'.CESL', 'L', jcesl1)
!
    nbma = zi(jcesd-1+1)
    call assert(nbma.eq.zi(jcesd1-1+1))
!
!       -- CALCUL DE NCMP
    ncmp=0
    do 69, k2=k,nbcvrc
    if (zk8(jcvvar-1+k2) .eq. varc) ncmp=ncmp+1
69  continue
!
    do 70,ima = 1,nbma
    nbpt = zi(jcesd-1+5+4* (ima-1)+1)
    nbsp = max(1,zi(jcesd-1+5+4* (ima-1)+2))
!
    call cesexi('C', jcesd1, jcesl1, ima, 1,&
                1, 1, iad)
    if (iad .le. 0) goto 70
    valref=zr(jcesv1-1+iad)
!
    do 60,ipt = 1,nbpt
    do 50,isp = 1,nbsp
    do 51,icmp = 1,ncmp
    call cesexi('C', jcesd, jcesl, ima, ipt,&
                isp, k-1+ icmp, iad)
    call assert(iad.le.0)
    if (iad .eq. 0) goto 51
    iad=-iad
    zl(jcesl-1+iad)=.true.
    zr(jcesv-1+iad)=valref
51  continue
50  continue
60  continue
70  continue
    call detrsd('CHAMP', ces1)
    1 end do
!
!
!     3. RECOPIE DU CHAMP SIMPLE DANS LE CHAMP CHVREF
!     -----------------------------------------------------
!
!     LE CHAMP DE TEMP_REF PEUT CONTENIR DES VALEURS "VIDES"
!     ON LES TRANSFORME EN "NAN"
    call juvinn(csvref//'.CESV')
    call cescel(csvref, ligrmo, 'INIT_VARC', 'PVARCPR', 'NAN',&
                nncp, 'V', chvref, 'F', ibid)
    call detrsd('CHAM_ELEM_S', csvref)
!
9999  continue
    call jedema()
end subroutine
