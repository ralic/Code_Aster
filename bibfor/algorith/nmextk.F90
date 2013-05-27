subroutine nmextk(noma, motfac, iocc, champ, nomcha,&
                  nomchs, typcha, listno, listma, listpi,&
                  listsp, nbno, nbma, nbpi, nbspi,&
                  listcp, nbcmp)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include 'jeveux.h'
!
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/lxliis.h'
    include 'asterfort/posddl.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: noma
    character(len=16) :: motfac
    integer :: iocc
    character(len=4) :: typcha
    integer :: nbno, nbma, nbcmp
    character(len=24) :: nomcha, nomchs
    character(len=19) :: champ
    character(len=24) :: listpi, listsp
    integer :: nbpi, nbspi
    character(len=24) :: listcp, listno, listma
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (EXTRACTION - LECTURE)
!
! LECTURE COMPOSANTE DU CHAMP
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  MOTFAC : MOT-FACTEUR POUR LIRE
! IN  IOCC   : OCCURRENCE DU MOT-CLEF FACTEUR MOTFAC
! IN  NOMCHA : NOM DU CHAMP
! IN  TYPCHA : TYPE DU CHAMP 'NOEU' OU 'ELGA'
! IN  LISTNO : LISTE CONTENANT LES NOEUDS
! IN  LISTMA : LISTE CONTENANT LES MAILLES
! IN  NBNO   : LONGUEUR DE LA LISTE DES NOEUDS
! IN  NBMA   : LONGUEUR DE LA LISTE DES MAILLES
! IN  CHAMP  : CHAMP EXEMPLE POUR VERIF COMPOSANTE
! IN  LISTPI : LISTE CONTENANT LES POINTS D'EXTRACTION
! IN  LISTSP : LISTE CONTENANT LES SOUS-POINTS D'EXTRACTION
! OUT LISTCP : LISTE DES COMPOSANTES
! OUT NBCMP  : NOMBRE DE COMPOSANTES
!
!
!
!
    integer :: nparx
    parameter    (nparx=20)
    character(len=8) :: k8bid
    integer :: n1
    integer :: iret
    integer :: jcmp, jno, jma, jpi, jspi, iad
    integer :: ino, ima, icmp, ipi, ispi, ipar, i
    integer :: nbcmpx, nuno, nuddl
    integer :: nmapt, nmaspt, npi, nspi
    integer :: numnoe, nummai, num, snum
    character(len=8) :: nomnoe, nommai, nomcmp
    character(len=8) :: nomvar
    integer :: ivari
    character(len=16) :: valk(2)
    integer :: jcesd, jcesl, jcesv, jcesc
    integer :: vali(4)
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nbcmp = 0
!
! --- RECUP DU CHAM_ELEM_S
!
    if (typcha .eq. 'ELGA') then
        call jeveuo(nomchs(1:19)//'.CESD', 'L', jcesd)
        call jeveuo(nomchs(1:19)//'.CESL', 'L', jcesl)
        call jeveuo(nomchs(1:19)//'.CESV', 'L', jcesv)
        call jeveuo(nomchs(1:19)//'.CESC', 'L', jcesc)
        nbcmpx = zi(jcesd+4)
    endif
!
! --- LECTURE DES COMPOSANTES : IL FAUT AU MOINS UNE COMPOSANTE MAIS
! --- MOINS DE NPARX
!
    call getvtx(motfac, 'NOM_CMP', iocc, iarg, 0,&
                k8bid, n1)
    nbcmp = -n1
    if ((nbcmp.lt.1) .or. (nbcmp.gt.nparx)) then
        vali(1) = nparx
        vali(2) = nbcmp
        call u2mesi('F', 'EXTRACTION_12', 2, vali)
    endif
!
! --- LECTURE EFFECTIVE DES NOMS DE COMPOSANTES
!
    call wkvect(listcp, 'V V K8', nbcmp, jcmp)
    call getvtx(motfac, 'NOM_CMP', iocc, iarg, nbcmp,&
                zk8(jcmp), iret)
!
! --- VERIFICATION QUE LES NOEUDS SUPPORTENT LES COMPOSANTES FOURNIES
!
    if (typcha .eq. 'NOEU') then
        call jeveuo(listno, 'L', jno)
        do 20 ino = 1, nbno
            numnoe = zi(jno-1+ino)
            call jenuno(jexnum(noma(1:8)//'.NOMNOE', numnoe), nomnoe)
            do 21 icmp = 1, nbcmp
                nomcmp = zk8(jcmp-1+icmp)
                call posddl('CHAM_NO', champ, nomnoe, nomcmp, nuno,&
                            nuddl)
                if ((nuno.eq.0) .or. (nuddl.eq.0)) then
                    valk(1) = nomnoe
                    valk(2) = nomcmp
                    call u2mesk('F', 'EXTRACTION_20', 2, valk)
                endif
21          continue
20      continue
!
! --- VERIFICATION QUE LES ELEMENTS SUPPORTENT LES COMPOSANTES FOURNIES
!
    else if (typcha.eq.'ELGA') then
        call jeveuo(listma, 'L', jma)
        call jeveuo(listpi, 'L', jpi)
        call jeveuo(listsp, 'L', jspi)
        do 30 ima = 1, nbma
!
! ------- PROPRIETES DE LA MAILLE
!
            nummai = zi(jma-1+ima)
            call jenuno(jexnum(noma(1:8)//'.NOMMAI', nummai), nommai)
!
! ------- NOMBRE EFFECTIF DE POINTS/SOUS-POINTS SUR LA MAILLE
!
            nmapt = zi(jcesd+5+4*(nummai-1))
            nmaspt = zi(jcesd+5+4*(nummai-1)+1)
!
! ------- PLAFONNEMENT
!
            npi = nbpi
            nspi = nbspi
            if (npi .gt. nmapt) npi = nmapt
            if (nspi .gt. nmaspt) nspi = nmaspt
!
            do 31 ipar = 1, nbcmp
                nomcmp = zk8(jcmp-1+ipar)
                if (nomcha(1:4) .eq. 'VARI') then
                    nomvar = nomcmp(2:8)//' '
                    call lxliis(nomvar, ivari, iret)
                else
                    ivari = 0
                endif
                nbcmpx=zi(jcesd+4)
                if (nomcha(1:4) .eq. 'VARI') then
                    icmp = ivari
                else
                    do 32 i = 1, nbcmpx
                        if (nomcmp .eq. zk8(jcesc-1+i)) icmp=i
32                  continue
                endif
                do 45 ipi = 1, npi
                    num = zi(jpi-1+ipi )
                    call assert(num.ne.0)
                    do 46 ispi = 1, nspi
                        snum = zi(jspi-1+ispi )
                        call assert(snum.ne.0)
                        call cesexi('C', jcesd, jcesl, nummai, num,&
                                    snum, icmp, iad)
                        if (iad .eq. 0) then
                            valk(1) = nommai
                            valk(2) = nomcmp
                            vali(1) = num
                            vali(2) = snum
                            call u2mesg('F', 'EXTRACTION_21', 2, valk, 2,&
                                        vali, 0, 0.d0)
                        endif
46                  continue
45              continue
31          continue
30      continue
    else
        call assert(.false.)
    endif
!
    call jedema()
!
end subroutine
