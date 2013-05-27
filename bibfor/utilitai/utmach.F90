subroutine utmach(champz, ncmp, nocmp, typemz, litroz,&
                  nbtrou)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/carces.h'
    include 'asterfort/celces.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/cesred.h'
    include 'asterfort/cnocns.h'
    include 'asterfort/cnsred.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: nbtrou, ncmp
    character(len=8) :: nocmp(*)
    character(len=*) :: champz, typemz, litroz
!     ------------------------------------------------------------------
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
!
!     CETTE ROUTINE PERMET DE CREER UN OBJET JEVEUX CONTENANT UNE LISTE
!     DE NOMS OU NUMEROS DE MAILLES OU DE NOEUDS CORRESPONDANT AUX
!     MOTS-CLES TRANSMIS EN ARGUMENTS.
!
! IN  : CHAMP  : NOM D'UN CHAMP
! IN  : NCMP   : NOMBRE DE COMPOSANTES DE NOCMP
! IN  : NOCMP  : LISTE DES COMPOSANTES
! IN  : TYPEM  : PRECISE LE TYPE DE LISTE QUE L'ON VEUT RECUPERER
!              : 'NU'  : NUMEROS DE MAILLES OU DE NOEUDS
!              : 'NO'  : NOMS    DE MAILLES OU DE NOEUDS
! IN/JXOUT : LITROZ : NOM DE L'OBJET JEVEUX QUI CONTIENDRA LA LISTE DES
!                     ENTITES (MAILLE OU NOEUD) TROUVEES
! OUT : NBTROU : NOMBRE D'ENTITES TROUVEES
!     ------------------------------------------------------------------
!
    integer :: ibid, ierd, jcesd, jcesk, jcesl, nbent, jent, i, nbpt, nbsp, ipt
    integer :: isp, icp, iad, idlist, icmp, ncmpmx, gd, ier
    character(len=2) :: typem
    character(len=4) :: docu
    character(len=8) :: k8b, nomgd
    character(len=19) :: champ, chtra1, chtra2
    character(len=24) :: litrou, nomobj
    character(len=24) :: valk(5)
! DEB ------------------------------------------------------------------
    call jemarq()
!
    litrou = litroz
    champ = champz
    typem = typemz
!
    nbtrou = 0
    if (ncmp .eq. 0) goto 9999
!
    chtra1 = '&&UTMACH.CHAMP_COP'
    chtra2 = '&&UTMACH.CHAMP_RED'
!
    call dismoi('F', 'TYPE_CHAMP', champ, 'CHAMP', ibid,&
                docu, ierd)
    call dismoi('F', 'NUM_GD', champ, 'CHAMP', gd,&
                k8b, ierd)
    call jenuno(jexnum('&CATA.GD.NOMGD', gd), nomgd)
    if (nomgd(1:6) .eq. 'VARI_R') goto 9999
!
! --- VERIFICATION QUE LES COMPOSANTES SONT DANS LE CHAMP
!
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx, k8b)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iad)
    ier = 0
    do 2 icp = 1, ncmp
        do 4 icmp = 1, ncmpmx
            if (nocmp(icp) .eq. zk8(iad+icmp-1)) goto 2
 4      continue
        ier = ier + 1
        call u2mesk('E', 'UTILITAI5_48', 1, nocmp(icp))
 2  end do
    if (ier .ne. 0) call u2mess('F', 'PREPOST_60')
!
!
    if (docu .eq. 'ELGA' .or. docu .eq. 'ELNO' .or. docu .eq. 'ELEM' .or. docu .eq. 'CART') then
!          ----------------
        if (docu .eq. 'CART') then
            call carces(champ, 'ELEM', k8b, 'V', chtra1,&
                        'A', ierd)
        else
            call celces(champ, 'V', chtra1)
        endif
        call cesred(chtra1, 0, ibid, ncmp, nocmp,&
                    'V', chtra2)
        call jeveuo(chtra2//'.CESD', 'L', jcesd)
        call jeveuo(chtra2//'.CESK', 'L', jcesk)
        call jeveuo(chtra2//'.CESL', 'L', jcesl)
        nomobj = zk8(jcesk-1+1)//'.NOMMAI         '
        nbent = zi(jcesd-1+1)
        call wkvect('&&UTMACH.LIST_ENT', 'V V I', nbent, jent)
        do 10 i = 1, nbent
            nbpt = zi(jcesd-1+5+4* (i-1)+1)
            nbsp = zi(jcesd-1+5+4* (i-1)+2)
            do 12 ipt = 1, nbpt
                do 14 isp = 1, nbsp
                    do 16 icp = 1, ncmp
                        call cesexi('C', jcesd, jcesl, i, ipt,&
                                    isp, icp, iad)
                        if (iad .gt. 0) then
                            zi(jent+i-1) = 1
                            goto 10
                        else
                        endif
16                  continue
14              continue
12          continue
10      continue
        call detrsd('CHAM_ELEM_S', chtra1)
        call detrsd('CHAM_ELEM_S', chtra2)
!
!
    else if (docu .eq. 'NOEU') then
!              ----------------
        call cnocns(champ, 'V', chtra1)
        call cnsred(chtra1, 0, ibid, ncmp, nocmp,&
                    'V', chtra2)
        call jeveuo(chtra2//'.CNSD', 'L', jcesd)
        call jeveuo(chtra2//'.CNSK', 'L', jcesk)
        call jeveuo(chtra2//'.CNSL', 'L', jcesl)
        nomobj = zk8(jcesk-1+1)//'.NOMNOE         '
        nbent = zi(jcesd-1+1)
        call wkvect('&&UTMACH.LIST_ENT', 'V V I', nbent, jent)
        do 20 i = 1, nbent
            do 22 icp = 1, ncmp
                if (zl(jcesl-1+(i-1)*ncmp+icp)) then
                    zi(jent+i-1) = 1
                    goto 20
                endif
22          continue
20      continue
        call detrsd('CHAM_NO_S', chtra1)
        call detrsd('CHAM_NO_S', chtra2)
!
    else
!
        call u2mesk('F', 'UTILITAI5_49', 1, docu)
!
    endif
!
    if (nbent .eq. 0) then
        valk (1) = champ
        valk (2) = nocmp(1)
        valk (3) = nocmp(2)
        valk (4) = nocmp(3)
        valk (5) = nocmp(4)
        if (docu .eq. 'NOEU') then
            call u2mesg('F', 'UTILITAI8_61', 5, valk, 0,&
                        0, 0, 0.d0)
        else
            call u2mesg('F', 'UTILITAI8_62', 5, valk, 0,&
                        0, 0, 0.d0)
        endif
    endif
!
    nbtrou = 0
    do 100 i = 1, nbent
        if (zi(jent+i-1) .eq. 1) nbtrou = nbtrou + 1
100  end do
!
    if (typem .eq. 'NU') then
!          ---------------
        call wkvect(litrou, 'V V I', nbtrou, idlist)
        nbtrou = 0
        do 110 i = 1, nbent
            if (zi(jent+i-1) .eq. 1) then
                nbtrou = nbtrou + 1
                zi(idlist+nbtrou-1) = i
            endif
110      continue
!
    else if (typem .eq. 'NO') then
!              ---------------
        call wkvect(litrou, 'V V K8', nbtrou, idlist)
        nbtrou = 0
        do 120 i = 1, nbent
            if (zi(jent+i-1) .eq. 1) then
                nbtrou = nbtrou + 1
                call jenuno(jexnum(nomobj, zi(jent+i-1)), zk8(idlist+ nbtrou-1))
            endif
120      continue
!
    else
        call u2mesk('F', 'PREPOST3_6', 1, typem)
    endif
!
    call jedetr('&&UTMACH.LIST_ENT')
!
9999  continue
!
    call jedema()
end subroutine
