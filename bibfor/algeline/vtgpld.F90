subroutine vtgpld(cumul, geomiz, alpha, deplaz, base,&
                  geomfz)
!
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit      none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/copisd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nbec.h'
    character(len=*) :: geomiz, deplaz, geomfz
    character(len=1) :: base
    real(kind=8) :: alpha
    character(len=4) :: cumul
!
! ----------------------------------------------------------------------
!
! REACTUALISATION D'UN CHAMP GEOMETRIQUE (GEOM_R) AVEC UN CHAMP SUR
! LA GRANDEUR DEPL_R
!
! ----------------------------------------------------------------------
!
! ON FAIT:
!   GEOMF = GEOMI + ALPHA * DEPLA SI CUMUL = 'CUMU'
!   GEOMF = ALPHA * DEPLA         SI CUMUL = 'ZERO'
!
! ON SE SERT UNIQIUEMENT DES COMPOSANTES DX, DY, DZ SUR LE CHAMP
! DE DEPLACEMENT
! SI SUR CERTAINS NOEUDS, ON NE TROUVE PAS DE DEPLACEMENT,
! ON LES LAISSE INCHANGES
!
! IN  CUMUL : 'ZERO' OU 'CUMU'
! IN  GEOMI : CHAM_NO(GEOM_R) - CHAMP DE GEOMETRIE A ACTUALISER.
! IN  ALPHA : COEFFICIENT MULTIPLICATEUR DE DEPLA
! IN  DEPLA : CHAM_NO(DEPL_R) - CHAMP DE DEPLACEMENT A AJOUTER.
! IN  BASE  : BASE SUR LAQUELLE DOIT ETRE CREE GEOMF
! OUT GEOMF : CHAM_NO(GEOM_R) : CHAMP DE GEOMETRIE ACTUALISE.
!                 (CE CHAMP EST DETRUIT S'IL EXISTE DEJA)
!
!
!
!
    integer :: iad, ibid, icompt, igd, ival, ldim, iret
    integer :: nbno, ncmp, ncmpmx, nec, num
    integer :: iadesc, iaprno, iarefe, iavald, iavalf, iavali
    integer :: icmp, ino
    real(kind=8) :: rdepla
    character(len=8) :: noma, nomgd, k8bid, ktype
    character(len=19) :: geomi, depla, geomf
    character(len=24) :: nomnu
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATION
!
    geomi = geomiz
    geomf = geomfz
    depla = deplaz
!
! --- CONFORMITE DES GRANDEURS
!
    call dismoi('F', 'NOM_GD', geomi, 'CHAM_NO', ibid,&
                nomgd, iret)
    call assert(nomgd(1:6).eq.'GEOM_R')
    call dismoi('F', 'NOM_GD', depla, 'CHAM_NO', ibid,&
                nomgd, iret)
    call assert(nomgd(1:6).eq.'DEPL_R')
    call jelira(depla//'.VALE', 'TYPE', ibid, ktype)
    call assert(ktype(1:1).eq.'R')
!
! --- ON RECOPIE BESTIALEMENT LE CHAMP POUR CREER LE NOUVEAU
!
    call copisd('CHAMP_GD', base, geomi, geomf)
!
! --- INFORMATIONS SUR LE MAILLAGE
!
    call dismoi('F', 'NOM_MAILLA', geomi, 'CHAM_NO', ibid,&
                noma, iret)
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbno,&
                k8bid, iret)
!
    call jelira(geomi//'.VALE', 'LONMAX', ldim, k8bid)
    call assert(ldim/3.eq.nbno)
!
! --- ACCES AUX CHAMPS
!
    call jeveuo(geomi//'.VALE', 'L', iavali)
    call jeveuo(geomf//'.VALE', 'E', iavalf)
    call jeveuo(depla//'.REFE', 'L', iarefe)
    call jeveuo(depla//'.VALE', 'L', iavald)
    call jeveuo(depla//'.DESC', 'L', iadesc)
!
! --- INFORMATIONS SUR LE CHAMP DE DEPLACEMENT
!
    nomnu = zk24(iarefe-1+2)
    igd = zi(iadesc-1+1)
    num = zi(iadesc-1+2)
    nec = nbec(igd)
    call assert(nec.le.10)
    call jelira(jexnum('&CATA.GD.NOMCMP', igd), 'LONMAX', ncmpmx, k8bid)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', igd), 'L', iad)
    call assert(zk8(iad-1+1).eq.'DX')
    call assert(zk8(iad-1+2).eq.'DY')
    call assert(zk8(iad-1+3).eq.'DZ')
!
! --- SI LE CHAMP EST A REPRESENTATION CONSTANTE
!
    if (num .lt. 0) then
        do 13 ino = 1, nbno
            do 14 icmp = 1, 3
                rdepla = zr(iavald-1+3*(ino-1)+icmp)
                if (cumul .eq. 'CUMU') then
                    zr(iavalf-1+3*(ino-1)+icmp) = zr(iavali-1+3*(ino- 1)+icmp )+alpha*rdepla
                else if (cumul.eq.'ZERO') then
                    zr(iavalf-1+3*(ino-1)+icmp) = alpha*rdepla
                else
                    call assert(.false.)
                endif
14          continue
13      continue
    else
!
! --- ON RECUPERE CE QUI CONCERNE LES NOEUDS DU MAILLAGE
!
        call jelira(jexnum(nomnu(1:19)//'.PRNO', 1), 'LONMAX', ibid, k8bid)
        call assert(ibid.ne.0)
        call jeveuo(jexnum(nomnu(1:19)//'.PRNO', 1), 'L', iaprno)
        do 11 ino = 1, nbno
            ival = zi(iaprno-1+(ino-1)*(nec+2)+1)
            ncmp = zi(iaprno-1+(ino-1)*(nec+2)+2)
            if (ncmp .ne. 0) then
                icompt = 0
                do 12 icmp = 1, 3
                    if (exisdg(zi(iaprno-1+(ino-1)*(nec+2)+3),icmp)) then
                        icompt = icompt + 1
                        rdepla = zr(iavald-1+ival-1+icompt)
                        if (cumul .eq. 'CUMU') then
                            zr(iavalf-1+3*(ino-1)+icmp)= zr(iavali-1+&
                            3*(ino-1)+icmp)+alpha*rdepla
                        else if (cumul.eq.'ZERO') then
                            zr(iavalf-1+3*(ino-1)+icmp)= alpha*rdepla
                        else
                            call assert(.false.)
                        endif
                    endif
12              continue
            endif
11      continue
    endif
!
    call jedema()
end subroutine
