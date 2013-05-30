subroutine cafotu(char, ligrmo, ialloc, noma, fonree)
    implicit   none
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/alcart.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nocart.h'
    include 'asterfort/reliem.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    integer :: ialloc
    character(len=4) :: fonree
    character(len=8) :: char, noma
    character(len=*) :: ligrmo
!-----------------------------------------------------------------------
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
! BUT : STOCKAGE DE FORCE_TUYAU(PRES) DANS UNE CARTE ALLOUEE SUR LE
!       LIGREL DU MODELE
!
! ARGUMENTS D'ENTREE:
!      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
!      LIGRMO : NOM DU LIGREL DE MODELE
!      IALLOC : 1 SI LA CARTE DE PRESSION ALLOUE PAR CAPRES, 0 SINON
!      NOMA   : NOM DU MAILLAGE
!      FONREE : FONC OU REEL
!
!-----------------------------------------------------------------------
    integer :: npres, jncmp, jvalv, ncmp, iocc, npr, iatyma, nbma, i, ima
    integer :: iadtyp, jma, ibid, nmatot, nbtou
    character(len=8) :: k8b, maille, type, typmcl(2)
    character(len=16) :: motclf, motcle(2)
    character(len=19) :: carte
    character(len=24) :: mesmai, valk(4)
    integer :: iarg
!-----------------------------------------------------------------------
!
    call jemarq()
!
    motclf = 'FORCE_TUYAU'
    call getfac(motclf, npres)
!
    carte = char//'.CHME.PRESS'
    if (ialloc .eq. 0) then
        if (fonree .eq. 'REEL') then
            call alcart('G', carte, noma, 'PRES_R')
        else if (fonree.eq.'FONC') then
            call alcart('G', carte, noma, 'PRES_F')
        else
            call u2mesk('F', 'MODELISA2_37', 1, fonree)
        endif
    endif
!
    call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nmatot,&
                k8b, ibid)
!
    call jeveuo(carte//'.NCMP', 'E', jncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
!
! --- STOCKAGE DE PRESSIONS NULLES SUR TOUT LE MAILLAGE
!
    ncmp = 1
    zk8(jncmp) = 'PRES'
    if (ialloc .eq. 0) then
        if (fonree .eq. 'REEL') then
            zr(jvalv) = 0.d0
        else
            zk8(jvalv) = '&FOZERO'
        endif
        call nocart(carte, 1, ' ', 'NOM', 0,&
                    ' ', 0, ligrmo, ncmp)
    endif
!
    mesmai = '&&CAFOTU.MES_MAILLES'
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
! --- STOCKAGE DANS LA CARTE
!
    do 10 iocc = 1, npres
!
        if (fonree .eq. 'REEL') then
            call getvr8(motclf, 'PRES', iocc, iarg, 1,&
                        zr(jvalv), npr)
        else
            call getvid(motclf, 'PRES', iocc, iarg, 1,&
                        zk8(jvalv), npr)
        endif
!
        call getvtx(motclf, 'TOUT', iocc, iarg, 1,&
                    k8b, nbtou)
!
        if (nbtou .ne. 0) then
            do 12 ima = 1, nmatot
                iadtyp = iatyma-1+ima
                call jenuno(jexnum('&CATA.TM.NOMTM', zi(iadtyp)), type)
                if ((type(1:4).ne.'SEG3') .and. (type(1:4).ne.'SEG4')) then
                    call jenuno(jexnum(noma//'.NOMMAI', ima), maille)
                    valk(1) = maille
                    valk(2) = motclf
                    call u2mesg('A', 'MODELISA9_81', 2, valk, 0,&
                                0, 0, 0.d0)
                endif
12          continue
            call nocart(carte, 1, ' ', 'NOM', 0,&
                        ' ', 0, ligrmo, ncmp)
!
        else
            call reliem(ligrmo, noma, 'NU_MAILLE', motclf, iocc,&
                        2, motcle, typmcl, mesmai, nbma)
            if (nbma .eq. 0) goto 10
            call jeveuo(mesmai, 'L', jma)
            do 14 i = 1, nbma
                ima = zi(jma-1+i)
                iadtyp = iatyma-1+ima
                call jenuno(jexnum('&CATA.TM.NOMTM', zi(iadtyp)), type)
                if ((type(1:4).ne.'SEG3') .and. (type(1:4).ne.'SEG4')) then
                    call jenuno(jexnum(noma//'.NOMMAI', ima), maille)
                    valk(1) = maille
                    valk(2) = motclf
                    call u2mesg('A', 'MODELISA9_81', 2, valk, 0,&
                                0, 0, 0.d0)
                endif
14          continue
            call nocart(carte, 3, k8b, 'NUM', nbma,&
                        k8b, zi(jma), ' ', ncmp)
            call jedetr(mesmai)
        endif
!
10  end do
!
    call jedetr(char//'.PRES.GROUP')
    call jedetr(char//'.PRES.LISTE')
!-----------------------------------------------------------------------
    call jedema()
end subroutine
