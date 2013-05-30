subroutine cagrai(char, ligrmo, noma, fonree)
    implicit   none
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/alcart.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/nocart.h'
    include 'asterfort/reliem.h'
    include 'asterfort/u2mesk.h'
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
! BUT : STOCKAGE DES CHARGES DES GRADIENTS INITIAUX DE TEMPERAT REPARTIS
!       DANS UNE CARTE ALLOUEE SUR LE LIGREL DU MODELE
!
! ARGUMENTS D'ENTREE:
!      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
!      LIGRMO : NOM DU LIGREL DE MODELE
!      NOMA   : NOM DU MAILLAGE
!      FONREE : FONC OU REEL
!-----------------------------------------------------------------------
    integer :: nchgi, jvalv, jncmp, nx, ny, nz, i, iocc, nbtou, nbma, jma
    real(kind=8) :: grx, gry, grz
    character(len=8) :: k8b, typmcl(2), grxf, gryf, grzf
    character(len=16) :: motclf, motcle(2)
    character(len=19) :: carte
    character(len=24) :: mesmai
    integer :: iarg
!     ------------------------------------------------------------------
    call jemarq()
!
    motclf = 'PRE_GRAD_TEMP'
    call getfac(motclf, nchgi)
!
    carte = char//'.CHTH.'//'GRAIN'
!
    if (fonree .eq. 'REEL') then
        call alcart('G', carte, noma, 'FLUX_R')
    else if (fonree.eq.'FONC') then
        call alcart('G', carte, noma, 'FLUX_F')
    else
        call u2mesk('F', 'MODELISA2_37', 1, fonree)
    endif
!
    call jeveuo(carte//'.NCMP', 'E', jncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
!
    zk8(jncmp-1+1) = 'FLUX'
    zk8(jncmp-1+2) = 'FLUY'
    zk8(jncmp-1+3) = 'FLUZ'
    if (fonree .eq. 'REEL') then
        do 10 i = 1, 3
            zr(jvalv-1+i) = 0.d0
10      continue
    else if (fonree.eq.'FONC') then
        do 12 i = 1, 3
            zk8(jvalv-1+i) = '&FOZERO'
12      continue
    endif
    call nocart(carte, 1, ' ', 'NOM', 0,&
                ' ', 0, ligrmo, 3)
!
    mesmai = '&&CAGRAI.MES_MAILLES'
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
    do 20 iocc = 1, nchgi
!
        if (fonree .eq. 'REEL') then
            call getvr8(motclf, 'FLUX_X', iocc, iarg, 1,&
                        grx, nx)
            call getvr8(motclf, 'FLUX_Y', iocc, iarg, 1,&
                        gry, ny)
            call getvr8(motclf, 'FLUX_Z', iocc, iarg, 1,&
                        grz, nz)
            do 22 i = 1, 3
                zr(jvalv-1+i) = 0.d0
22          continue
            if (nx .ne. 0) zr(jvalv-1+1) = grx
            if (ny .ne. 0) zr(jvalv-1+2) = gry
            if (nz .ne. 0) zr(jvalv-1+3) = grz
        else if (fonree.eq.'FONC') then
            call getvid(motclf, 'FLUX_X', iocc, iarg, 1,&
                        grxf, nx)
            call getvid(motclf, 'FLUX_Y', iocc, iarg, 1,&
                        gryf, ny)
            call getvid(motclf, 'FLUX_Z', iocc, iarg, 1,&
                        grzf, nz)
            do 24 i = 1, 3
                zk8(jvalv-1+i) = '&FOZERO'
24          continue
            if (nx .ne. 0) zk8(jvalv-1+1) = grxf
            if (ny .ne. 0) zk8(jvalv-1+2) = gryf
            if (nz .ne. 0) zk8(jvalv-1+3) = grzf
        endif
!
        call getvtx(motclf, 'TOUT', iocc, iarg, 1,&
                    k8b, nbtou)
        if (nbtou .ne. 0) then
            call nocart(carte, 1, ' ', 'NOM', 0,&
                        ' ', 0, ligrmo, 3)
!
        else
            call reliem(ligrmo, noma, 'NU_MAILLE', motclf, iocc,&
                        2, motcle, typmcl, mesmai, nbma)
            if (nbma .eq. 0) goto 20
            call jeveuo(mesmai, 'L', jma)
            call nocart(carte, 3, k8b, 'NUM', nbma,&
                        k8b, zi(jma), ' ', 3)
            call jedetr(mesmai)
        endif
!
20  end do
!
    call jedema()
end subroutine
