subroutine tbajli(nomta, nbpar, nompar, vi, vr,&
                  vc, vk, nume)
    implicit   none
    include 'jeveux.h'
!
    include 'asterc/ismaem.h'
    include 'asterc/r8vide.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/juveca.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: nbpar, vi(*), nume
    real(kind=8) :: vr(*)
    complex(kind=8) :: vc(*)
    character(len=*) :: nomta, nompar(*), vk(*)
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
!      AJOUTER UNE LIGNE A UNE TABLE.
! ----------------------------------------------------------------------
! IN  : NOMTA  : NOM DE LA STRUCTURE "TABLE".
! IN  : NBPAR  : NOMBRE DE PARAMETRES DE NOMPAR
! IN  : NOMPAR : NOMS DES PARAMETRES DE LA LIGNE
! IN  : VI     : LISTE DES VALEURS POUR LES PARAMETRES "I"
! IN  : VR     : LISTE DES VALEURS POUR LES PARAMETRES "R"
! IN  : VC     : LISTE DES VALEURS POUR LES PARAMETRES "C"
! IN  : VK     : LISTE DES VALEURS POUR LES PARAMETRES "K"
! IN  : NUME   : NUMERO DE LIGNE
!                = 0 : ON AJOUTE UNE LIGNE
!                > 0 : ON SURCHARGE UNE LIGNE
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: iret, nbpara, nblign, jtbnp, nbpm, nbpu
    integer :: ndim, jtblp, i, j, jvale, jlogq, ki, kr, kc, kk
    character(len=3) :: type
    character(len=8) :: k8b
    character(len=19) :: nomtab
    character(len=24) :: nomjv, nomjvl, inpar, jnpar
! ----------------------------------------------------------------------
!
    call jemarq()
!
    nomtab = ' '
    nomtab = nomta
    call jeexin(nomtab//'.TBBA', iret)
    if (iret .eq. 0) then
        call u2mess('F', 'UTILITAI4_64')
    endif
    if (nomtab(18:19) .ne. '  ') then
        call u2mess('F', 'UTILITAI4_68')
    endif
!
    call jeveuo(nomtab//'.TBNP', 'E', jtbnp)
    nbpara = zi(jtbnp )
    nblign = zi(jtbnp+1)
    if (nbpara .eq. 0) then
        call u2mess('F', 'UTILITAI4_65')
    endif
    if (nume .lt. 0) then
        call u2mess('F', 'UTILITAI4_70')
    endif
    if (nume .gt. nblign) then
        call u2mess('F', 'UTILITAI4_74')
    endif
!
    call jeveuo(nomtab//'.TBLP', 'L', jtblp)
    nomjv = zk24(jtblp+2)
    call jelira(nomjv, 'LONMAX', nbpm, k8b)
    call jelira(nomjv, 'LONUTI', nbpu, k8b)
!
    ndim = nbpu + 1
    if (ndim .gt. nbpm) then
        ndim = nint((ndim*3.d0)/2.d0)
        do 10 i = 1, nbpara
            nomjv = zk24(jtblp+4*(i-1)+2)
            call juveca(nomjv, ndim)
            nomjv = zk24(jtblp+4*(i-1)+3)
            call juveca(nomjv, ndim)
10      continue
    endif
!
    if (nume .eq. 0) then
        nblign = nblign + 1
        zi(jtbnp+1) = nblign
!
        do 20 i = 1, nbpara
            nomjv = zk24(jtblp+4*(i-1)+2)
            call jeecra(nomjv, 'LONUTI', nblign, ' ')
20      continue
!
        ki = 0
        kr = 0
        kc = 0
        kk = 0
        do 30 j = 1, nbpar
            jnpar = nompar(j)
            do 32 i = 1, nbpara
                inpar = zk24(jtblp+4*(i-1) )
                if (jnpar .eq. inpar) then
                    type = zk24(jtblp+4*(i-1)+1)
                    nomjv = zk24(jtblp+4*(i-1)+2)
                    nomjvl = zk24(jtblp+4*(i-1)+3)
                    call jeveuo(nomjv, 'E', jvale)
                    call jeveuo(nomjvl, 'E', jlogq)
                    if (type(1:1) .eq. 'I') then
                        ki = ki + 1
                        if (vi(ki) .eq. ismaem()) then
                            zi(jlogq+nblign-1) = 0
                        else
                            zi(jvale+nblign-1) = vi(ki)
                            zi(jlogq+nblign-1) = 1
                        endif
                    else if (type(1:1) .eq. 'R') then
                        kr = kr + 1
                        if (vr(kr) .eq. r8vide()) then
                            zi(jlogq+nblign-1) = 0
                        else
                            zr(jvale+nblign-1) = vr(kr)
                            zi(jlogq+nblign-1) = 1
                        endif
                    else if (type(1:1) .eq. 'C') then
                        kc = kc + 1
                        if (dble(vc(kc)) .eq. r8vide() .and. dimag( vc(kc)) .eq. r8vide()) then
                            zi(jlogq+nblign-1) = 0
                        else
                            zc(jvale+nblign-1) = vc(kc)
                            zi(jlogq+nblign-1) = 1
                        endif
                    else if (type(1:3) .eq. 'K80') then
                        kk = kk + 1
                        if (vk(kk)(1:7) .eq. '???????') then
                            zi(jlogq+nblign-1) = 0
                        else
                            zk80(jvale+nblign-1) = vk(kk)
                            zi(jlogq+nblign-1) = 1
                        endif
                    else if (type(1:3) .eq. 'K32') then
                        kk = kk + 1
                        if (vk(kk)(1:7) .eq. '???????') then
                            zi(jlogq+nblign-1) = 0
                        else
                            zk32(jvale+nblign-1) = vk(kk)
                            zi(jlogq+nblign-1) = 1
                        endif
                    else if (type(1:3) .eq. 'K24') then
                        kk = kk + 1
                        if (vk(kk)(1:7) .eq. '???????') then
                            zi(jlogq+nblign-1) = 0
                        else
                            zk24(jvale+nblign-1) = vk(kk)
                            zi(jlogq+nblign-1) = 1
                        endif
                    else if (type(1:3) .eq. 'K16') then
                        kk = kk + 1
                        if (vk(kk)(1:7) .eq. '???????') then
                            zi(jlogq+nblign-1) = 0
                        else
                            zk16(jvale+nblign-1) = vk(kk)
                            zi(jlogq+nblign-1) = 1
                        endif
                    else if (type(1:2) .eq. 'K8') then
                        kk = kk + 1
                        if (vk(kk)(1:7) .eq. '???????') then
                            zi(jlogq+nblign-1) = 0
                        else
                            zk8(jvale+nblign-1) = vk(kk)
                            zi(jlogq+nblign-1) = 1
                        endif
                    endif
                    goto 34
                endif
32          continue
            call u2mesk('F', 'TABLE0_1', 1, jnpar)
34          continue
30      continue
!
    else
        ki = 0
        kr = 0
        kc = 0
        kk = 0
        do 40 j = 1, nbpar
            jnpar = nompar(j)
            do 42 i = 1, nbpara
                inpar = zk24(jtblp+4*(i-1) )
                if (jnpar .eq. inpar) then
                    type = zk24(jtblp+4*(i-1)+1)
                    nomjv = zk24(jtblp+4*(i-1)+2)
                    nomjvl = zk24(jtblp+4*(i-1)+3)
                    call jeveuo(nomjv, 'E', jvale)
                    call jeveuo(nomjvl, 'E', jlogq)
                    if (type(1:1) .eq. 'I') then
                        ki = ki + 1
                        if (vi(ki) .eq. ismaem()) then
                            zi(jlogq+nblign-1) = 0
                        else
                            zi(jvale+nume-1) = vi(ki)
                            zi(jlogq+nume-1) = 1
                        endif
                    else if (type(1:1) .eq. 'R') then
                        kr = kr + 1
                        if (vr(kr) .eq. r8vide()) then
                            zi(jlogq+nume-1) = 0
                        else
                            zr(jvale+nume-1) = vr(kr)
                            zi(jlogq+nume-1) = 1
                        endif
                    else if (type(1:1) .eq. 'C') then
                        kc = kc + 1
                        if (dble(vc(kc)) .eq. r8vide() .and. dimag( vc(kc)) .eq. r8vide()) then
                            zi(jlogq+nume-1) = 0
                        else
                            zc(jvale+nume-1) = vc(kc)
                            zi(jlogq+nume-1) = 1
                        endif
                    else if (type(1:3) .eq. 'K80') then
                        kk = kk + 1
                        if (vk(kk)(1:7) .eq. '???????') then
                            zi(jlogq+nblign-1) = 0
                        else
                            zk80(jvale+nume-1) = vk(kk)
                            zi(jlogq+nume-1) = 1
                        endif
                    else if (type(1:3) .eq. 'K32') then
                        kk = kk + 1
                        if (vk(kk)(1:7) .eq. '???????') then
                            zi(jlogq+nblign-1) = 0
                        else
                            zk32(jvale+nume-1) = vk(kk)
                            zi(jlogq+nume-1) = 1
                        endif
                    else if (type(1:3) .eq. 'K24') then
                        kk = kk + 1
                        if (vk(kk)(1:7) .eq. '???????') then
                            zi(jlogq+nblign-1) = 0
                        else
                            zk24(jvale+nume-1) = vk(kk)
                            zi(jlogq+nume-1) = 1
                        endif
                    else if (type(1:3) .eq. 'K16') then
                        kk = kk + 1
                        if (vk(kk)(1:7) .eq. '???????') then
                            zi(jlogq+nblign-1) = 0
                        else
                            zk16(jvale+nume-1) = vk(kk)
                            zi(jlogq+nume-1) = 1
                        endif
                    else if (type(1:2) .eq. 'K8') then
                        kk = kk + 1
                        if (vk(kk)(1:7) .eq. '???????') then
                            zi(jlogq+nblign-1) = 0
                        else
                            zk8(jvale+nume-1) = vk(kk)
                            zi(jlogq+nume-1) = 1
                        endif
                    endif
                    goto 44
                endif
42          continue
            call u2mesk('F', 'TABLE0_1', 1, jnpar)
44          continue
40      continue
!
    endif
!
    call jedema()
end subroutine
