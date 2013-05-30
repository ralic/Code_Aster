subroutine tbajco(nomta, para, type, nbval, vi,&
                  vr, vc, vk, action, llign)
    implicit   none
!
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: nbval, vi(*), llign(*)
    real(kind=8) :: vr(*)
    complex(kind=8) :: vc(*)
    character(len=*) :: nomta, para, type, vk(*), action
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
!      AJOUTER OU REMPLIR  UNE COLONNE DANS UNE TABLE.
! ----------------------------------------------------------------------
! IN  : NOMTA   : NOM DE LA STRUCTURE "TABLE".
! IN  : PARA    : NOM DU PARAMETRE
! IN  : NBVAL   : NOMBRE DE VALEUR A RENTRER
! IN  : TYPE    : TYPE DU PARAMETRE D ENTREE
! IN  : VI      : LISTE DES VALEURS POUR LES PARAMETRES "I"
! IN  : VR      : LISTE DES VALEURS POUR LES PARAMETRES "R"
! IN  : VC      : LISTE DES VALEURS POUR LES PARAMETRES "C"
! IN  : VK      : LISTE DES VALEURS POUR LES PARAMETRES "K"
! IN  : ACTION  : TYPE D ACTION A ENTREPRENDRE
!                  'A' ON AJOUTE UNE COLONNE
!                  'R' ON REMPLIT UNE COLONNE EXISTANTE
! IN  : LLIGN   : LISTE DES INDICES DE LIGNES A AJOUTER EFFECTIVEMENT
!                 SI PREMIERE VALEUR =-1, ON AJOUTE SANS DECALAGE
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: i, iret, jtblp, jtbnp, nbpara, jvale, jlogq, nblign, iind
    character(len=1) :: actioz
    character(len=3) :: typez, typev
    character(len=19) :: nomtab
    character(len=24) :: nomjv, nomjvl, paraz, indic
! ----------------------------------------------------------------------
!
    call jemarq()
!
    indic='&&TABJCO.IND'
    call wkvect(indic, 'V V I', nbval, iind)
    nomtab=' '
    nomtab=nomta
    typez=' '
    typez=type
    actioz=action
    paraz=' '
    paraz=para
!
    call jeexin(nomtab//'.TBBA', iret)
    if (iret .eq. 0) then
        call u2mess('F', 'UTILITAI4_64')
    endif
    if (nomtab(18:19) .ne. '  ') then
        call u2mess('F', 'UTILITAI4_68')
    endif
!
    if (actioz .eq. 'A') then
        call tbajpa(nomtab, 1, para, typez)
    endif
!
    call jeveuo(nomtab//'.TBLP', 'L', jtblp)
    call jeveuo(nomtab//'.TBNP', 'L', jtbnp)
!
    nbpara=zi(jtbnp)
    nblign=zi(jtbnp+1)
    if (nbpara .eq. 0) then
        call u2mess('F', 'UTILITAI4_65')
    endif
!
    if (nbval .gt. nblign) then
        call u2mess('F', 'UTILITAI4_69')
    endif
!
    if (llign(1) .ne. -1) then
        do 10 i = 1, nbval
            zi(iind+i-1)=llign(i)
            if (llign(i) .le. 0) then
                call u2mess('F', 'UTILITAI4_70')
            endif
            if (llign(i) .gt. nblign) then
                call u2mess('F', 'UTILITAI4_71')
            endif
10      continue
    else
        do 20 i = 1, nbval
            zi(iind+i-1)=i
20      continue
    endif
!
!  --- RECHERCHE DES NOMS JEVEUX DU PARAMETRE
    iret=0
    do 40 i = 1, nbpara
        if (paraz .eq. zk24(jtblp+(4*(i-1)))) then
            nomjv=zk24(jtblp+(4*(i-1)+2))
            nomjvl=zk24(jtblp+(4*(i-1)+3))
            typev=zk24(jtblp+(4*(i-1)+1))
            iret=1
        endif
40  end do
!
    if (iret .eq. 0) then
        call u2mess('F', 'UTILITAI4_72')
    endif
!
    if (typev .ne. typez) then
        call u2mess('F', 'UTILITAI4_73')
    endif
!
    call jeecra(nomjv, 'LONUTI', nblign, ' ')
    call jeveuo(nomjv, 'E', jvale)
    call jeveuo(nomjvl, 'E', jlogq)
!
!  --- REMPLISSAGE DES CELLULES DE LA COLONNE
!
    do 50 i = 1, nbval
!
        if (typez(1:1) .eq. 'I') then
            zi(jvale+zi(iind+i-1)-1) = vi(i)
            zi(jlogq+zi(iind+i-1)-1) = 1
        else if (typez(1:1) .eq. 'R') then
            zr(jvale+zi(iind+i-1)-1) = vr(i)
            zi(jlogq+zi(iind+i-1)-1) = 1
        else if (typez(1:1) .eq. 'C') then
            zc(jvale+zi(iind+i-1)-1) = vc(i)
            zi(jlogq+zi(iind+i-1)-1) = 1
        else if (typez(1:3) .eq. 'K80') then
            zk80(jvale+zi(iind+i-1)-1) = vk(i)
            zi( jlogq+zi(iind+i-1)-1) = 1
        else if (typez(1:3) .eq. 'K32') then
            zk32(jvale+zi(iind+i-1)-1) = vk(i)
            zi( jlogq+zi(iind+i-1)-1) = 1
        else if (typez(1:3) .eq. 'K24') then
            zk24(jvale+zi(iind+i-1)-1) = vk(i)
            zi( jlogq+zi(iind+i-1)-1) = 1
        else if (typez(1:3) .eq. 'K16') then
            zk16(jvale+zi(iind+i-1)-1) = vk(i)
            zi( jlogq+zi(iind+i-1)-1) = 1
        else if (typez(1:2) .eq. 'K8') then
            zk8(jvale+zi(iind+i-1)-1) = vk(i)
            zi( jlogq+zi(iind+i-1)-1) = 1
        endif
50  end do
    call jedetr(indic)
    call jedema()
end subroutine
