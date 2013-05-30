subroutine tbacce(nomta, numeli, para, mode, vi,&
                  vr, vc, vk)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: numeli, vi
    real(kind=8) :: vr
    complex(kind=8) :: vc
    character(len=*) :: nomta, para, mode, vk
! ----------------------------------------------------------------------
! person_in_charge: Christophe-mmn.durand at edf.fr
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
!      ACCES A UNE CELLULE D'UNE LIGNE DE LA TABLE
!         EN MODE LECTURE , LE V. EST EN SORTIE
!         EN MODE ECRITURE, LE V. EST EN DONNEE
! ----------------------------------------------------------------------
! IN     : NOMTA  : NOM DE LA STRUCTURE "TABLE".
! IN     : NUMELI : NUMERO DE LA LIGNE
! IN     : PARA   : PARAMETRE
! IN     : MODE   : ACCES EN MODE ECRITURE 'E' OU LECTURE 'L'
! IN/OUT : VI     : VALEUR POUR LE PARAMETRE "I"
! IN/OUT : VR     : VALEUR POUR LE PARAMETRE "R"
! IN/OUT : VC     : VALEUR POUR LE PARAMETRE "C"
! IN/OUT : VK     : VALEUR POUR LE PARAMETRE "K"
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: iret, nbpara, nblign, jtbnp, jtblp, j, jvale, jvall
    character(len=1) :: modacc
    character(len=4) :: type
    character(len=19) :: nomtab
    character(len=24) :: nomjv, nomjvl, inpar
    character(len=24) :: valk
! ----------------------------------------------------------------------
!
    call jemarq()
!
    modacc = mode
    if (modacc .eq. 'L') then
    else if (modacc .eq. 'E') then
    else
        call u2mesk('F', 'UTILITAI4_63', 1, modacc)
    endif
!
    nomtab = nomta
    call jeexin(nomtab//'.TBBA', iret)
    if (iret .eq. 0) then
        call u2mess('F', 'UTILITAI4_64')
    endif
!
    call jeveuo(nomtab//'.TBNP', 'E', jtbnp)
    nbpara = zi(jtbnp )
    nblign = zi(jtbnp+1)
    if (nbpara .eq. 0) then
        call u2mess('F', 'UTILITAI4_65')
    endif
    if (nblign .eq. 0) then
        call u2mess('F', 'UTILITAI4_66')
    endif
    if (numeli .gt. nblign) then
        call u2mess('F', 'UTILITAI4_67')
    endif
!
    call jeveuo(nomtab//'.TBLP', 'L', jtblp)
!
!     --- VERIFICATION QUE LE PARAMETRE EXISTE DANS LA TABLE ---
!
    inpar = para
    do 10 j = 1, nbpara
        if (inpar .eq. zk24(jtblp+4*(j-1))) goto 12
10  end do
    valk = inpar
    call u2mesg('F', 'UTILITAI6_89', 1, valk, 0,&
                0, 0, 0.d0)
12  continue
!
    type = zk24(jtblp+4*(j-1)+1)
    nomjv = zk24(jtblp+4*(j-1)+2)
    nomjvl = zk24(jtblp+4*(j-1)+3)
!
    call jeveuo(nomjv, modacc, jvale)
    call jeveuo(nomjvl, modacc, jvall)
!
    if (type(1:1) .eq. 'I') then
        if (modacc .eq. 'L') then
            vi = zi(jvale+numeli-1)
        else
            zi(jvale+numeli-1) = vi
            zi(jvall+numeli-1) = 1
        endif
!
    else if (type(1:1) .eq. 'R') then
        if (modacc .eq. 'L') then
            vr = zr(jvale+numeli-1)
        else
            zr(jvale+numeli-1) = vr
            zi(jvall+numeli-1) = 1
        endif
!
    else if (type(1:1) .eq. 'C') then
        if (modacc .eq. 'L') then
            vc = zc(jvale+numeli-1)
        else
            zc(jvale+numeli-1) = vc
            zi(jvall+numeli-1) = 1
        endif
!
    else if (type(1:3) .eq. 'K80') then
        if (modacc .eq. 'L') then
            vk = zk80(jvale+numeli-1)
        else
            zk80(jvale+numeli-1) = vk
            zi (jvall+numeli-1) = 1
        endif
!
    else if (type(1:3) .eq. 'K32') then
        if (modacc .eq. 'L') then
            vk = zk32(jvale+numeli-1)
        else
            zk32(jvale+numeli-1) = vk
            zi (jvall+numeli-1) = 1
        endif
!
    else if (type(1:3) .eq. 'K24') then
        if (modacc .eq. 'L') then
            vk = zk24(jvale+numeli-1)
        else
            zk24(jvale+numeli-1) = vk
            zi (jvall+numeli-1) = 1
        endif
!
    else if (type(1:3) .eq. 'K16') then
        if (modacc .eq. 'L') then
            vk = zk16(jvale+numeli-1)
        else
            zk16(jvale+numeli-1) = vk
            zi (jvall+numeli-1) = 1
        endif
!
    else if (type(1:2) .eq. 'K8') then
        if (modacc .eq. 'L') then
            vk = zk8(jvale+numeli-1)
        else
            zk8(jvale+numeli-1) = vk
            zi (jvall+numeli-1) = 1
        endif
    endif
!
    call jedema()
end subroutine
