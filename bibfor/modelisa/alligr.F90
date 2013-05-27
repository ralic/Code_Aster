subroutine alligr(char, oper, noma, fonree, ligrcz)
    implicit none
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterfort/alcar1.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/ligecp.h'
    include 'asterfort/wkvect.h'
    character(len=4) :: fonree
    character(len=8) :: char, noma
    character(len=16) :: oper
    character(len=*) :: ligrcz
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
!     ALLOUER LE LIGREL DE CHARGE  CORRESPONDANT AUX MOTS-CLE:
!       FORCE_NODALE       EN MECANIQUE
!       ECHANGE_PAROI      EN THERMIQUE
!
! REMARQUE : LA DUALISATION DES CL POURRA ENRICHIR ULTERIEUREMENT CE
!            LIGREL (OU LE CREER). VOIR AFLRCH.F
!
! IN  : CHAR   : NOM UTILISATEUR DE LA CHARGE
! IN  : OPER   : NOM DE LA COMMANDE (AFFE_CHAR_XXXX)
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : FONREE : FONC OU REEL SUIVANT L'OPERATEUR
! OUT : LIGRCZ : NOM DU LIGREL DE CHARGE
! ----------------------------------------------------------------------
!
    integer :: nfono, nechp, ntot, lonlig, lonema, nbgrel, nbmata, idnbno
    integer :: idlgns, nbet, jlgrf
    integer :: nbt1(10), llig, lnema, ngrel
    character(len=8) :: typmcl(2)
    character(len=16) :: moclef, mocles(2)
    character(len=19) :: ligrch
!     ------------------------------------------------------------------
!
    call jemarq()
    nfono = 0
    nechp = 0
    lonlig = 0
    lonema = 0
    nbgrel = 0
    nbmata = 0
!
    mocles(1) = 'GROUP_NO'
    mocles(2) = 'NOEUD'
    typmcl(1) = 'GROUP_NO'
    typmcl(2) = 'NOEUD'
!
    if (oper(1:14) .eq. 'AFFE_CHAR_MECA') then
        if (fonree .ne. 'COMP') then
            moclef = 'FORCE_NODALE'
            call getfac(moclef, nfono)
            if (nfono .ne. 0) then
                call alcar1(noma, moclef, 2, mocles, typmcl,&
                            nbet)
                lonlig = lonlig + 2*nbet
                lonema = lonema + 2*nbet
                nbgrel = nbgrel + nbet
                nbmata = nbmata + nbet
            endif
        endif
        ligrch = char//'.CHME.LIGRE'
!
!
    else if (oper(1:14).eq.'AFFE_CHAR_THER') then
        call getfac('ECHANGE_PAROI', nechp)
        if (nechp .ne. 0) then
            call ligecp(noma, nbt1, llig, lnema, ngrel)
            lonlig = lonlig + llig
            lonema = lonema + lnema
            nbgrel = nbgrel + ngrel
            nbmata = nbmata + nbt1(3)
        endif
        ligrch = char//'.CHTH.LIGRE'
!
!
    else if (oper(1:14).eq.'AFFE_CHAR_ACOU') then
        ligrch = char//'.CHAC.LIGRE'
    endif
!
!
!     --- CREATION DU LIGREL DE CHARGE SI NECESSAIRE ---
    ntot = nfono + nechp
    if (ntot .ne. 0) then
        nbgrel = max(nbgrel,1)
        call jecrec(ligrch//'.LIEL', 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                    nbgrel)
        lonlig = max(lonlig,1)
        call jeecra(ligrch//'.LIEL', 'LONT', lonlig, ' ')
        nbmata = max(nbmata,1)
        call jecrec(ligrch//'.NEMA', 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                    nbmata)
        lonema = max(lonema,1)
        call jeecra(ligrch//'.NEMA', 'LONT', lonema, ' ')
        call wkvect(ligrch//'.LGRF', 'G V K8', 2, jlgrf)
        zk8(jlgrf) = noma
        call wkvect(ligrch//'.NBNO', 'G V I', 1, idnbno)
        zi(idnbno) = 0
        call wkvect(ligrch//'.LGNS', 'G V I', 2*lonema, idlgns)
    endif
    ligrcz = ligrch
!
    call jedema()
end subroutine
