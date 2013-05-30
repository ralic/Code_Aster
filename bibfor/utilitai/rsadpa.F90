subroutine rsadpa(nomsd, cel, npara, lpara, iordr,&
                  itype, ljeveu, ctype)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/extrs3.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeimpo.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rsutrg.h'
    include 'asterfort/u2mesg.h'
    integer :: npara, iordr, itype, ljeveu(*)
    character(len=1) :: cel
    character(len=*) :: nomsd, lpara(*), ctype(*)
! ----------------------------------------------------------------------
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
! person_in_charge: nicolas.sellenet at edf.fr
!
!      RECUPERATION DES ADRESSES JEVEUX DES PARAMETRES DE CALCUL
!      (OU DES VARIABLES D'ACCES)
!      D'UN RESULTAT-COMPOSE POUR LE NUMERO D'ORDRE : IORDR
!      ET POUR LA LISTE DE VARIABLES DE NOMS SYMBOLIQUES LPARA.
! ----------------------------------------------------------------------
! IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT".
! IN  : CEL    : CONDITION D'ACCES AUX PARAMETRES :
!                    'L' : LECTURE, 'E' : ECRITURE.
! IN  : NPARA  : NOMBRE DE PARAMETRES CHERCHES.
! IN  : LPARA  : LISTE DES NOMS SYMBOLIQUES DES PARAMETRES.
! IN  : IORDR  : NUMERO D'ORDRE SOUHAITE.
! IN  : ITYPE  : CODE INDIQUANT QUE L'ON DESIRE LE TYPE
!                     = 0  PAS DE TYPE
!                    /= 0  ON FOURNIT LE TYPE
! OUT : LJEVEU : LISTE DES ADRESSES JEVEUX DANS ZI,ZR,...
! OUT : CTYPE  : CODE DU TYPE
!               R REAL,I INTEGER,C COMPLEXE,K8 K16 K24 K32 K80 CHARACTER
!-----------------------------------------------------------------------
! REMARQUE : CETTE ROUTINE NE FAIT PAS JEMARQ/JEDEMA POUR NE PAS
!            INVALIDER LJEVEU
!-----------------------------------------------------------------------
    integer :: ibid, nbordr, nrang, jordr, i, ipara, ier, irang, ifr
    integer :: vali(2)
    real(kind=8) :: r8bid
    character(len=8) :: k8b
    character(len=24) :: valk(3)
    character(len=16) :: param, k16b
    character(len=19) :: noms2
! ----------------------------------------------------------------------
!
    noms2 = nomsd
!
!     --- RECUPERATION DU NUMERO DE RANGEMENT ---
    call rsutrg(nomsd, iordr, irang, nrang)
!
    if (cel .eq. 'L') then
        if (irang .eq. 0) then
            valk (1) = nomsd
            vali (1) = iordr
            call u2mesg('F', 'UTILITAI6_77', 1, valk, 1,&
                        vali, 0, r8bid)
        endif
    else
        if (irang .eq. 0) then
            call jelira(noms2//'.ORDR', 'LONMAX', nbordr, k8b)
            nrang = nrang + 1
            if (nrang .gt. nbordr) then
                valk (1) = nomsd
                vali (1) = iordr
                vali (2) = nbordr
                call u2mesg('F', 'UTILITAI6_78', 1, valk, 2,&
                            vali, 0, r8bid)
            endif
            call jeecra(noms2//'.ORDR', 'LONUTI', nrang, ' ')
            call jeveuo(noms2//'.ORDR', 'E', jordr)
            if (nrang .gt. 1) then
                call assert(zi(jordr+nrang-2).lt.iordr)
            endif
            zi(jordr+nrang-1) = iordr
            irang = nrang
        endif
    endif
!
    call jelira(jexnum(noms2//'.TACH', 1), 'LONMAX', nbordr, k8b)
    if (irang .gt. nbordr) then
        valk (1) = nomsd
        vali (1) = irang
        vali (2) = nbordr
        call u2mesg('F', 'UTILITAI6_79', 1, valk, 2,&
                    vali, 0, r8bid)
    endif
!
    do 10,i = 1,npara
    param = lpara(i)
    call jenonu(jexnom(noms2//'.NOVA', param), ipara)
    if (ipara .eq. 0) then
        ifr = iunifi('RESULTAT')
        call dismoi('F', 'TYPE_RESU', nomsd, 'RESULTAT', ibid,&
                    k16b, ier)
        call jeimpo(ifr, noms2//'.NOVA', ' ')
        valk (1) = nomsd
        valk (2) = param
        valk (3) = k16b
        call u2mesg('F', 'UTILITAI6_80', 3, valk, 0,&
                    0, 0, r8bid)
    endif
!
    call extrs3(noms2, param, irang, cel, itype,&
                ctype(i), ljeveu(i))
!
    10 end do
!
end subroutine
