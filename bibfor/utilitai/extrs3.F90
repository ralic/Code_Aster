subroutine extrs3(resu, param, iordr, cel, itype,&
                  type, iad)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/codent.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/lxliis.h'
    include 'asterfort/u2mesg.h'
    integer :: itype, iad, iordr
    character(len=1) :: cel
    character(len=*) :: resu, param, type
!     ------------------------------------------------------------------
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
! IN  : RESU   : NOM DE LA STRUCTURE "RESULTAT".
! IN  : NOPARA : NOM SYMBOLIQUE DU PARAMETRE.
! IN  : IORDR  : NUMERO DE RANGEMENT
! IN  : CEL    : CONDITION D'ACCES AUX PARAMETRES :
!                    'L' : LECTURE, 'E' : ECRITURE.
! IN  : ITYPE  : CODE INDIQUANT QUE L'ON DESIRE LE TYPE
!                     = 0  PAS DE TYPE
!                    /= 0  ON FOURNIT LE TYPE
! OUT : TYPE   : CODE DU TYPE
!               R REAL,I INTEGER,C COMPLEXE,K8 K16 K24 K32 K80 CHARACTER
! OUT : IAD    : ADRESSE JEVEUX DANS ZI,ZR,...
!     ------------------------------------------------------------------
!
    integer :: ipara, iatava, ire1, ire2, idebu, imaxi, iloty
    integer :: ibid, iaobj, len
    character(len=8) :: k8b, nomobj, k8debu, k8maxi
    character(len=24) :: valk(3)
    character(len=16) :: nopara
    character(len=19) :: nomsd
!     ------------------------------------------------------------------
!
    nomsd = resu
    nopara = param
!
    call jenonu(jexnom(nomsd//'.NOVA', nopara), ipara)
    if (ipara .eq. 0) then
        valk (1) = nopara
        valk (2) = nomsd
        call u2mesg('F', 'UTILITAI6_12', 2, valk, 0,&
                    0, 0, 0.d0)
    endif
!
    call jeveuo(jexnum(nomsd//'.TAVA', ipara), 'L', iatava)
    nomobj = zk8(iatava-1+1)
    k8debu = zk8(iatava-1+2)
    call lxliis(k8debu, idebu, ire1)
    k8maxi = zk8(iatava-1+3)
    call lxliis(k8maxi, imaxi, ire2)
    if (abs(ire1)+abs(ire2) .gt. 0) then
        valk (1) = nopara
        valk (2) = k8debu
        valk (3) = k8maxi
        call u2mesg('F', 'UTILITAI6_13', 3, valk, 0,&
                    0, 0, 0.d0)
    endif
!
    call jeveuo(nomsd//nomobj, cel, iaobj)
    iad = iaobj - 1 + (iordr-1)*imaxi + idebu
!
    if (itype .ne. 0) then
        call jelira(nomsd//nomobj, 'TYPE', ibid, type)
        if (type(1:1) .eq. 'K') then
            call assert(len(type).ge.2)
            call jelira(nomsd//nomobj, 'LTYP', iloty, k8b)
            call codent(iloty, 'G', k8b)
            type = type(1:1)//k8b
        endif
    endif
!
end subroutine
