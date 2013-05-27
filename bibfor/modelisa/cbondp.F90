subroutine cbondp(char, noma)
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
    implicit none
!     BUT: TRAITE LE MOT_CLE : ONDE_PLANE
!
! ARGUMENTS D'ENTREE:
!      CHAR   : NOM UTILISATEUR DE LA CHARGE
!      NOMA   : NOM DU MAILLAGE
!
!
    include 'jeveux.h'
!
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
    include 'asterfort/u2mess.h'
    real(kind=8) :: dir(4)
    character(len=24) :: signal, mesmai
    character(len=8) :: char, noma, typmcl(2), modele, k8b
    character(len=2) :: type
    integer :: nondp, jma, nbma, iarg
    character(len=16) :: motcle(2)
    character(len=19) :: carte, ligrmo
!
!-----------------------------------------------------------------------
    integer :: i, jncmp, jvalv, nbid, ndir, nsi, nty
!
!-----------------------------------------------------------------------
    call jemarq()
!
    call getvid(' ', 'MODELE', 0, iarg, 1,&
                modele, jma)
    ligrmo = modele//'.MODELE'
!
! --- INFORMATIONS SUR L'ONDE PLANE
!
    call getfac('ONDE_PLANE', nondp)
!
    if (nondp .eq. 0) goto 9999
    call getvr8('ONDE_PLANE', 'DIRECTION', 1, iarg, 0,&
                dir, ndir)
    ndir = - ndir
    call getvr8('ONDE_PLANE', 'DIRECTION', 1, iarg, ndir,&
                dir, nbid)
    if (ndir .eq. 2) dir(3) = 0.d0
    call getvtx('ONDE_PLANE', 'TYPE_ONDE', 1, iarg, 1,&
                type, nty)
    call getvid('ONDE_PLANE', 'FONC_SIGNAL', 1, iarg, 1,&
                signal, nsi)
!
! --- MAILLES CONSERNEES PAR L'ONDE PLANE
!
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
    mesmai ='&&ONDPLA.MESMAI'
!
    call reliem(ligrmo, noma, 'NU_MAILLE', 'ONDE_PLANE', 1,&
                2, motcle, typmcl, mesmai, nbma)
!
    if (nbma .eq. 0) goto 100
!
    call jeveuo(mesmai, 'L', jma)
!
! --- STOCKAGE DANS LA CARTE NEUT_K24
!
    carte=char//'.CHME.ONDPL'
    call alcart('G', carte, noma, 'NEUT_K24')
!
    call jeveuo(carte//'.NCMP', 'E', jncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
!
! --- STOCKAGE DES VALEURS NULLES SUR TOUT LE MAILLAGE
!
    zk8(jncmp) = 'Z1'
    zk24(jvalv) = '&FOZERO'
    call nocart(carte, 1, ' ', 'NOM', 0,&
                ' ', 0, ligrmo, 1)
!
    zk24(jvalv) = signal
    call nocart(carte, 3, k8b, 'NUM', nbma,&
                k8b, zi(jma), ' ', 1)
!
! --- STOCKAGE DANS LA CARTE NEUT_R
!
    carte=char//'.CHME.ONDPR'
    call alcart('G', carte, noma, 'NEUT_R')
!
    call jeveuo(carte//'.NCMP', 'E', jncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
!
! --- STOCKAGE DES VALEURS NULLES SUR TOUT LE MAILLAGE
!
    zk8(jncmp) = 'X1'
    zk8(jncmp+1) = 'X2'
    zk8(jncmp+2) = 'X3'
    zk8(jncmp+3) = 'X4'
    call nocart(carte, 1, ' ', 'NOM', 0,&
                ' ', 0, ligrmo, 4)
!
! --- REMPLISSAGE DE LA CARTE NEUT_R CORRESPONDANTE
!
    if (ndir .eq. 3) then
        if (type .eq. 'P ') then
            dir(4) = 0.d0
        else if (type.eq.'SV') then
            dir(4) = 1.d0
        else if (type.eq.'SH') then
            dir(4) = 2.d0
        else if (type.eq.'S ') then
            call u2mess('F', 'MODELISA3_61')
        endif
    else
        if (type .eq. 'P ') then
            dir(4) = 0.d0
        else if (type.eq.'S ') then
            dir(4) = 1.d0
        else if (type.eq.'SV'.or.type.eq.'SH') then
            call u2mess('F', 'MODELISA3_62')
        endif
    endif
!
    do 10 i = 1, 4
        zr(jvalv+i-1) = dir(i)
10  continue
!
    call nocart(carte, 3, k8b, 'NUM', nbma,&
                k8b, zi(jma), ' ', 4)
!
100  continue
!
    call jedetr(mesmai)
!
9999  continue
    call jedema()
end subroutine
