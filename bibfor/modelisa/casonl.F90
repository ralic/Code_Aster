subroutine casonl(char, ligrmo, noma, ndim)
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
    implicit   none
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/getvtx.h'
    include 'asterfort/alcart.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/nocart.h'
    include 'asterfort/reliem.h'
    include 'asterfort/vetyma.h'
    integer :: ndim
    character(len=8) :: char, noma
    character(len=*) :: ligrmo
!
! ----------------------------------------------------------------------
!                   SOURCE THERMIQUE NON LINEAIRE
!
!-----------------------------------------------------------------------
!  STOCKAGE DES SOURCES DANS UNE CARTE ALLOUEE SUR LE LIGREL DU MODELE
!-----------------------------------------------------------------------
!      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
!      LIGRMO : NOM DU LIGREL DE MODELE
!      NOMA   : NOM DU MAILLAGE
!      NDIM   : DIMENSION DU PROBLEME (2D/3D) POUR FILTRER LES MAILLES
!-----------------------------------------------------------------------
    integer :: ibid, nsour, jvalv, jncmp, n1, ier, ncmp, iocc, nbtou, nbma, jma
    character(len=8) :: k8b, typmcl(2)
    character(len=16) :: motclf, motcle(2)
    character(len=19) :: carte
    character(len=24) :: mesmai
    integer :: iarg
! ----------------------------------------------------------------------
    data mesmai /'&&CASONL.MES_MAILLES'/
    data motcle /'GROUP_MA','MAILLE'/
    data typmcl /'GROUP_MA','MAILLE'/
! ----------------------------------------------------------------------
    call jemarq()
!
    motclf = 'SOUR_NL'
    ncmp = 1
    call getfac(motclf, nsour)
!
    carte = char//'.CHTH.SOUNL'
    call alcart('G', carte, noma, 'SOUR_F')
    call jeveuo(carte//'.NCMP', 'E', jncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
    zk8(jncmp) = 'SOUR'
!
!
! --- DEFAUT: STOCKAGE D'UNE SOURCE NULLE SUR TOUT LE MAILLAGE
!
    zk8(jvalv) = '&FOZERO'
    call nocart(carte, 1, ' ', 'NOM', 0,&
                ' ', 0, ligrmo, ncmp)
!
!
! --- STOCKAGE DES FONCTIONS SOURCES DANS LA CARTE
!
    do 10 iocc = 1, nsour
        call getvid(motclf, 'SOUR', iocc, iarg, 1,&
                    zk8(jvalv), n1)
!
        call getvtx(motclf, 'TOUT', iocc, iarg, 1,&
                    k8b, nbtou)
        if (nbtou .ne. 0) then
            call nocart(carte, 1, ' ', 'NOM', 0,&
                        ' ', 0, ligrmo, ncmp)
!
        else
            call reliem(ligrmo, noma, 'NO_MAILLE', motclf, iocc,&
                        2, motcle, typmcl, mesmai, nbma)
            if (nbma .eq. 0) goto 10
!
            call jeveuo(mesmai, 'L', jma)
            call vetyma(noma, zk8(jma), nbma, k8b, 0,&
                        motclf, ndim, ier)
            call nocart(carte, 3, k8b, 'NOM', nbma,&
                        zk8(jma), ibid, ' ', ncmp)
            call jedetr(mesmai)
        endif
10  end do
!
    call jedema()
end subroutine
