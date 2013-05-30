subroutine rccomp(chmat, nomail, nomode)
    implicit   none
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/getvtx.h'
    include 'asterc/lccree.h'
    include 'asterc/lcinfo.h'
    include 'asterfort/alcart.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/nocart.h'
    include 'asterfort/reliem.h'
    include 'asterfort/u2mess.h'
    character(len=8) :: chmat, nomail, nomode
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
!
!  IN : CHMAT  : CHAMP MATERIAU PRODUIT
!  IN : NOMAIL : NOM DU MAILLAGE
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
    integer :: ibid, nocc, i, j, nm, nt, jncmp, jvalv, nbma, jmail
    character(len=8) :: typmcl(2)
    character(len=16) :: motcle(2)
    character(len=24) :: mesmai
    character(len=1) :: k1bid
!
    integer :: ncmpma, icmp, icpri, icprk, nbgmax, icp, numlc
    parameter (ncmpma=7+9+4)
    character(len=8) :: nomcmp(ncmpma), k8b, sdcomp
    character(len=16) :: comcod
    character(len=19) :: compor
    integer :: iarg
    data nomcmp/'RELCOM  ','NBVARI  ','DEFORM  ','INCELA  ',&
     &     'C_PLAN  ','XXXX1','XXXX2','KIT1    ','KIT2    ','KIT3    ',&
     &     'KIT4    ','KIT5    ','KIT6    ','KIT7    ','KIT8    ',&
     &     'KIT9    ', 'NVI_C   ', 'NVI_T   ', 'NVI_H   ', 'NVI_M   '/
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call getfac('AFFE_COMPOR', nocc)
    if (nocc .eq. 0) goto 999
!
    compor = chmat//'.COMPOR'
!
    call alcart('G', compor, nomail, 'COMPOR')
    call jeveuo(compor//'.NCMP', 'E', jncmp)
    call jeveuo(compor//'.VALV', 'E', jvalv)
!
    do 90 icmp = 1, ncmpma
        zk8(jncmp+icmp-1) = nomcmp(icmp)
        zk16(jvalv+icmp-1)= ' '
90  end do
!
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
    mesmai = '&&RCCOMP.MES_MAILLES'
!
    do 10 i = 1, nocc
        call getvid('AFFE_COMPOR', 'COMPOR', i, iarg, 1,&
                    sdcomp, nm)
        call jeveuo(sdcomp//'.CPRI', 'L', icpri)
        call assert(zi(icpri).eq.3)
! ---ON MET LE NOM DE LA PREMIERE RELATION NON VIDE DANS RELCOM POUR QUE
!    ALGO1D FONCTIONNE (AVEC UN SEUL GROUPE DE FIBRE)
        call jeveuo(sdcomp//'.CPRK', 'L', icprk)
! --- RECHERCHE DE LA PREMIERE RELATION NON VIDE
        call jelira(sdcomp//'.CPRK', 'LONMAX', nbgmax, k1bid)
        nbgmax=(nbgmax-1)/6
        do 20 j = 1, nbgmax
            icp=icprk-1+6*(j-1)
            if (zk24(icp+2) .ne. 'VIDE') goto 25
20      continue
        call u2mess('F', 'MODELISA7_99')
25      continue
!---- REMPLISSAGE DE LA CARTE
        zk16(jvalv-1+1) = zk24(icp+3)
        write (zk16(jvalv-1+2),'(I16)') zi(icpri+1)
        zk16(jvalv-1+3) = zk24(icp+5)
        zk16(jvalv-1+4) = 'COMP_INCR'
        zk16(jvalv-1+5) = zk24(icp+4)
!         APPEL A LCINFO POUR RECUPERER LE NUMERO DE LC
        call lccree(1, zk24(icp+3), comcod)
        call lcinfo(comcod, numlc, ibid)
        write(zk16(jvalv-1+6),'(I16)') numlc
        zk16(jvalv-1+7) = sdcomp//'.CPRK'
!
        call getvtx('AFFE_COMPOR', 'TOUT', i, iarg, 1,&
                    k8b, nt)
        if (nt .ne. 0) then
            call nocart(compor, 1, k8b, k8b, 0,&
                        k8b, ibid, ' ', ncmpma)
        else
            call reliem(nomode, nomail, 'NU_MAILLE', 'AFFE_COMPOR', i,&
                        2, motcle(1), typmcl(1), mesmai, nbma)
            if (nbma .ne. 0) then
                call jeveuo(mesmai, 'L', jmail)
                call nocart(compor, 3, k8b, 'NUM', nbma,&
                            k8b, zi(jmail), ' ', ncmpma)
                call jedetr(mesmai)
            endif
        endif
10  end do
!
!
    call jedetr(compor//'.VALV')
    call jedetr(compor//'.NCMP')
!
999  continue
    call jedema()
end subroutine
