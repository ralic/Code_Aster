subroutine ssingu(nomail, nelem, nbr, ligrmo, alpha,&
                  re, he, chelem)
    implicit none
    include 'jeveux.h'
    include 'asterfort/cescel.h'
    include 'asterfort/cescre.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: nelem, nbr(nelem)
    real(kind=8) :: alpha(nelem), re(nelem), he(nelem)
    character(len=8) :: nomail
    character(len=24) :: ligrmo, chelem
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT:
!         STOCKAGE DE LA SINGULARITE (ALPHA), DU RAPPORT DE TAILLE (RE)
!         ET DE LA NOUVELLE TAILLE (TAILLE) DES ELEMENTS DANS CHELEM
!         OPTION : 'SING_ELEM'
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NOMAIL       : NOM DU MAILLAGE
! IN   NELEM        : NOMBRE D ELEMENTS FINIS DU MAILLAGE
! IN   NBR(NELEM)   : NOMBRE DE COMPOSANTES A STOCKER PAR EF
!      3 SI EF SURFACIQUES EN 2D OU VOLUMIQUES EN 3D
!      0 SINON
! IN   LIGRMO       : NOM DU LIGREL DU MODELE
! IN   ALPHA(NELEM) : DEGRE DE LA SINGULARITE
! IN   RE(NELEM)    : RAPPORT ENTRE ANCIENNE ET NOUVELLE TAILLE
! IN   HE(NELEM)    : NOUVELLE TAILLE
! IN   CHELEM       : CHAM_ELEM QUI VA CONTENIR LE DEGRE ET LA TAILLE
!
!      SORTIE :
!-------------
!
! ......................................................................
!
!
!
!
    integer :: jcesc, jcesd, jcesl, jcesv, iad1, iad2, iad3
    integer :: nbcmp, ncmp1, ncmp2, ncmp3
    integer :: icmp, inel, nncp, ibid
    character(len=8) :: k8bid, nompaz, licmp(3)
    character(len=16) :: opti
    character(len=19) :: chsing
!
    call jemarq()
!
! 1 - CREATION D UN CHAM_ELEM_S CHSING
!
    chsing='&&SINGUE.SING'
    nompaz='SING_R'
    licmp(1)='DEGRE'
    licmp(2)='RAPPORT'
    licmp(3)='TAILLE'
    call cescre('V', chsing, 'ELEM', nomail, nompaz,&
                3, licmp, -1, -1, nbr)
!
! 2 - STOCKAGE DANS CHSING DE ALPHA ET RE
!
    call jeveuo(chsing//'.CESC', 'L', jcesc)
    call jelira(chsing//'.CESC', 'LONMAX', nbcmp, k8bid)
    call jeveuo(chsing//'.CESD', 'L', jcesd)
    call jeveuo(chsing//'.CESL', 'E', jcesl)
    call jeveuo(chsing//'.CESV', 'E', jcesv)
!
    do 10 icmp = 1, nbcmp
        if (zk8(jcesc+icmp-1)(1:5) .eq. 'DEGRE') ncmp1=icmp
        if (zk8(jcesc+icmp-1)(1:7) .eq. 'RAPPORT') ncmp2=icmp
        if (zk8(jcesc+icmp-1)(1:6) .eq. 'TAILLE') ncmp3=icmp
10  end do
!
    do 20 inel = 1, nelem
        call cesexi('C', jcesd, jcesl, inel, 1,&
                    1, ncmp1, iad1)
        call cesexi('C', jcesd, jcesl, inel, 1,&
                    1, ncmp2, iad2)
        call cesexi('C', jcesd, jcesl, inel, 1,&
                    1, ncmp3, iad3)
        if ((-iad1) .gt. 0) then
            zl(jcesl-iad1-1)=.true.
            zr(jcesv-iad1-1)= alpha(inel)
        endif
        if ((-iad2) .gt. 0) then
            zl(jcesl-iad2-1)=.true.
            zr(jcesv-iad2-1)= 1.d0/re(inel)
        endif
        if ((-iad3) .gt. 0) then
            zl(jcesl-iad3-1)=.true.
            zr(jcesv-iad3-1)= he(inel)
        endif
20  end do
!
! 3 - TRANSFORMATION DU CHAM_ELEM_S EN CHAM_ELEM
!
    opti='SING_ELEM'
    nompaz='PSING_R'
!
    call cescel(chsing, ligrmo(1:19), opti, nompaz, 'NON',&
                nncp, 'G', chelem(1:19), 'F', ibid)
!
    call detrsd('CHAM_ELEM_S', chsing)
!
    call jedema()
!
end subroutine
