subroutine gcharm(lfchar, cartei, nomfct, newfct, time  ,&
                  carteo)

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
    implicit none
    include 'jeveux.h'
    include 'asterfort/codent.h'
    include 'asterfort/copisd.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lisnca.h'
    include 'asterfort/u2mesk.h'
    logical :: lfchar
    character(len=8) :: nomfct,newfct
    real(kind=8) :: time
    character(len=19) :: cartei,carteo
!
! ----------------------------------------------------------------------
!
! ROUTINE CALC_G
!
! APPLIQUE LA FONCTION MULTIPLICATRICE SUR UN CHARGEMENT
!
! ----------------------------------------------------------------------
!
! IN  LFCHAR :  .TRUE.  SI LE CHARGEMENT EST 'FONCTION'
! IN  NOMFCT : NOM DE LA FONCTION MULTIPLICATRICE
! IN  TIME   : INSTANT
! I/O NEWFCT : FONCTION MULTIPLICATRICE MODIFIEE DANS LA CARTE DE SORTIE
!              PRODUIT DE LA FONC_MULT ET DE LA DEPENDANCE EVENTUELLE
!              VENUE D'AFFE_CHAR_MECA_F
! IN  CARTEI : CARTE DU CHARGEMENT AVANT LA PRISE EN COMPTE
!              DE LA FONCTION MULTIPLICATRICE
! OUT CARTEO : CARTE DU CHARGEMENT APRES LA PRISE EN COMPTE
!              DE LA FONCTION MULTIPLICATRICE
!
! ----------------------------------------------------------------------
!
    integer :: jvalin, jvalou, jvalf, jprol
    integer :: nbvale, iret, in, k, i, nb, npt
    real(kind=8) :: const
    character(len=8) :: k8bid,charge
    character(len=19) :: nch19
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! - VALEUR DE LA FONCTION
!
    call fointe('A', nomfct, 1, 'INST', time,&
                const, iret)
    charge = cartei(1:8)
!
! - ACCES AUX CARTES
!
    call jeveuo(cartei//'.VALE', 'L', jvalin)
    call jeveuo(carteo//'.VALE', 'E', jvalou)
    call jelira(cartei//'.VALE', 'LONMAX', nbvale, k8bid)
!
! - 1. CHARGEMENT 'SCALAIRE'
!
    if (.not.lfchar) then
        do 10 in = 1, nbvale
            zr(jvalou+in-1) = const*zr(jvalin +in-1)
10      continue
!
! - 2. CHARGEMENT 'FONCTION'
!
    else
        k=0
        do 20 in = 1, nbvale
            if (zk8(jvalin+in-1)(1:7) .ne. '&FOZERO' .and. zk8(jvalin+in-1)(1:7) .ne.&
                '       ' .and. zk8(jvalin+in-1)(1:6) .ne. 'GLOBAL') then
                k=k+1
                call codent(k, 'D0', newfct(8:8))
                call copisd('FONCTION', 'V', zk8(jvalin+in-1), newfct)
                nch19 = newfct
                call jeveuo(nch19//'.PROL', 'L', jprol)
                if (zk24(jprol)(1:8) .ne. 'INTERPRE') then
                    call jeveuo(nch19//'.VALE', 'E', jvalf)
                    call jelira(nch19//'.VALE', 'LONMAX', nb, k8bid)
                    npt=nb/2
                    do 30 i = 1, npt
                        zr(jvalf+npt+i-1)=const*zr(jvalf+npt+i-1)
30                  continue
                    zk8(jvalou+in-1) = newfct
                else
                    call u2mesk('A', 'RUPTURE2_4', 1, charge)
                    call jedupo(cartei//'.VALE', 'V', carteo//'.VALE', .false.)
                    goto 999 
                endif
            endif
20      continue
    endif
!
999 continue
    call jedema()
!
end subroutine
