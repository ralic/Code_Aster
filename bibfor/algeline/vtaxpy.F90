subroutine vtaxpy(alpha, chamna, chamnb)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  ENCAPSULATION DAXPY SUR LES .VALE DES CHAM_NO
!                          CHAMN1 ET CHAMN2
!                       CHAMN2.VALE = ALPHA * CHAMN1.VALE + CHAMN2.VALE
!     CETTE ROUTINE EST ADAPTEE A DES CHAM_NOS FETI TRAITES EN PARALLELE
!     ------------------------------------------------------------------
!     IN  ALPHA     :  R8  : COEFF. MULTIPLICATEUR
!     IN  CHAMNA    :  K*  : CHAM_NO MAITRE 1
!     IN/OUT CHAMNB :  K*  : CHAM_NO MAITRE 2
!----------------------------------------------------------------------
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mess.h'
    include 'blas/daxpy.h'
    character(len=*) :: chamna, chamnb
    real(kind=8) :: alpha
!
!
! DECLARATION VARIABLES LOCALES
    integer :: nbsd, ilimpi, ifetc1, ifetc2, idd, neq, ival1, ival2, iret1
    integer :: iret2
    character(len=8) :: k8bid
    character(len=24) :: kval1, kval2, chamn1, chamn2
    logical :: iddok, lfeti
!
! CORPS DU PROGRAMME
    call jemarq()
    chamn1=chamna
    chamn2=chamnb
!
! --- TEST POUR SAVOIR SI LE SOLVEUR EST DE TYPE FETI
!
    call jeexin(chamn1(1:19)//'.FETC', iret1)
    if (iret1 .ne. 0) then
        call jeexin(chamn2(1:19)//'.FETC', iret2)
        if (iret2 .eq. 0) call u2mess('F', 'ALGELINE3_91')
        lfeti=.true.
    else
        lfeti=.false.
    endif
    if (lfeti) then
        call jelira(chamn1(1:19)//'.FETC', 'LONMAX', nbsd, k8bid)
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
        call jeveuo(chamn1(1:19)//'.FETC', 'L', ifetc1)
        call jeveuo(chamn2(1:19)//'.FETC', 'L', ifetc2)
    else
        nbsd=0
    endif
!
! --- BOUCLE SUR LES SOUS-DOMAINES CF ASSMAM OU VTCMBL PAR EXEMPLE
    do 20 idd = 0, nbsd
        iddok=.false.
        if (.not.lfeti) then
            iddok=.true.
        else
            if (zi(ilimpi+idd) .eq. 1) iddok=.true.
        endif
        if (iddok) then
            if (idd .eq. 0) then
                kval1=chamn1(1:19)//'.VALE'
                kval2=chamn2(1:19)//'.VALE'
            else
                kval1=zk24(ifetc1+idd-1)(1:19)//'.VALE'
                kval2=zk24(ifetc2+idd-1)(1:19)//'.VALE'
            endif
            call jeveuo(kval1, 'L', ival1)
            call jeveuo(kval2, 'E', ival2)
            call jelira(kval2, 'LONMAX', neq, k8bid)
            call daxpy(neq, alpha, zr(ival1), 1, zr(ival2),&
                       1)
        endif
20  end do
    call jedema()
end subroutine
