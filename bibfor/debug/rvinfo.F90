subroutine rvinfo(ifm, iocc, i1, i2, c,&
                  sdchef)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    integer :: ifm, iocc, i1, i2
    character(len=1) :: c
    character(len=16) :: sdchef
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     AFFICHAGE INFO SUR LE POST COURRANT
!     ------------------------------------------------------------------
! IN  SDCHEF : K : SD DES CHAMPS EFFECTIF
! IN  C      : K : INDICATEUR D' ERREUR
! IN  IOCC   : I : INDICE OCCURENCE
! IN  I1,I2  : I : REPERAGE CHAMPS
!     ------------------------------------------------------------------
!
!
    integer :: adrval, vali, adracc
    real(kind=8) :: valr
    character(len=8) :: acces
    character(len=24) :: nomval, nomacc
!
!=======================================================================
!
    call jemarq()
    nomval = sdchef//'.VALACCE'
    nomacc = sdchef//'.TYPACCE'
    if (c .eq. 'R') then
        call jeveuo(jexnum(nomacc, iocc), 'L', adracc)
        call jeveuo(jexnum(nomval, iocc), 'L', adrval)
        acces = zk8(adracc + i1-1)
    else
        call jeveuo(nomval, 'L', adrval)
        call jeveuo(nomacc, 'L', adracc)
        acces = zk8(adracc)
    endif
!
    write(ifm,*)
    write(ifm,*)'--- POST_TRAITEMENT NUMERO : ',iocc,&
     &            ' - CHAMP NUMERO           : ',i2
    if ((acces(1:1).eq.'O') .or. (acces(1:1).eq.'M')) then
        vali = zi(adrval + i1-1)
        if (acces(1:1) .eq. 'O') then
            write(ifm,*)' NUME_ORDRE           : ',vali
        else
            write(ifm,*)' NUME_MODE            : ',vali
        endif
    else if ((acces(1:1).eq.'F') .or. (acces(1:1).eq.'I')) then
        valr = zr(adrval + i1-1)
        if (acces(1:1) .eq. 'I') then
            write(ifm,*)' INSTANT                : ',valr
        else
            write(ifm,*)' FREQUENCE              : ',valr
        endif
    else
    endif
!
    if (c .eq. 'E') write(ifm,*)' CHAMP INEXISTANT '
!
    call jedema()
end subroutine
