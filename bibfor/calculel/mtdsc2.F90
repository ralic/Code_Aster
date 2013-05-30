subroutine mtdsc2(matas, objet, eoul, adress)
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
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jeveuo.h'
    character(len=*) :: matas, objet, eoul
    integer :: adress
!
! -----------------------------------------------------------
!   BUT : RECUPERER L'ADRESSE D'UN OBJET D'UNE MATR_ASSE
!
!   MATAS K19  IN/JXIN : NOM DE LA MATR_ASSE
!   OBJET K4   IN      : NOM D'UN SUFFIXE : ABLO,ADIA,...
!   EOUL  K1   IN      : 'E' : EN ECRITURE
!                        'L' : EN LECTURE
!   ADRESS I   OUT     : ADRESSE DANS ZI, ZR, ... DE L'OBJET
!
!
!  ATTENTION : CETTE ROUTINE NE FAIT PAS JEMARQ/JEDEMA
!              POUR NE PAS INVALIDER L'ADRESSE "OUT"
! -----------------------------------------------------------
    character(len=19) :: mat
    character(len=14) :: nu
    character(len=4) :: obj
    integer :: i1, i2, jrefa
!
    mat=matas
    obj=objet
    call assert(obj(3:4).eq.'BL' .or. obj(3:4).eq.'DI' .or. obj(3:4).eq.'HC')
!
    call jeveuo(mat//'.REFA', eoul, jrefa)
    nu=zk24(jrefa-1+2)
!
    call jeexin(nu//'.SMOS.SMDI', i1)
    call jeexin(nu//'.SLCS.SCDI', i2)
!       SI LDLT, LES 2 OBJETS PEUVENT EXISTER
    call assert(i1.gt.0 .or. i2.gt.0)
!
    if (obj(2:2) .eq. 'X') then
!           -- ON PRIVILEGIE LE STOCKAGE MORSE :
        if (i1 .gt. 0) then
            obj='SM'//obj(3:4)
            call assert(obj.ne.'SMBL')
            call jeveuo(nu//'.SMOS.'//obj, eoul, adress)
        else
            call assert(.false.)
!            -- LE STOCK. LIGNE_CIEL N'EXISTE QU'AVEC UN STOCK. MORSE
            obj='SC'//obj(3:4)
            call jeveuo(nu//'.SLCS.'//obj, eoul, adress)
        endif
!
    else if (obj(2:2).eq.'M') then
        call assert(i1.gt.0)
        call assert(obj.ne.'SMBL')
        call jeveuo(nu//'.SMOS.'//obj, eoul, adress)
!
    else if (obj(2:2).eq.'C') then
        call assert(i2.gt.0)
        call jeveuo(nu//'.SLCS.'//obj, eoul, adress)
!
    else
        call assert(.false.)
    endif
!
end subroutine
