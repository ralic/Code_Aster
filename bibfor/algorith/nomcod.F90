subroutine nomcod(nom, num, ic, nc)
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
!***********************************************************************
!    G. JAQUART     DATE 26/05/93
!-----------------------------------------------------------------------
!  BUT:  CONCATENATION D'UN CARACTERE ET D'UN ENTIER
!-----------------------------------------------------------------------
    implicit none
!
!  NOM : CARACTERE A CONCATENER AVEC UN ENTIER
!  NUM : ENTIER A CONCATENER
!  IC  : INDICE DU PREMIER CARACTERE COMPRENANT L'ENTIER
!  NC  : INDICE DU DERNIER CARACTERE
!
!  EXEMPLE D'UTILISATION
!
!  NOMNOE='NO'
!  CALL NOMCOD(NOMNOE,IND,3,8)
!
!
    include 'asterfort/assert.h'
    character(len=*) :: nom
    character(len=4) :: format
    integer :: num, ic, nc
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    format='(IX)'
    do 10 i = ic, nc
        nom(i:i)=' '
10  end do
!
    do 20 i = 1, nc-ic+1
        if (num .ge. 10**(i-1) .and. num .lt. 10**i) then
            write (format(3:3),'(I1)') i
            goto 21
        endif
20  end do
!
    call assert(.false.)
!
21  continue
    write (nom(ic:ic+i-1),format) num
!
end subroutine
