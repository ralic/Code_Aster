subroutine veassc(lischa, vecele)
!
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
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/copisd.h'
    include 'asterfort/corich.h'
    include 'asterfort/exisd.h'
    include 'asterfort/gcnco2.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/liscva.h'
    include 'asterfort/lisico.h'
    include 'asterfort/lislco.h'
    include 'asterfort/lisllc.h'
    include 'asterfort/lisnbg.h'
    include 'asterfort/lisnnb.h'
    include 'asterfort/reajre.h'
    character(len=19) :: lischa
    character(len=19) :: vecele
!
! ----------------------------------------------------------------------
!
! CALCUL DES VECTEURS ELEMENTAIRES DES CHARGEMENTS MECANIQUES
!
! VECT_ASSE_CHAR
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : SD LISTE DES CHARGES
! OUT VECELE : VECT_ELEM RESULTAT
!
!
!
!
    integer :: ichar, nbchar, ibid
    character(len=8) :: newnom
    character(len=19) :: chamno, lchout
    character(len=13) :: prefob
    integer :: codcha
    logical :: lveac
    integer :: nbveac, iexis
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    newnom = '.0000000'
!
! --- NOMBRE DE CHARGES
!
    call lisnnb(lischa, nbchar)
!
! --- NOMBRE DE CHARGES DE TYPE VECT_ASSE_CHAR
!
    nbveac = lisnbg(lischa,'VECT_ASSE_CHAR')
    if (nbveac .eq. 0) goto 99
!
! --- BOUCLE SUR LES CHARGES
!
    do 10 ichar = 1, nbchar
!
! ----- CODE DU GENRE DE LA CHARGE
!
        call lislco(lischa, ichar, codcha)
        lveac = lisico('VECT_ASSE_CHAR',codcha)
        if (lveac) then
!
! ------- NOM DU CHAM_NO
!
            call lisllc(lischa, ichar, prefob)
            call liscva(prefob, chamno)
!
! ------- NOM DU CHAMP
!
            call exisd('CHAMP_GD', chamno, iexis)
            call assert(iexis.gt.0)
!
! ------- ON RECOPIE SIMPLEMENT LE CHAMP DANS VECT_ELEM
!
            call gcnco2(newnom)
            lchout = '&&VEASSE.'//newnom(2:8)
            call corich('E', lchout, ichar, ibid)
            call copisd('CHAMP_GD', 'V', chamno, lchout)
            call reajre(vecele, lchout, 'V')
        endif
10  end do
!
99  continue
!
    call jedema()
end subroutine
