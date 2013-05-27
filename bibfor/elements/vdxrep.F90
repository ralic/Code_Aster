subroutine vdxrep(nomte, epais, xi)
    implicit none
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
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
!
! BUT : REMPLIR L'OBJET .DESR DANS LES ZONES 1090 ET 2000
!       POUR POUVOIR CALCULER LES MATRICES DE PASSAGE AVEC VDREPE
!      (COQUE_3D)
!
! ARGUMENTS :
!   NOMTE  IN : NOM TYPE_ELEMENT
!   XI     IN : GEOMETRIE DES NOEUDS DE L'ELEMENT
!
    include 'jeveux.h'
    include 'asterfort/jevete.h'
    include 'asterfort/vectan.h'
    include 'asterfort/vectgt.h'
    character(len=16) :: nomte
    integer :: nb1, nb2, npgsr, i, j, k, ind, intsr, lzi, lzr
    real(kind=8) :: xi(3, 9)
    real(kind=8) :: vecta(9, 2, 3), vectn(9, 3), vectg(2, 3), vectt(3, 3)
    real(kind=8) :: epais, zero, vectpt(9, 2, 3)
!
    zero = 0.0d0
!
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
    call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
    nb1  =zi(lzi-1+1)
    nb2  =zi(lzi-1+2)
    npgsr=zi(lzi-1+3)
!
!     -- POUR REMPLIR LZR+1090+...  ET CALCULER VECTN :
    call vectan(nb1, nb2, xi, zr(lzr), vecta,&
                vectn, vectpt)
!
!     -- POUR REMPLIR LZR+2000+... :
!     -- QUELLE VALEUR POUR IND ? FICHE ???
    ind =0
    k = 0
    do 110 intsr = 1, npgsr
        call vectgt(ind, nb1, xi, zero, intsr,&
                    zr(lzr), epais, vectn, vectg, vectt)
        do 120 j = 1, 3
            do 130 i = 1, 3
                k = k + 1
                zr(lzr+2000+k-1) = vectt(i,j)
130          continue
120      continue
110  end do
end subroutine
