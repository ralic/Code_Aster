subroutine moinip(nch, ncoef, iich, iisuiv, ilig,&
                  ilig2)
! aslint: disable=W1304
    implicit none
    integer :: iich(*), iisuiv(*), ilig(*)
    integer :: nch, ncoef, ii2, j, ii1
    integer(kind=4) :: ilig2(1)
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     DESIMBRICATION DES CHAINES DE LA STRUCTURE (IICH, IISUIV,ILIG) :
!     ------------------------------------------------------------------
! IN  NCH       DIMENSION DU TABLEAU IICH = NOMBRE DE CHAINES
! OUT NCOEF     LONGUEUR DE ILIG
! VAR IICH(K)   CF CI-DESSOUS
! VAR IISUIV(K) CF CI-DESSOUS
!       EN ENTREE IICH(J) EST L'ADRESSE DANS ILIG DU DEBUT DE
!                  LA CHAINE J . ILIG(IISUIV(K)) EST L'ELEMENT SUIVANT
!                  ILIG(K) DANS LA CHAINE A LAQUELLE ILS APPARTIENNENT.
!                  SI IICH(J) <= 0 LA CHAINE J EST VIDE .
!                  SI IISUIV(K) < 0 -IISUIV(K) EST LE NUMERO DE LA
!                  CHAINE A LAQUELLE APPARTIENT ILIG(K).
!       EN SORTIE IICH(J) EST L'ADRESSE DANS ILIG DE LA FIN DE
!                  LA CHAINE J ET POUR QUE ILIG(K) APPARTIENNE A LA
!                  CHAINE J IL FAUT ET IL SUFFIT QUE
!                  IICH(J-1) < K < IICH(J) + 1 .
! VAR ILIG(.)   TABLE DES ELEMENTS CHAINES
!     ------------------------------------------------------------------
    ii2 = 1
    do 120 j = 1, nch
        ii1 = iich(j)
        if (ii1 .le. 0) then
!            PREMIER MAILLON DE LA CHAINE VIDE
            iich(j) = ii2 - 1
        else
!            CHAINE NON VIDE    PREMIER MAILLON
            ilig2(ii2) = ilig(ii1)
            ii2 = ii2 + 1
            ii1 = iisuiv(ii1)
!
!             MAILLONS SUIVANTS DE LA CHAINE
!             TANT QUE II1 > 0 FAIRE :
110          continue
            if (ii1 .le. 0) then
!                 FIN DE LA CHAINE J
                iich(j) = ii2 - 1
                goto 120
            else
                ilig2(ii2) = ilig(ii1)
                ii2 = ii2 + 1
                ii1 = iisuiv(ii1)
                goto 110
            endif
        endif
!
120  end do
    ncoef = iich(nch)
end subroutine
