subroutine veripl(ma, nbma, linuma, ang, typerr)
    implicit none
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
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    character(len=*) :: ma
    integer :: nbma, jcoor, jconx, ima, k, iprem, numai1, numail, linuma(nbma)
    real(kind=8) :: n1(3), n(3), ab(3), ac(3), nn, n1n, n1n1
    real(kind=8) :: cos2b, cos2a, a(3), b(3), c(3), ang
    character(len=1) :: typerr
    character(len=8) :: ma2, nomail, nomai1
! -------------------------------------------------------
! BUT : VERIFIER LA PLANEITE D'UNE LISTE DE MAILLES
! -------------------------------------------------------
!  MA    IN   K8 : NOM DU MAILLAGE
!  NBMA  IN   I  : NOMBRE DE MAILLES DANS LINUMA
!  LINUMA IN  V(I): LISTE DES NUMEROS DES MAILLES DONT ON VEUT
!                    VERIFIER LA PLANEITE.
!  ANG   IN   R  : ANGLE (EN DEGRE) QUE NE DOIT PAS DEPASSER
!                  LA NORMALE DES FACETTES 2,3,... AVEC LA FACETTE 1
!  TYPERR IN  K1 : /'A' -> <A>LARME SI NECESSAIRE
!                  /'F' -> ERREUR <F>ATALE SI NECESSAIRE
!
! REMARQUES :
!   ON CALCULE TOUS LES ANGLES QUE FONT LES MAILLES 2,3,...,N
!   AVEC LA 1ERE. L'ANGLE MAX ENTRE 2 MAILLES QUELCONQUES
!   EST DONC INFERIEUR A 2*ANG
!
!   LA NORMALE A UNE FACETTE EST CALCULEE AVEC SES 3 1ER SOMMETS
!
!   LES MAILLES DE LINUMA DOIVENT ETRE "SURFACIQUES" 3D
!   (LE PROGRAMME NE VERIFIE PAS CETTE CONDITION)
! -------------------------------------------------------
    character(len=24) :: valk(2)
!
!
    call jemarq()
    ma2 = ma
!
    call jeveuo(ma2//'.COORDO    .VALE', 'L', jcoor)
!
    cos2b = cos(ang*r8dgrd())**2
!
    iprem = 0
    do 20,ima = 1,nbma
    numail = linuma(ima)
    call jeveuo(jexnum(ma2//'.CONNEX', numail), 'L', jconx)
!
!       -- VERIFIER NOMAIL : TRIA OU QUAD ???
!
!       CALCUL DE LA NORMALE (N) A LA FACETTE IMA :
    do 10,k = 1,3
    a(k) = zr(jcoor-1+3* (zi(jconx-1+1)-1)+k)
    b(k) = zr(jcoor-1+3* (zi(jconx-1+2)-1)+k)
    c(k) = zr(jcoor-1+3* (zi(jconx-1+3)-1)+k)
    ab(k) = b(k) - a(k)
    ac(k) = c(k) - a(k)
10  continue
    n(1) = ab(2)*ac(3) - ab(3)*ac(2)
    n(2) = ab(3)*ac(1) - ab(1)*ac(3)
    n(3) = ab(1)*ac(2) - ab(2)*ac(1)
!
!
    iprem = iprem + 1
    if (iprem .eq. 1) then
        n1(1) = n(1)
        n1(2) = n(2)
        n1(3) = n(3)
        numai1 = numail
        goto 20
    endif
!
    nn = n(1)*n(1) + n(2)*n(2) + n(3)*n(3)
    n1n = n1(1)*n(1) + n1(2)*n(2) + n1(3)*n(3)
    n1n1 = n1(1)*n1(1) + n1(2)*n1(2) + n1(3)*n1(3)
!
    cos2a = (n1n*n1n)/ (nn*n1n1)
!
    if (cos2a .lt. cos2b) then
        call jenuno(jexnum(ma2//'.NOMMAI', numail), nomail)
        call jenuno(jexnum(ma2//'.NOMMAI', numai1), nomai1)
        valk(1) = nomai1
        valk(2) = nomail
        call utmess(typerr, 'MODELISA7_80', nk=2, valk=valk)
    endif
!
    20 end do
!
    call jedema()
!
end subroutine
