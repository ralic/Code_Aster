subroutine mamagi(nomte, xr, yr)
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
#include "asterfort/mgauss.h"
    character(len=8) :: nomte
    real(kind=8) :: xr(*), yr(*), det
    real(kind=8) :: psi3(3), bt(12, 27), btb(12, 12)
    real(kind=8) :: atild(3), btild(3), ctild(3)
    real(kind=8) :: coefa(4), coefb(4), coefc(4), coefd(4)
!
!-----------------------------------------------------------------------
    integer :: i, i1, i2, ifon, ig, intef, integ
    integer :: intsn, iret, j, jf, k, kp, l
    integer :: npge, npgsn, nso
    real(kind=8) :: bij, xi1, xi2, xi3
!-----------------------------------------------------------------------
    npge = 3
!
!--- ON PREND LES 3 POINTS D'INTEGRATION DANS L'EPAISSEUR. METHODE DE
!--- NEWTON-COTES
!
    psi3(1)=-1.d0
    psi3(2)= 0.d0
    psi3(3)= 1.d0
!
!-- COEFFICIENTS DES POLYNOMES D'ORDRE 2 D'INTERPOLATION DU TYPE
!     AXI3*XI3 + B*XI3 + C
!
    atild(1)= 0.5d0
    btild(1)=-0.5d0
    ctild(1)= 0.d0
!
    atild(2)=-1.d0
    btild(2)= 0.d0
    ctild(2)= 1.d0
!
    atild(3)= 0.5d0
    btild(3)= 0.5d0
    ctild(3)= 0.d0
!
!     LES 4 OU 3 FONCTIONS LINEAIRES AUX NOEUDS SOMMETS SONT DU TYPE
!     A + B*XI1 + C*XI2 + D*XI1*XI2
!
    if (nomte(1:8) .eq. 'MEC3QU9H' .or. nomte(1:8) .eq. 'MEGRC3Q9') then
!
        nso =4
        npgsn=9
!
        coefa(1)= 0.25d0
        coefb(1)=-0.25d0
        coefc(1)=-0.25d0
        coefd(1)= 0.25d0
!
        coefa(2)= 0.25d0
        coefb(2)= 0.25d0
        coefc(2)=-0.25d0
        coefd(2)=-0.25d0
!
        coefa(3)= 0.25d0
        coefb(3)= 0.25d0
        coefc(3)= 0.25d0
        coefd(3)= 0.25d0
!
        coefa(4)= 0.25d0
        coefb(4)=-0.25d0
        coefc(4)= 0.25d0
        coefd(4)=-0.25d0
!
        elseif ( nomte(1:8) .eq. 'MEC3TR7H' .or. nomte(1:8) .eq.&
    'MEGRC3T7' ) then
!
        nso =3
        npgsn=7
!
        coefa(1)= 1.d0
        coefb(1)=-1.d0
        coefc(1)=-1.d0
        coefd(1)= 0.d0
!
        coefa(2)= 0.d0
        coefb(2)= 1.d0
        coefc(2)= 0.d0
        coefd(2)= 0.d0
!
        coefa(3)= 0.d0
        coefb(3)= 0.d0
        coefc(3)= 1.d0
        coefd(3)= 0.d0
!
    endif
!
!     CREATION DE LA MATRICE BT(NPGE*NSO,NPGE*NPGSN)
!
    ig=0
    do 350 integ = 1, npge
        xi3=psi3(integ)
        do 360 intsn = 1, npgsn
            i1=108+intsn
            i2=108+9+intsn
            xi1=xr(i1)
            xi2=xr(i2)
            ig=ig+1
            do 370 intef = 1, npge
                do 380 ifon = 1, nso
                    jf=nso*(intef-1)+ifon
                    bij= (atild(intef)*xi3**2+btild(intef)*xi3+ctild(&
                    intef))* (coefa(ifon)+coefb(ifon)*xi1+coefc(ifon)*&
                    xi2 +coefd(ifon)*xi1*xi2)
                    bt(jf,ig)=bij
380              continue
370          continue
360      continue
350  continue
!
    do 385 i = 1, npge*nso
        do 390 j = 1, npge*nso
            btb(i,j)=0.d0
            do 400 k = 1, npge*npgsn
                btb(i,j)=btb(i,j)+bt(i,k)*bt(j,k)
400          continue
390      continue
385  continue
!
!     MATRICE DE PASSAGE = (BT*B*B)-1*BT
!
    call mgauss('NFVP', btb, bt, 12, npge*nso,&
                npge*npgsn, det, iret)
!
    do 410 i = 1, npge*nso
        l = npge*npgsn*(i-1)
        do 420 kp = 1, npge*npgsn
            yr(l+kp) = bt(i,kp)
420     continue
410  continue
!
end subroutine
