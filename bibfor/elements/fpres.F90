subroutine fpres(nomte, xi, nb1, vecl, vectpt)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/forsrg.h'
    include 'asterfort/jevech.h'
    include 'asterfort/jevete.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/utpvlg.h'
    include 'asterfort/vectci.h'
    include 'asterfort/vexpan.h'
    integer :: nb1
    character(len=16) :: nomte
    real(kind=8) :: vecl(51), vectpt(9, 3, 3)
!
!
    real(kind=8) :: rnormc, f1, chg(6), kijkm1(40, 2), pgl(3, 3)
    real(kind=8) :: xi(3, *), vecl1(42), chgsrg(6, 8), chgsrl(6)
    integer :: lzi, nb2, npgsn, lzr, jpres, j, i, jp, ip, intsn, i1, k
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
    nb1  =zi(lzi-1+1)
    nb2  =zi(lzi-1+2)
    npgsn=zi(lzi-1+4)
!
    call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
!
    call r8inir(42, 0.d0, vecl1, 1)
!
! --- CAS DES CHARGEMENTS DE PRESSION : Z LOCAL
!
    call jevech('PPRESSR', 'L', jpres)
    do 70 j = 1, nb1
        do 30 i = 1, 6
            chgsrl(i)=0.d0
30      continue
!-----------------------------------------------------
!  LE SIGNE MOINS CORRESPOND A LA CONVENTION :
!      UNE PRESSION POSITIVE PROVOQUE UN GONFLEMENT
        chgsrl(3)= - zr(jpres-1+j)
!-----------------------------------------------------
        do 50 jp = 1, 3
            do 40 ip = 1, 3
                pgl(jp,ip)=vectpt(j,jp,ip)
40          continue
50      continue
        call utpvlg(1, 6, pgl, chgsrl, chg)
        do 60 i = 1, 6
            chgsrg(i,j)=chg(i)
60      continue
70  continue
!
    do 200 intsn = 1, npgsn
        call vectci(intsn, nb1, xi, zr(lzr), rnormc)
!
        call forsrg(intsn, nb1, nb2, zr(lzr), chgsrg,&
                    rnormc, vectpt, vecl1)
200  end do
!
!     RESTITUTION DE KIJKM1 POUR CONDENSER LES FORCES
!     ATTENTION LA ROUTINE N'EST PAS UTILISEE DANS LE CAS DES
!     EFFORTS SUIVANTS (MOMENTS SURFACIQUES)
!
    i1=5*nb1
    do 220 j = 1, 2
        do 210 i = 1, i1
            k=(j-1)*i1+i
            kijkm1(i,j)=zr(lzr-1+1000+k)
210      continue
220  end do
!
    do 240 i = 1, i1
        f1=0.d0
        do 230 k = 1, 2
            f1=f1+kijkm1(i,k)*vecl1(i1+k)
230      continue
        vecl1(i)=vecl1(i)-f1
240  end do
!
!     EXPANSION DU VECTEUR VECL1 : DUE A L'AJOUT DE LA ROTATION FICTIVE
!
    call vexpan(nb1, vecl1, vecl)
    do 90 i = 1, 3
        vecl(6*nb1+i)=0.d0
90  end do
!
end subroutine
