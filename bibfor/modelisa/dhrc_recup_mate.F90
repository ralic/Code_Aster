subroutine dhrc_recup_mate(imate, compor, h, a0, b0, c0,&
                 aa_t, ga_t, ab, gb, ac,&
                 gc, aa_c, ga_c, cstseu)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: sebastien.fayolle at edf.fr
!
    implicit none
#include "jeveux.h"
!
#include "asterfort/r8inir.h"
#include "asterfort/rcvala.h"
#include "asterfort/u2mesk.h"
    character(len=16) :: compor
    integer :: imate
    real(kind=8) :: a0(6, 6), b0(6, 2), c0(2, 2, 2), h
    real(kind=8) :: aa_t(6, 6, 2), ab(6, 2, 2), ac(2, 2, 2)
    real(kind=8) :: ga_t(6, 6, 2), gb(6, 2, 2), gc(2, 2, 2)
    real(kind=8) :: aa_c(6, 6, 2)
    real(kind=8) :: ga_c(6, 6, 2)
    real(kind=8) :: cstseu(2)
! ----------------------------------------------------------------------
!
! BUT : LECTURE DES PARAMETRES MATERIAU POUR LE MODELE DHRC
!
!
! IN:
!       IMATE   : ADRESSE DU MATERIAU
!       COMPOR  : COMPORTMENT
!       EP      : EPAISSEUR DE LA PLAQUE
! OUT:
!       A0      : PARAMETRE D ELASTICITE
!       B0      : PARAMETRE D ELASTICITE
!       C0      : PARAMETRE D ELASTICITE
!       AA_T    : PARAMETRE ALPHA POUR LE TENSEUR A EN TRACTION
!       AA_C    : PARAMETRE ALPHA POUR LE TENSEUR A EN COMPRESSION
!       AB      : PARAMETRE ALPHA POUR LE TENSEUR B
!       AC      : PARAMETRE ALPHA POUR LE TENSEUR C
!       GA_T    : PARAMETRE GAMMA POUR LE TENSEUR A EN TRACTION
!       GA_C    : PARAMETRE GAMMA POUR LE TENSEUR A EN COMPRESSION
!       GB      : PARAMETRE GAMMA POUR LE TENSEUR B
!       GC      : PARAMETRE GAMMA POUR LE TENSEUR C
!
!       CSTSEU  : PARAMETRES DE SEUILS
!            (1): POUR L'ENDOMMAGEMENT
!            (2): POUR LE GLISSEMENT
! ----------------------------------------------------------------------
!
    integer :: icodre(24), i, j, l
    real(kind=8) :: valres(24)
    character(len=8) :: nomres(24)
!
    if ((.not.( compor(1:4) .eq. 'DHRC'))) then
        call u2mesk('F', 'ELEMENTS4_65', 1, compor)
    endif
!
    call r8inir(24, 0.0d0, valres, 1)
!
!    h=0.2d0
!
!     -----------------------------------------------------------------
!     MATRICE A0(6,6)
!     -----------------------------------------------------------------
    nomres(1) = 'A011'
    nomres(2) = 'A012'
    nomres(3) = 'A013'
    nomres(4) = 'A014'
    nomres(5) = 'A015'
    nomres(6) = 'A016'
    nomres(7) = 'A022'
    nomres(8) = 'A023'
    nomres(9) = 'A024'
    nomres(10) = 'A025'
    nomres(11) = 'A026'
    nomres(12) = 'A033'
    nomres(13) = 'A034'
    nomres(14) = 'A035'
    nomres(15) = 'A036'
    nomres(16) = 'A044'
    nomres(17) = 'A045'
    nomres(18) = 'A046'
    nomres(19) = 'A055'
    nomres(20) = 'A056'
    nomres(21) = 'A066'
!
    call rcvala(imate, ' ', 'DHRC_SEUILS', 0, '',&
                0.0d0, 21, nomres, valres, icodre,&
                1)
!
    l=0
    do i = 1, 6
        do j = i, 6
            l=l+1
            a0(j,i)=valres(l)
        end do
    end do
!
    do i = 1, 6
        do j = i, 6
            a0(i,j)=a0(j,i)
        end do
    end do
!
!     -----------------------------------------------------------------
!     MATRICE AA_C(6,6,1)
!     -----------------------------------------------------------------
!
    nomres(1) = 'AAC111'
    nomres(2) = 'AAC121'
    nomres(3) = 'AAC131'
    nomres(4) = 'AAC141'
    nomres(5) = 'AAC151'
    nomres(6) = 'AAC161'
    nomres(7) = 'AAC221'
    nomres(8) = 'AAC231'
    nomres(9) = 'AAC241'
    nomres(10) = 'AAC251'
    nomres(11) = 'AAC261'
    nomres(12) = 'AAC331'
    nomres(13) = 'AAC341'
    nomres(14) = 'AAC351'
    nomres(15) = 'AAC361'
    nomres(16) = 'AAC441'
    nomres(17) = 'AAC451'
    nomres(18) = 'AAC461'
    nomres(19) = 'AAC551'
    nomres(20) = 'AAC561'
    nomres(21) = 'AAC661'
!
    call rcvala(imate, ' ', 'DHRC', 0, '',&
                0.0d0, 21, nomres, valres, icodre,&
                1)
!
    l=0
    do i = 1, 6
        do j = i, 6
            l=l+1
            aa_c(j,i,1)=valres(l)
        end do
    end do
!
    do i = 1, 6
        do j = i, 6
            aa_c(i,j,1)=aa_c(j,i,1)
        end do
    end do
!
!     -----------------------------------------------------------------
!     MATRICE AA_C(6,6,1)
!     -----------------------------------------------------------------
!
    nomres(1) = 'AAC112'
    nomres(2) = 'AAC122'
    nomres(3) = 'AAC132'
    nomres(4) = 'AAC142'
    nomres(5) = 'AAC152'
    nomres(6) = 'AAC162'
    nomres(7) = 'AAC222'
    nomres(8) = 'AAC232'
    nomres(9) = 'AAC242'
    nomres(10) = 'AAC252'
    nomres(11) = 'AAC262'
    nomres(12) = 'AAC332'
    nomres(13) = 'AAC342'
    nomres(14) = 'AAC352'
    nomres(15) = 'AAC362'
    nomres(16) = 'AAC442'
    nomres(17) = 'AAC452'
    nomres(18) = 'AAC462'
    nomres(19) = 'AAC552'
    nomres(20) = 'AAC562'
    nomres(21) = 'AAC662'
!
    call rcvala(imate, ' ', 'DHRC', 0, '',&
                0.0d0, 21, nomres, valres, icodre,&
                1)
!
    l=0
    do i = 1, 6
        do j = i, 6
            l=l+1
            aa_c(j,i,2)=valres(l)
        end do
    end do
!
    do i = 1, 6
        do j = i, 6
            aa_c(i,j,2)=aa_c(j,i,2)
        end do
    end do
!
!     -----------------------------------------------------------------
!     MATRICE AA_T(6,6,1)
!     -----------------------------------------------------------------
!
    nomres(1) = 'AAT111'
    nomres(2) = 'AAT121'
    nomres(3) = 'AAT131'
    nomres(4) = 'AAT141'
    nomres(5) = 'AAT151'
    nomres(6) = 'AAT161'
    nomres(7) = 'AAT221'
    nomres(8) = 'AAT231'
    nomres(9) = 'AAT241'
    nomres(10) = 'AAT251'
    nomres(11) = 'AAT261'
    nomres(12) = 'AAT331'
    nomres(13) = 'AAT341'
    nomres(14) = 'AAT351'
    nomres(15) = 'AAT361'
    nomres(16) = 'AAT441'
    nomres(17) = 'AAT451'
    nomres(18) = 'AAT461'
    nomres(19) = 'AAT551'
    nomres(20) = 'AAT561'
    nomres(21) = 'AAT661'
!
    call rcvala(imate, ' ', 'DHRC', 0, '',&
                0.0d0, 21, nomres, valres, icodre,&
                1)
!
    l=0
    do i = 1, 6
        do j = i, 6
            l=l+1
            aa_t(j,i,1)=valres(l)
        end do
    end do
!
    do i = 1, 6
        do j = i, 6
            aa_t(i,j,1)=aa_t(j,i,1)
        end do
    end do
!
!     -----------------------------------------------------------------
!     MATRICE AA_T(6,6,1)
!     -----------------------------------------------------------------
!
    nomres(1) = 'AAT112'
    nomres(2) = 'AAT122'
    nomres(3) = 'AAT132'
    nomres(4) = 'AAT142'
    nomres(5) = 'AAT152'
    nomres(6) = 'AAT162'
    nomres(7) = 'AAT222'
    nomres(8) = 'AAT232'
    nomres(9) = 'AAT242'
    nomres(10) = 'AAT252'
    nomres(11) = 'AAT262'
    nomres(12) = 'AAT332'
    nomres(13) = 'AAT342'
    nomres(14) = 'AAT352'
    nomres(15) = 'AAT362'
    nomres(16) = 'AAT442'
    nomres(17) = 'AAT452'
    nomres(18) = 'AAT462'
    nomres(19) = 'AAT552'
    nomres(20) = 'AAT562'
    nomres(21) = 'AAT662'
!
    call rcvala(imate, ' ', 'DHRC', 0, '',&
                0.0d0, 21, nomres, valres, icodre,&
                1)
!
    l=0
    do i = 1, 6
        do j = i, 6
            l=l+1
            aa_t(j,i,2)=valres(l)
        end do
    end do
!
    do i = 1, 6
        do j = i, 6
            aa_t(i,j,2)=aa_t(j,i,2)
        end do
    end do
!
!     -----------------------------------------------------------------
!     MATRICE GA_C(6,6,1)
!     -----------------------------------------------------------------
!
    nomres(1) = 'GAC111'
    nomres(2) = 'GAC121'
    nomres(3) = 'GAC131'
    nomres(4) = 'GAC141'
    nomres(5) = 'GAC151'
    nomres(6) = 'GAC161'
    nomres(7) = 'GAC221'
    nomres(8) = 'GAC231'
    nomres(9) = 'GAC241'
    nomres(10) = 'GAC251'
    nomres(11) = 'GAC261'
    nomres(12) = 'GAC331'
    nomres(13) = 'GAC341'
    nomres(14) = 'GAC351'
    nomres(15) = 'GAC361'
    nomres(16) = 'GAC441'
    nomres(17) = 'GAC451'
    nomres(18) = 'GAC461'
    nomres(19) = 'GAC551'
    nomres(20) = 'GAC561'
    nomres(21) = 'GAC661'
!
    call rcvala(imate, ' ', 'DHRC', 0, '',&
                0.0d0, 21, nomres, valres, icodre,&
                1)
!
    l=0
    do i = 1, 6
        do j = i, 6
            l=l+1
            ga_c(j,i,1)=valres(l)
        end do
    end do
!
    do i = 1, 6
        do j = i, 6
            ga_c(i,j,1)=ga_c(j,i,1)
        end do
    end do
!
!     -----------------------------------------------------------------
!     MATRICE GA_C(6,6,1)
!     -----------------------------------------------------------------
!
    nomres(1) = 'GAC112'
    nomres(2) = 'GAC122'
    nomres(3) = 'GAC132'
    nomres(4) = 'GAC142'
    nomres(5) = 'GAC152'
    nomres(6) = 'GAC162'
    nomres(7) = 'GAC222'
    nomres(8) = 'GAC232'
    nomres(9) = 'GAC242'
    nomres(10) = 'GAC252'
    nomres(11) = 'GAC262'
    nomres(12) = 'GAC332'
    nomres(13) = 'GAC342'
    nomres(14) = 'GAC352'
    nomres(15) = 'GAC362'
    nomres(16) = 'GAC442'
    nomres(17) = 'GAC452'
    nomres(18) = 'GAC462'
    nomres(19) = 'GAC552'
    nomres(20) = 'GAC562'
    nomres(21) = 'GAC662'
!
    call rcvala(imate, ' ', 'DHRC', 0, '',&
                0.0d0, 21, nomres, valres, icodre,&
                1)
!
    l=0
    do i = 1, 6
        do j = i, 6
            l=l+1
            ga_c(j,i,2)=valres(l)
        end do
    end do
!
    do i = 1, 6
        do j = i, 6
            ga_c(i,j,2)=ga_c(j,i,2)
        end do
    end do
!
!     -----------------------------------------------------------------
!     MATRICE GA_T(6,6,1)
!     -----------------------------------------------------------------
!
    nomres(1) = 'GAT111'
    nomres(2) = 'GAT121'
    nomres(3) = 'GAT131'
    nomres(4) = 'GAT141'
    nomres(5) = 'GAT151'
    nomres(6) = 'GAT161'
    nomres(7) = 'GAT221'
    nomres(8) = 'GAT231'
    nomres(9) = 'GAT241'
    nomres(10) = 'GAT251'
    nomres(11) = 'GAT261'
    nomres(12) = 'GAT331'
    nomres(13) = 'GAT341'
    nomres(14) = 'GAT351'
    nomres(15) = 'GAT361'
    nomres(16) = 'GAT441'
    nomres(17) = 'GAT451'
    nomres(18) = 'GAT461'
    nomres(19) = 'GAT551'
    nomres(20) = 'GAT561'
    nomres(21) = 'GAT661'
!
    call rcvala(imate, ' ', 'DHRC', 0, '',&
                0.0d0, 21, nomres, valres, icodre,&
                1)
!
    l=0
    do i = 1, 6
        do j = i, 6
            l=l+1
            ga_t(j,i,1)=valres(l)
        end do
    end do
!
    do i = 1, 6
        do j = i, 6
            ga_t(i,j,1)=ga_t(j,i,1)
        end do
    end do
!
!     -----------------------------------------------------------------
!     MATRICE GA_T(6,6,1)
!     -----------------------------------------------------------------
!
    nomres(1) = 'GAT112'
    nomres(2) = 'GAT122'
    nomres(3) = 'GAT132'
    nomres(4) = 'GAT142'
    nomres(5) = 'GAT152'
    nomres(6) = 'GAT162'
    nomres(7) = 'GAT222'
    nomres(8) = 'GAT232'
    nomres(9) = 'GAT242'
    nomres(10) = 'GAT252'
    nomres(11) = 'GAT262'
    nomres(12) = 'GAT332'
    nomres(13) = 'GAT342'
    nomres(14) = 'GAT352'
    nomres(15) = 'GAT362'
    nomres(16) = 'GAT442'
    nomres(17) = 'GAT452'
    nomres(18) = 'GAT462'
    nomres(19) = 'GAT552'
    nomres(20) = 'GAT562'
    nomres(21) = 'GAT662'
!
    call rcvala(imate, ' ', 'DHRC', 0, '',&
                0.0d0, 21, nomres, valres, icodre,&
                1)
!
    l=0
    do i = 1, 6
        do j = i, 6
            l=l+1
            ga_t(j,i,2)=valres(l)
        end do
    end do
!
    do i = 1, 6
        do j = i, 6
            ga_t(i,j,2)=ga_t(j,i,2)
        end do
    end do
!
!     -----------------------------------------------------------------
!     MATRICE B0(6,2)
!     -----------------------------------------------------------------
!
    b0(1,1)=1.0d6
    b0(1,2)=1.0d6
    b0(2,1)=1.0d6
    b0(2,2)=1.0d6
    b0(3,1)=1.0d6
    b0(3,2)=1.0d6
    b0(4,1)=1.0d6
    b0(4,2)=1.0d6
    b0(5,1)=1.0d6
    b0(5,2)=1.0d6
    b0(6,1)=1.0d6
    b0(6,2)=1.0d6
!
!     -----------------------------------------------------------------
!     MATRICE AB(6,2,2)
!     -----------------------------------------------------------------
!
    nomres(1) = 'AB111'
    nomres(2) = 'AB211'
    nomres(3) = 'AB311'
    nomres(4) = 'AB411'
    nomres(5) = 'AB511'
    nomres(6) = 'AB611'
    nomres(7) = 'AB121'
    nomres(8) = 'AB221'
    nomres(9) = 'AB321'
    nomres(10) = 'AB421'
    nomres(11) = 'AB521'
    nomres(12) = 'AB621'
    nomres(13) = 'AB112'
    nomres(14) = 'AB212'
    nomres(15) = 'AB312'
    nomres(16) = 'AB412'
    nomres(17) = 'AB512'
    nomres(18) = 'AB612'
    nomres(19) = 'AB122'
    nomres(20) = 'AB222'
    nomres(21) = 'AB322'
    nomres(22) = 'AB422'
    nomres(23) = 'AB522'
    nomres(24) = 'AB622'
!
    call rcvala(imate, ' ', 'DHRC', 0, '',&
                0.0d0, 24, nomres, ab, icodre,&
                1)
!
!     -----------------------------------------------------------------
!     MATRICE GB(6,2,2)
!     -----------------------------------------------------------------
!
    nomres(1) = 'GB111'
    nomres(2) = 'GB211'
    nomres(3) = 'GB311'
    nomres(4) = 'GB411'
    nomres(5) = 'GB511'
    nomres(6) = 'GB611'
    nomres(7) = 'GB121'
    nomres(8) = 'GB221'
    nomres(9) = 'GB321'
    nomres(10) = 'GB421'
    nomres(11) = 'GB521'
    nomres(12) = 'GB621'
    nomres(13) = 'GB112'
    nomres(14) = 'GB212'
    nomres(15) = 'GB312'
    nomres(16) = 'GB412'
    nomres(17) = 'GB512'
    nomres(18) = 'GB612'
    nomres(19) = 'GB122'
    nomres(20) = 'GB222'
    nomres(21) = 'GB322'
    nomres(22) = 'GB422'
    nomres(23) = 'GB522'
    nomres(24) = 'GB622'
!
    call rcvala(imate, ' ', 'DHRC', 0, '',&
                0.0d0, 24, nomres, gb, icodre,&
                1)
!
!     -----------------------------------------------------------------
!     MATRICE C0(2,2,2)
!     -----------------------------------------------------------------
!
    nomres(1) = 'C0111'
    nomres(2) = 'C0211'
    nomres(3) = 'C0121'
    nomres(4) = 'C0221'
    nomres(5) = 'C0112'
    nomres(6) = 'C0212'
    nomres(7) = 'C0122'
    nomres(8) = 'C0222'
!
    call rcvala(imate, ' ', 'DHRC', 0, '',&
                0.0d0, 8, nomres, c0, icodre,&
                1)
!
!     -----------------------------------------------------------------
!     MATRICE AC(2,2,2)
!     -----------------------------------------------------------------
!
    nomres(1) = 'AC111'
    nomres(2) = 'AC211'
    nomres(3) = 'AC121'
    nomres(4) = 'AC221'
    nomres(5) = 'AC112'
    nomres(6) = 'AC212'
    nomres(7) = 'AC122'
    nomres(8) = 'AC222'
!
    call rcvala(imate, ' ', 'DHRC', 0, '',&
                0.0d0, 8, nomres, ac, icodre,&
                1)
!
!     -----------------------------------------------------------------
!     MATRICE GC(2,2,2)
!     -----------------------------------------------------------------
!
    nomres(1) = 'GC111'
    nomres(2) = 'GC211'
    nomres(3) = 'GC121'
    nomres(4) = 'GC221'
    nomres(5) = 'GC112'
    nomres(6) = 'GC212'
    nomres(7) = 'GC122'
    nomres(8) = 'GC222'
!
    call rcvala(imate, ' ', 'DHRC', 0, '',&
                0.0d0, 8, nomres, gc, icodre,&
                1)
!
!     -----------------------------------------------------------------
!     SEUILS CSTSEU
!     -----------------------------------------------------------------
!
    nomres(1) = 'SYD'
    nomres(2) = 'SCRIT'
    nomres(3) = 'K0MICR'
!
    call rcvala(imate, ' ', 'DHRC_SEUILS', 0, '',&
                0.0d0, 3, nomres, valres, icodre,&
                1)
!
    cstseu(1)=valres(1)**2*valres(3)*h
!
    cstseu(2)=valres(2)*h
!
end subroutine
