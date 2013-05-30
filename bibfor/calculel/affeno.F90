subroutine affeno(ioc, ino, nocmp, nbcmp, ncmpgd,&
                  ncmpmx, val, kval, desc, valglo,&
                  kvalgl, type, nec)
    implicit  none
    include 'asterc/indik8.h'
    include 'asterfort/u2mesg.h'
    integer :: ino, nbcmp, ncmpmx, nec, desc(*), ioc
    real(kind=8) :: valglo(*), val(*)
    character(len=*) :: type
    character(len=8) :: nocmp(*), kvalgl(*), ncmpgd(*), kval(*)
    character(len=24) :: valk
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!
!    ROUTINE UTILISEE PAR OP0052 (OPERATEUR AFFE_CHAM_NO)
!
!    CETTE ROUTINE AFFECTE LES COMPOSANTES NOCMP DU NOEUD INO AUX
!    VALEURS VAL DANS LE TABLEAU VALGLO
!    ELLE CALCULE EGALEMENT LE DESCRIPTEUR GRANDEUR DESC DU NOEUD INO
!
! IN  : IOC    : NUMERO DE L'OCCURENCE DE AFFE DANS LA COMMANDE
! IN  : INO    : NUMERO DU NOEUD
! IN  : NOCMP  : LISTE DES COMPOSANTES A AFFECTER
! IN  : NBCMP  : NOMBRE DE COMPOSANTES A AFFECTER
! IN  : NCMPGD : LISTE GLOBALE DES COMPOSANTES DE LA GRANDEUR
! IN  : NCMPMX : NOMBRE DE COMPOSANTES DE LA GRANDEUR
! IN  : VAL    : VALEURS DES COMPOSANTES A AFFECTER
! IN  : KVAL   : VALEURS DES COMPOSANTES A AFFECTER EN K*8
! OUT : DESC   : DESCRIPTEUR GRANDEUR DU NOEUD
! OUT : VALGLO : TABLEAU DES VALEURS DES COMPOSANTES DE LA GRANDEUR
!
!-----------------------------------------------------------------------
!
    integer :: nocoaf, icmp, j, iec, jj, ind, ior
    integer :: vali(3)
!
    nocoaf = 0
    do 10 icmp = 1, nbcmp
        j = indik8 ( ncmpgd, nocmp(icmp), 1, ncmpmx )
        if (j .ne. 0) then
            iec = ( j - 1 ) / 30 + 1
            jj = j - 30 * ( iec - 1 )
            desc((ino-1)*nec+iec) = ior(desc((ino-1)*nec+iec),2**jj)
        else
            valk = nocmp(icmp)
            call u2mesg('F', 'CALCULEL5_65', 1, valk, 0,&
                        0, 0, 0.d0)
        endif
        nocoaf = nocoaf + 1
        ind = j + (ino-1)*ncmpmx
        if (type(1:1) .eq. 'R') then
            valglo(ind) = val(icmp)
        else
            kvalgl(ind) = kval(icmp)
        endif
10  end do
!
    if (nocoaf .ne. nbcmp) then
        vali (1) = ioc
        vali (2) = nocoaf
        vali (3) = nbcmp
        call u2mesg('F', 'CALCULEL5_66', 0, ' ', 3,&
                    vali, 0, 0.d0)
    endif
!
end subroutine
